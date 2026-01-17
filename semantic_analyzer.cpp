#include "semantic_analyzer.h"
#include "class_builder.h"
#include <iostream>

void SemanticAnalyzer::analyze(ProgramNode* root) {
    analyzeProgram(root);
}

void SemanticAnalyzer::analyzeProgram(ProgramNode* root) {
    if (!root) return;

    // 1. СНАЧАЛА создаем класс
    if (currentClass == nullptr) {
        constPool.addUtf8("Code");
        currentClass = new JvmClass("HaskellProgram", constPool);
        currentClass->classIdx = constPool.addClass("HaskellProgram");
        currentClass->superIdx = constPool.addClass("java/lang/Object");

        // 1. Создаем MethodRef на Object.<init> (инструкция invokespecial будет использовать это)
        // Это индекс #9 в вашем выводе
        int superInitRef = constPool.addMethodRef("java/lang/Object", "<init>", "()V");

        // 2. Создаем структуру метода для НАШЕГО класса
        JvmMethod initMethod("<init>", "()V", nullptr);
        initMethod.nameIdx = constPool.addUtf8("<init>");
        initMethod.descIdx = constPool.addUtf8("()V");
        initMethod.accessFlags = 0x0001; // ACC_PUBLIC (не static!)

        currentClass->methods.push_back(initMethod);
    }

    // 2. Сначала собираем ВСЕ сигнатуры (первый проход)
    // Это важно, чтобы функции видели друг друга
    for (auto* decl : root->decls) {
        if (decl && decl->type == DECL_FUNC_SIGN) {
            analyzeDecl(decl);
        }
    }

    // 3. Затем анализируем тела функций (второй проход)
    for (auto* decl : root->decls) {
        if (decl && (decl->type == DECL_FUNC || decl->type == DECL_VAR)) {
            analyzeDecl(decl);
        }
    }

    // 4. После анализа всех деклараций добавляем JVM Entry Point
    // Это public static void main(String[] args)

    // Регистрируем дескриптор
    std::string mainDesc = "([Ljava/lang/String;)V";

    // Регистрируем MethodRef для самой точки входа
    int javaMainRef = constPool.addMethodRef("HaskellProgram", "main", mainDesc);

    JvmMethod entryPoint("main", mainDesc, nullptr);
    entryPoint.nameIdx = constPool.addUtf8("main");
    entryPoint.descIdx = constPool.addUtf8(mainDesc);
    entryPoint.accessFlags = 0x0009; // ACC_PUBLIC | ACC_STATIC

    currentClass->methods.push_back(entryPoint);
    std::cout << "[JvmGen] Added synthetic JVM entry point 'main' -> calls 'haskellMain'\n";

    ClassBuilder builder("output/HaskellProgram.class", currentClass);
    bool res = builder.build();
}

void SemanticAnalyzer::analyzeDeclList(DeclListNode* list) {
    if (!list) return;
    for (auto decl : list->decls) {
        analyzeDecl(decl);
    }
}

void SemanticAnalyzer::analyzeDecl(DeclNode* node) {
    if (!node) return;

    // 1. Обработка переменных
    if (node->type == DECL_VAR) {
        if (node->expr) {
            analyzeExpr(node->expr);

            // 2. Регистрируем переменную
            LocalVariable varInfo;
            varInfo.index = nextLocalIndex++;
            varInfo.type = node->expr->inferredType;

            SemanticType* type = node->expr->inferredType;
            symbolTable[node->name] = LocalVariable(type, nextLocalIndex++);

            // 3. Атрибутируем узел
            if (dynamic_cast<ASTNode*>(node)) node->inferredType = type;

            std::cout << "[Semantic] Declared var '" << node->name
                << "' index=" << varInfo.index
                << " type=" << (varInfo.type ? varInfo.type->getDescriptor() : "?") << "\n";
        }
    }
    
    // 2. Обработка заголовков функций
    else if (node->type == DECL_FUNC_SIGN) {
        std::vector<SemanticType*> allTypes;
        collectTypes(node->typeExpr, allTypes);

        if (!allTypes.empty()) {
            FunctionSignature sig;
            sig.returnType = allTypes.back(); // Последний - всегда возвращаемое значение
            allTypes.pop_back();
            sig.paramTypes = allTypes;        // Все остальные - аргументы
            functionSignatures[node->name] = sig;

            std::cout << "[Semantic] Registered signature for '" << node->name 
                    << "' (" << sig.paramTypes.size() << " params)\n";
        }
    }

    // 3. Обработка тел функций
    else if (node->type == DECL_FUNC) {
        // 1. Ищем сигнатуру
        auto it = functionSignatures.find(node->name);
        if (it == functionSignatures.end()) {
            std::cerr << "[Error] No signature for function: " << node->name << "\n";
            return;
        }
        FunctionSignature& sig = it->second;
        std::string internalName = node->name;
        std::string descriptor = sig.getDescriptor();

        // Обработка main
        if (node->name == "main") {
            internalName = "haskellMain";
            descriptor = "()V"; 
            
            // Создаем MethodRef для Haskell-кода
            // Это позволит JVM найти этот метод внутри этого же класса (#3)
            int haskellMainRef = constPool.addMethodRef("HaskellProgram", internalName, descriptor);
            
            std::cout << "[JvmGen] MethodRef for haskellMain created at index: " << haskellMainRef << "\n";
        }

        // 2. Создаем JVM метод
        JvmMethod method(internalName, descriptor, node);
        
        // Регистрируем имя и дескриптор в Constant Pool
        method.nameIdx = constPool.addUtf8(internalName);
        method.descIdx = constPool.addUtf8(descriptor);

        // 3. Подготовка к анализу тела
        symbolTable.clear();
        int currentLocalIdx = 0;

        // 4. Анализ аргументов (Паттерны)
        if (node->paramsList) {
            auto* declList = dynamic_cast<DeclListNode*>(node->paramsList);
            for (size_t i = 0; i < declList->decls.size(); ++i) {
                if (i < sig.paramTypes.size()) {
                    analyzePattern(declList->decls[i]->expr, sig.paramTypes[i], currentLocalIdx);
                }
            }
        }

        // 5. Анализ тела функции
        if (node->expr) {
            analyzeExpr(node->expr);

            // Для пустых списков
            if (node->expr->type == EXPR_ARRAY && node->expr->block.empty()) {
                node->expr->inferredType = sig.returnType;
            }
        }

        // 6. ФИНАЛ: Сохраняем локальные переменные в структуру метода
        // (SymbolTable сейчас содержит все переменные этой функции)
        for (std::map<std::string, LocalVariable>::const_iterator it = symbolTable.begin(); it != symbolTable.end(); ++it) {
            // it->first — имя переменной (string)
            // it->second — объект LocalVariable
            method.locals.push_back(it->second);
        }
        
        if (currentClass) {
            currentClass->methods.push_back(method);
        }
        else {
            std::cerr << "[Critical Error] currentClass is NULL while analyzing " << node->name << "\n";
        }
        std::cout << "[JvmGen] Method '" << node->name << "' processed. Locals count: " 
                << method.locals.size() << "\n";
    }

    // 4. Обработка Let-блоков
    else if (node->type == DECL_BLOCK && node->name == "LET_BLOCK") {
        // В DeclNode список let-переменных лежит в letBlock (ASTNode*, кастим)
        if (node->letBlock) {
            auto* list = dynamic_cast<DeclListNode*>(node->letBlock);
            if (list) {
                for (auto* varDecl : list->decls) {
                    analyzeDecl(varDecl);
                }
            }
        }
    }

    else if (node->type == DECL_ACTION) {
        // У DeclNode есть поле expr (само выражение вызова функции)
        if (node->expr) {
            analyzeExpr(node->expr);
            // Тип действия совпадает с типом выражения
            if (dynamic_cast<ASTNode*>(node)) node->inferredType = node->expr->inferredType;
        }
    }
}

void SemanticAnalyzer::analyzeExpr(ExprNode* node) {
    if (!node) return;

    switch (node->type) {
    case EXPR_LITERAL: {
        if (node->value.find('.') != std::string::npos) {
            node->inferredType = SemanticType::Float();
            float val = std::stof(node->value);
            node->constPoolIndex = constPool.addFloat(val);
        }
        else if (isdigit(node->value[0]) || node->value[0] == '-') {
            node->inferredType = SemanticType::Int();
            int val = std::stoi(node->value);
            node->constPoolIndex = constPool.addInteger(val);
        }
        else {
            node->inferredType = SemanticType::String();
            node->constPoolIndex = constPool.addStringLiteral(node->value);
        }
        break;
    }

    case EXPR_PATTERN_VAR:
    case EXPR_VAR: {
        // 1. Проверка локальных переменных
        if (symbolTable.count(node->name)) {
            auto& info = symbolTable[node->name];
            node->inferredType = info.type;
            node->localVarIndex = info.index;
        } 
        // 2. Проверка глобальных функций
        else if (functionSignatures.count(node->name)) {
            FunctionSignature& sig = functionSignatures[node->name];
            
            // Создаем SemanticType::Function на основе сохраненной сигнатуры
            // Это позволит fold увидеть, что это функция (a -> b -> c)
            node->inferredType = SemanticType::Function(sig.paramTypes, sig.returnType);
            node->isFunctionRef = true;
        }
        // 3. Встроенные функции
        else if (builtinSignatures.count(node->name)) {
            node->isFunctionRef = true;
            node->isBuiltinFunciton = true;
            // Для встроенных пока оставим Unknown, они уточнятся в EXPR_FUNC_CALL
            node->inferredType = SemanticType::Unknown(); 
        }
        break;
    }

    case EXPR_BINARY: {
        analyzeExpr(node->left);
        analyzeExpr(node->right);

        SemanticType* lType = node->left->inferredType;
        SemanticType* rType = node->right->inferredType;

        if (!lType || !rType) return;

        // --- Оператор CONS (:) ---
        if (node->op == ":") {
            // Результат - всегда список типа головы
            node->inferredType = SemanticType::List(lType);
            // Тут можно добавить проверку: rType должен быть List(lType)
        }
        // --- Сравнения ---
        else if (node->op == "<=" || node->op == "==" || node->op == ">" || node->op == "<") {
            node->inferredType = SemanticType::Bool();

            // Авто-каст для примитивов (Int -> Float)
            if (lType->base == BaseType::FLOAT || rType->base == BaseType::FLOAT) {
                if (lType->base == BaseType::INT) {
                    node->left = createCastNode(node->left, SemanticType::Float());
                }
                if (rType->base == BaseType::INT) {
                    node->right = createCastNode(node->right, SemanticType::Float());
                }
            }
        }
        // --- Арифметика ---
        else {
            // Проверяем BaseType, так как теперь это структура
            bool leftIsFloat = (lType->kind == TypeKind::PRIMITIVE && lType->base == BaseType::FLOAT);
            bool rightIsFloat = (rType->kind == TypeKind::PRIMITIVE && rType->base == BaseType::FLOAT);

            if (leftIsFloat || rightIsFloat) {
                node->inferredType = SemanticType::Float();

                if (lType->base == BaseType::INT) {
                    node->left = createCastNode(node->left, SemanticType::Float());
                    std::cout << "[Semantic] Auto-cast LEFT to Float\n";
                }
                if (rType->base == BaseType::INT) {
                    node->right = createCastNode(node->right, SemanticType::Float());
                    std::cout << "[Semantic] Auto-cast RIGHT to Float\n";
                }
            }
            else {
                node->inferredType = SemanticType::Int();
            }
        }
        break;
    }

    case EXPR_TYPE_LIST:
        // Тип определяется динамически
        node->inferredType = SemanticType::List(SemanticType::Unknown());
        break;

    case EXPR_LIST:
    case EXPR_ARRAY: {
        SemanticType* elemType = SemanticType::Unknown();

        if (!node->block.empty()) {
            analyzeExpr(node->block[0]);
            elemType = node->block[0]->inferredType;
            for (auto* item : node->block) analyzeExpr(item);
        } 
        // Если список пустой [], а мы знаем, что функция должна вернуть [Float]
        // Нам нужен механизм передачи "ожидаемого типа" вниз по дереву.
        
        node->inferredType = SemanticType::List(elemType);
        break;
    }

    case EXPR_IF:
        analyzeExpr(node->cond);
        analyzeExpr(node->expr_true);
        analyzeExpr(node->expr_false);
        node->inferredType = node->expr_true->inferredType;
        break;

    case EXPR_FUNC_CALL: {
        std::vector<ExprNode*> allArgs;
        ExprNode* baseFuncNode = nullptr;
        flattenCall(node, allArgs, &baseFuncNode);

        if (!baseFuncNode) break;

        analyzeExpr(baseFuncNode);
        for (auto* arg : allArgs) analyzeExpr(arg);

        std::string name = baseFuncNode->name;
        if (name.find(' ') != std::string::npos) name = name.substr(0, name.find(' '));

        // 1. Пытаемся получить сигнатуру
        FunctionSignature sig;
        bool foundSig = false;
        bool isBuiltin = false; // Флаг, что функция из HaskellRuntime

        // СПЕЦИАЛЬНАЯ ЛОГИКА для head/tail (Динамическая сигнатура)
        if ((name == "head" || name == "tail" || name == "isNull") && !allArgs.empty()) {
        SemanticType* argType = allArgs[0]->inferredType;
        
        // Работаем только если аргумент - список
        if (argType && argType->kind == TypeKind::LIST) {
            sig.paramTypes = { argType };
            
            if (name == "head")      sig.returnType = argType->subType;
                else if (name == "tail") sig.returnType = argType;
                else if (name == "isNull") sig.returnType = SemanticType::Bool(); // Динамически [T] -> Bool
                
                foundSig = true;
                isBuiltin = true;
            }
        }

        // ОБЩАЯ ЛОГИКА (из словарей)
        else if (builtinSignatures.count(name)) {
            sig = builtinSignatures[name];
            foundSig = true;
            isBuiltin = true;
        } else if (functionSignatures.count(name)) {
            sig = functionSignatures[name];
            foundSig = true;
            isBuiltin = false;
        }

        // 2. Если сигнатура определена (динамически или статически)
        if (foundSig) {
            // Устанавливаем идентификатору тип функции
            baseFuncNode->inferredType = SemanticType::Function(sig.paramTypes, sig.returnType);
            baseFuncNode->isBuiltinFunciton = isBuiltin;

            std::string targetClass = isBuiltin ? "HaskellRuntime" : currentClass->className;
            node->constPoolIndex = constPool.addMethodRef(targetClass, name, sig.getDescriptor());

            // ТИПИЗАЦИЯ ЦЕПОЧКИ (чтобы не было ?)
            std::vector<ExprNode*> chain;
            ExprNode* curr = node;
            while (curr && curr->type == EXPR_FUNC_CALL) {
                chain.push_back(curr);
                curr = curr->function;
            }

            SemanticType* currentType = baseFuncNode->inferredType;
            for (int i = (int)chain.size() - 1; i >= 0; --i) {
                if (currentType->kind == TypeKind::FUNCTION && !currentType->paramTypes.empty()) {
                    if (currentType->paramTypes.size() > 1) {
                        std::vector<SemanticType*> rem(currentType->paramTypes.begin() + 1, currentType->paramTypes.end());
                        currentType = SemanticType::Function(rem, currentType->returnType);
                    } else {
                        currentType = currentType->returnType;
                    }
                }
                chain[i]->inferredType = currentType;
            }
        } 
        
        // Если это map/fold или другие сложные случаи 
        else if (name == "map") {
            if (allArgs.size() < 2) {
                // Если аргументов меньше 2, это частичное применение (пока Unknown или Function)
                node->inferredType = SemanticType::Unknown();
            } else {
                SemanticType* funcArg = allArgs[0]->inferredType;
                SemanticType* listArg = allArgs[1]->inferredType;

                if (funcArg->kind == TypeKind::FUNCTION) {
                    node->inferredType = SemanticType::List(funcArg->returnType);

                    // Проверка: тип элементов списка == типу аргумента функции
                    if (listArg->kind == TypeKind::LIST && !listArg->subType->equals(funcArg->paramTypes[0])) {
                        std::cout << "[Warning] map: list of " << listArg->subType->getDescriptor()
                                << " passed to function expecting " << funcArg->paramTypes[0]->getDescriptor() << "\n";
                    }
                }
            }
        }

        else if (name == "fold") {
            if (allArgs.size() < 3) {
                node->inferredType = SemanticType::Unknown();
            } else {
                SemanticType* folder = allArgs[0]->inferredType;
                SemanticType* initial = allArgs[1]->inferredType;
                SemanticType* list = allArgs[2]->inferredType;

                if (folder->kind == TypeKind::FUNCTION && folder->paramTypes.size() >= 2) {
                    node->inferredType = initial; // Результат fold всегда типа аккумулятора

                    // Проверка аккумулятора:
                    if (!folder->paramTypes[0]->equals(initial)) {
                        std::cout << "[Warning] fold: expected acc " << initial->getDescriptor() << "\n";
                    }
                }
            }
        }
        break;
    }

    case EXPR_DO_BLOCK: {
        if (node->decls) {
            for (auto* d : node->decls->decls) {
                analyzeDecl(d);
            }
        }
        node->inferredType = SemanticType::IO(); 
        break;
    }

    case EXPR_CASTING:
        // Cast node уже имеет явно заданный тип при создании
        // (см. createCastNode)
        break;

    default:
        if (node->left) analyzeExpr(node->left);
        if (node->right) analyzeExpr(node->right);
        break;
    }
}

void SemanticAnalyzer::analyzePattern(ExprNode* pattern, SemanticType* expectedType, int& localIdx) {
    if (!pattern || !expectedType) return;

    // Устанавливаем тип самому узлу паттерна (убираем ?)
    pattern->inferredType = expectedType;

    if (pattern->type == EXPR_PATTERN_VAR) {
        // Добавляем переменную в таблицу символов
        symbolTable[pattern->name] = LocalVariable(expectedType, localIdx);
        pattern->localVarIndex = localIdx++; // Инкрементируем ссылку
    }
    else if (pattern->type == EXPR_PATTERN_CONS) {
        if (expectedType->kind == TypeKind::LIST) {
            // x : xs -> x получает тип элемента, xs получает тип списка
            analyzePattern(pattern->left, expectedType->subType, localIdx);
            analyzePattern(pattern->right, expectedType, localIdx);
        }
    }
    else if (pattern->type == EXPR_PATTERN_LIST) {
        // Для пустого списка [] просто подтверждаем, что это список нужного типа
        // Новых переменных не создаем, localIdx не меняем
    }
}

void SemanticAnalyzer::collectTypes(ASTNode* node, std::vector<SemanticType*>& types) {
    if (!node) return;
    ExprNode* expr = dynamic_cast<ExprNode*>(node);
    if (!expr) return;

    if (expr->type == EXPR_TYPE_FUNCTION) {
        collectTypes(expr->left, types);
        collectTypes(expr->right, types);
    }
    else if (expr->type == EXPR_TYPE_PRIMITIVE) {
        if (expr->value == "Int") types.push_back(SemanticType::Int());
        else if (expr->value == "Float") types.push_back(SemanticType::Float());
        else if (expr->value == "Bool") types.push_back(SemanticType::Bool());
        else if (expr->value == "()" || expr->value == "Void") types.push_back(SemanticType::Void());
        else types.push_back(SemanticType::Unknown());
    }
    // Обработка конструкторов (IO, Maybe и т.д.)
    else if (expr->type == EXPR_TYPE_CONSTRUCTOR) {
        if (expr->name == "IO") {
            types.push_back(SemanticType::IO());
        } else {
            SemanticType* constr = new SemanticType(TypeKind::CONSTRUCTOR);
            constr->typeName = expr->name;
            types.push_back(constr);
        }
    }
    else if (expr->type == EXPR_TYPE_LIST) {
        std::vector<SemanticType*> inner;
        collectTypes(expr->right ? expr->right : expr->left, inner); 
        if (!inner.empty()) {
            types.push_back(SemanticType::List(inner.back()));
        } else {
            types.push_back(SemanticType::List(SemanticType::Unknown()));
        }
    }
}

// Создание узла кастинга. Оборачиваем старый узел в новый.
ExprNode* SemanticAnalyzer::createCastNode(ExprNode* target, SemanticType* toType) {
    ExprNode* castNode = new ExprNode(NodeType::EXPR_CASTING);
    castNode->left = target;
    castNode->inferredType = toType; // Просто копируем указатель
    castNode->isCastNode = true;
    return castNode;
}

void SemanticAnalyzer::initBuiltins() {
    builtinSignatures["isNull"] = { {}, SemanticType::Bool() };
    builtinSignatures["head"] = { {}, SemanticType::Unknown() };
    builtinSignatures["tail"] = { {}, SemanticType::Unknown() };


    // MAP

    SemanticType* anyA = SemanticType::Unknown();
    SemanticType* anyB = SemanticType::Unknown();
    SemanticType* mapFuncArg = SemanticType::Function({anyA}, anyB);
    builtinSignatures["map"] = { { mapFuncArg, SemanticType::List(anyA) }, SemanticType::List(anyB) };


    // FOLD

    SemanticType* anyAcc = SemanticType::Unknown();
    SemanticType* anyEl  = SemanticType::Unknown();

    // Первый аргумент: (acc -> el -> acc)
    SemanticType* foldFuncArg = SemanticType::Function({anyAcc, anyEl}, anyAcc);
    
    FunctionSignature foldSig;
    // Аргументы: функция, начальное значение (acc), список ([el])
    foldSig.paramTypes = { foldFuncArg, anyAcc, SemanticType::List(anyEl) };
    foldSig.returnType = anyAcc; // Результат — того же типа, что аккумулятор
    
    builtinSignatures["fold"] = foldSig;


    // IO функции
    builtinSignatures["putStrLn"] = { {SemanticType::String()}, SemanticType::IO() };
    builtinSignatures["print"] =    { {SemanticType::Unknown()}, SemanticType::IO() };

    builtinSignatures["readInt"]      = { {}, SemanticType::Int() };
    builtinSignatures["readFloat"]    = { {}, SemanticType::Float() };
    builtinSignatures["readString"]   = { {}, SemanticType::String() };
    builtinSignatures["readIntArray"] = { {}, SemanticType::List(SemanticType::Int()) };
}

void SemanticAnalyzer::flattenCall(ExprNode* node, std::vector<ExprNode*>& args, ExprNode** finalFunc) {
    if (node->type == EXPR_FUNC_CALL && node->function) {
        // Рекурсивно идем влево (вглубь вызова)
        flattenCall(dynamic_cast<ExprNode*>(node->function), args, finalFunc);
        // Собираем аргументы по пути назад
        for (auto* arg : node->arguments) {
            args.push_back(dynamic_cast<ExprNode*>(arg));
        }
    } else {
        // Мы дошли до самого корня — узла EXPR_VAR_REF (например, 'fold')
        *finalFunc = node;
    }
}


// PROCESSING

void SemanticAnalyzer::printDebugInfo() {
    std::cout << "\n============================================\n";
    std::cout << "           JVM CONSTANT POOL                \n";
    std::cout << "============================================\n";
    
    const auto& entries = constPool.getEntries();
    for (size_t i = 1; i < entries.size(); ++i) { // Индексы с 1
        const auto& e = entries[i];
        std::cout << "#" << i << " = ";
        
        switch (e.tag) {
            case CONSTANT_Utf8: 
                std::cout << "Utf8          \"" << e.stringValue << "\""; 
                break;
            case CONSTANT_Integer: 
                std::cout << "Integer       " << e.intValue; 
                break;
            case CONSTANT_Float: 
                std::cout << "Float         " << e.floatValue; 
                break;
            case CONSTANT_Class: 
                std::cout << "Class         #" << e.refIndex1; 
                break;
            case CONSTANT_String: 
                std::cout << "String        #" << e.refIndex1; 
                break;
            case CONSTANT_NameAndType: 
                std::cout << "NameAndType   #" << e.refIndex1 << ":#" << e.refIndex2; 
                break;
            case CONSTANT_Methodref: 
                std::cout << "MethodRef     #" << e.refIndex1 << ".#" << e.refIndex2; 
                break;
            default: 
                std::cout << "Unknown Tag (" << (int)e.tag << ")";
        }
        std::cout << "\n";
    }

    std::cout << "\n============================================\n";
    std::cout << "           CLASS METHODS                    \n";
    std::cout << "============================================\n";

    if (currentClass) {
        std::cout << "Class: " << currentClass->className << "\n";
        std::cout << "--------------------------------------------\n";
        for (const auto& m : currentClass->methods) {
            std::cout << "Method: " << m.name << "\n";
            std::cout << "  Descriptor: " << m.descriptor << "\n";
            std::cout << "  CP Indices: Name=#" << m.nameIdx << ", Desc=#" << m.descIdx << "\n";
            std::cout << "  Locals (" << m.locals.size() << "):\n";
            for (const auto& l : m.locals) {
                // Для красивого вывода нужно найти имя переменной. 
                // В SymbolTable мы хранили имя в ключе map, а LocalVariable хранит только индекс/тип.
                // В реальном коде можно добавить std::string name в LocalVariable для отладки.
                std::cout << "    Slot " << l.index << ": Type=" 
                          << (l.type ? l.type->toString() : "?") << "\n";
            }
            std::cout << "--------------------------------------------\n";
        }
    }
}