#include "semantic_analyzer.h"
#include <iostream>

void SemanticAnalyzer::analyze(ProgramNode* root) {
    if (!root) return;
    // В ProgramNode у тебя вектор decls
    for (auto decl : root->decls) {
        analyzeDecl(decl);
    }
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
        symbolTable.clear();
        int currentLocalIdx = 0; // Начинаем отсчет локальных переменных функции

        auto it = functionSignatures.find(node->name);
        if (it == functionSignatures.end()) return;
        FunctionSignature& sig = it->second;

        // 1. Анализируем паттерны аргументов
        if (node->paramsList) {
            auto* declList = dynamic_cast<DeclListNode*>(node->paramsList);
            for (size_t i = 0; i < declList->decls.size(); ++i) {
                if (i < sig.paramTypes.size()) {
                    // Теперь паттерны x, (y:ys) и [] получат типы из sig
                    analyzePattern(declList->decls[i]->expr, sig.paramTypes[i], currentLocalIdx);
                }
            }
        }

        // 2. Анализируем тело
        if (node->expr) {
            analyzeExpr(node->expr);

            if (node->expr->type == EXPR_ARRAY && node->expr->block.empty()) {
                node->expr->inferredType = sig.returnType;
            }
        }
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

        // СПЕЦИАЛЬНАЯ ЛОГИКА для head/tail (Динамическая сигнатура)
        if ((name == "head" || name == "tail") && !allArgs.empty()) {
            SemanticType* argType = allArgs[0]->inferredType;
            if (argType && argType->kind == TypeKind::LIST) {
                sig.paramTypes = { argType };
                sig.returnType = (name == "head") ? argType->subType : argType;
                foundSig = true;
            }
        } 
        // ОБЩАЯ ЛОГИКА (из словарей)
        else if (builtinSignatures.count(name)) {
            sig = builtinSignatures[name];
            foundSig = true;
        } else if (functionSignatures.count(name)) {
            sig = functionSignatures[name];
            foundSig = true;
        }

        // 2. Если сигнатура определена (динамически или статически)
        if (foundSig) {
            // Устанавливаем идентификатору тип функции
            baseFuncNode->inferredType = SemanticType::Function(sig.paramTypes, sig.returnType);
            
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
        else if (name == "null") {
            node->inferredType = SemanticType::Bool();
        }
        
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
    builtinSignatures["null"] = { {}, SemanticType::Bool() };
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
