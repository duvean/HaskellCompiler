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
            varInfo.type = node->expr->inferredType; // Тип берем из правой части
            
            symbolTable[node->name] = varInfo; // Добавляем в таблицу
            
            // 3. Атрибутируем узел
            node->localVarIndex = varInfo.index;
            node->inferredType = SemanticType::Void;
            
            std::cout << "[Semantic] Declared var '" << node->name 
                      << "' index=" << varInfo.index 
                      << " type=" << typeToString(varInfo.type) << "\n";
        }
    }
    // 2. Обработка функций
    else if (node->type == DECL_FUNC_SIGN) {
        // Обработка func :: Int -> Float
        FunctionSignature sig;
        std::vector<SemanticType> allTypes;
        collectTypes(node->typeExpr, allTypes);

        if (!allTypes.empty()) {
            sig.returnType = allTypes.back(); // Последний тип — возвращаемое значение
            allTypes.pop_back();              // Остальное — аргументы
            sig.paramTypes = allTypes;
            functionSignatures[node->name] = sig;
            
            std::cout << "[Semantic] Registered signature for '" << node->name << "'\n";
        }
    }

    else if (node->type == DECL_FUNC) {
        std::cout << "[Semantic] Entering function: " << node->name << "\n";
        symbolTable.clear();
        nextLocalIndex = 0; 

        if (functionSignatures.find(node->name) == functionSignatures.end()) {
            std::cerr << "[Error] Function '" << node->name << "' must have a signature!\n";
            return;
        }
        
        FunctionSignature& sig = functionSignatures[node->name];

        // --- НОВОЕ: Извлекаем имена из paramsList ---
        std::vector<std::string> paramNames;
        if (node->paramsList) {
            DeclListNode* pList = dynamic_cast<DeclListNode*>(node->paramsList);
            if (pList) {
                for (auto* paramDecl : pList->decls) {
                    // В вашем createParameter(ExprNode* patternNode) имя, 
                    // скорее всего, лежит в patternNode->name или самом paramDecl->name
                    // Предположим, что имя параметра сохранено в paramDecl->name
                    if (!paramDecl->name.empty()) {
                        paramNames.push_back(paramDecl->name);
                    }
                }
            }
        } else {
            // Если paramsList пуст, используем вектор строк (на случай, если парсер пишет туда)
            paramNames = node->params;
        }

        if (paramNames.size() != sig.paramTypes.size()) {
            std::cerr << "[Error] Parameter count mismatch in '" << node->name << "'. "
                    << "Expected " << sig.paramTypes.size() << ", but found " << paramNames.size() << "\n";
            return;
        }

        for (size_t i = 0; i < paramNames.size(); ++i) {
            LocalVariable var;
            var.index = nextLocalIndex++;
            var.type = sig.paramTypes[i];
            symbolTable[paramNames[i]] = var;
            
            std::cout << "[Semantic] Param '" << paramNames[i] 
                    << "' assigned to LocalVar #" << var.index 
                    << " (" << typeToString(var.type) << ")\n";
        }

        // 3. Анализируем whereBlock (если есть)
        if (node->whereBlock) {
            analyzeDeclList(dynamic_cast<DeclListNode*>(node->whereBlock));
        }

        // 4. Анализируем тело функции
        if (node->expr) {
            analyzeExpr(node->expr);
            
            // Проверка: совпадает ли тип выражения с объявленным в сигнатуре?
            if (node->expr->inferredType != sig.returnType) {
                 // Здесь можно вставить авто-кастинг для return, если нужно
                 std::cout << "[Warning] Return type mismatch in '" << node->name << "'\n";
            }
            node->inferredType = sig.returnType;
        }

        // 5. Constant Pool (Имя и Дескриптор)
        int nameIdx = constPool.addUtf8(node->name);
        
        // Генерируем дескриптор на основе сигнатуры: (IF)F
        std::string desc = "(";
        for (auto t : sig.paramTypes) desc += getJvmDescriptor(t);
        desc += ")" + getJvmDescriptor(sig.returnType);
        
        int descIdx = constPool.addUtf8(desc);
        node->constPoolIndex = nameIdx;
    }


    // 3. Обработка Let-блоков (если они приходят как отдельный DeclNode)
    else if (node->type == DECL_BLOCK) {
         // Если letBlock хранит внутри DeclListNode
         DeclListNode* list = dynamic_cast<DeclListNode*>(node->letBlock);
         if (list) analyzeDeclList(list);
    }
}

void SemanticAnalyzer::analyzeExpr(ExprNode* node) {
    if (!node) return;

    switch (node->type) {
        case EXPR_LITERAL: {
            // Пытаемся определить тип литерала (примитивно по наличию точки)
            if (node->value.find('.') != std::string::npos) {
                node->inferredType = SemanticType::Float;
                float val = std::stof(node->value);
                node->constPoolIndex = constPool.addFloat(val);
            } else if (isdigit(node->value[0]) || node->value[0] == '-') { // Упрощенно
                node->inferredType = SemanticType::Int;
                int val = std::stoi(node->value);
                node->constPoolIndex = constPool.addInteger(val);
            } else {
                // Строка
                node->inferredType = SemanticType::String;
                // Убираем кавычки если они есть в парсере, или храним как есть
                node->constPoolIndex = constPool.addStringLiteral(node->value);
            }
            break;
        }

        case EXPR_VAR: {
            // Ищем в таблице символов
            if (symbolTable.count(node->name)) {
                LocalVariable info = symbolTable[node->name];
                node->localVarIndex = info.index;
                node->inferredType = info.type;
            } else {
                std::cerr << "[Semantic Error] Undefined variable: " << node->name << "\n";
                node->inferredType = SemanticType::Unknown;
            }
            break;
        }

        case EXPR_BINARY: {
            analyzeExpr(node->left);
            analyzeExpr(node->right);

            SemanticType lType = node->left->inferredType;
            SemanticType rType = node->right->inferredType;

            // Логика неявного преобразования (Int -> Float)
            if (lType == SemanticType::Float || rType == SemanticType::Float) {
                node->inferredType = SemanticType::Float;
                
                if (lType == SemanticType::Int) {
                    node->left = createCastNode(node->left, SemanticType::Float);
                    std::cout << "[Semantic] Auto-cast LEFT to Float\n";
                }
                if (rType == SemanticType::Int) {
                    node->right = createCastNode(node->right, SemanticType::Float);
                    std::cout << "[Semantic] Auto-cast RIGHT to Float\n";
                }
            } 
            else if (lType == SemanticType::Int && rType == SemanticType::Int) {
                node->inferredType = SemanticType::Int;
            }
            else {
                node->inferredType = SemanticType::Unknown; // Для строк и прочего
            }
            break;
        }

        case EXPR_CASTING:
            break; // Уже обработан

        default:
            // Для остальных рекурсивно
            if (node->left) analyzeExpr(node->left);
            if (node->right) analyzeExpr(node->right);
            break;
    }
}

std::string SemanticAnalyzer::makeMethodDescriptor(DeclNode* funcNode) {
    // Пока упрощенно: считаем, что аргументов нет (нужно доработать парсинг аргументов)
    // И возвращаем тип, который мы вывели (inferredType)
    std::string retDesc = getJvmDescriptor(funcNode->inferredType); 
    return "()" + retDesc;
}

void SemanticAnalyzer::collectTypes(ASTNode* node, std::vector<SemanticType>& types) {
    if (!node) return;
    ExprNode* expr = dynamic_cast<ExprNode*>(node);
    if (!expr) return;

    if (expr->type == EXPR_TYPE_FUNCTION) {
        // Узел вида: ArgType -> ReturnType
        collectTypes(expr->left, types);  // Рекурсивно берем левую часть
        collectTypes(expr->right, types); // Рекурсивно берем правую часть
    } else if (expr->type == EXPR_TYPE_PRIMITIVE) {
        if (expr->value == "Int") types.push_back(SemanticType::Int);
        else if (expr->value == "Float") types.push_back(SemanticType::Float);
        // ... другие типы
    }
}

// Создание узла кастинга. Оборачиваем старый узел в новый.
ExprNode* SemanticAnalyzer::createCastNode(ExprNode* target, SemanticType toType) {
    ExprNode* castNode = new ExprNode(EXPR_CASTING);
    castNode->left = target; // Оборачиваемый узел кладем в left
    castNode->inferredType = toType;
    castNode->isCastNode = true;
    return castNode;
}