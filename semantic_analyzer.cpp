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
    // 2. Обработка заголовков функций
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

    // 3. Обработка функций
    else if (node->type == DECL_FUNC) {
        std::cout << "[Semantic] Entering function: " << node->name << "\n";
        
        // 1. ПОЛНАЯ ОЧИСТКА контекста перед каждым определением
        symbolTable.clear();
        nextLocalIndex = 0; 

        // 2. Получаем сигнатуру (обязательно)
        if (functionSignatures.find(node->name) == functionSignatures.end()) {
            std::cerr << "[Error] Function '" << node->name << "' must have a signature!\n";
            return;
        }
        FunctionSignature& sig = functionSignatures[node->name];

        // 3. СНАЧАЛА ГРЫЗЕМ ПАРАМЕТРЫ (РЕГИСТРИРУЕМ ПЕРЕМЕННЫЕ)
        std::vector<DeclNode*> parameters;
        if (node->paramsList) {
            parameters = dynamic_cast<DeclListNode*>(node->paramsList)->decls;
        }

        // Обрабатываем каждый параметр ДО анализа тела
        for (size_t i = 0; i < parameters.size(); ++i) {
            int baseIndex = nextLocalIndex++; // Основной индекс аргумента в JVM
            // Вызываем деконструкцию (x:xs) или регистрацию 'x'
            analyzePattern(parameters[i]->expr, sig.paramTypes[i], baseIndex);
        }

        // 4. ТЕПЕРЬ ОБРАБАТЫВАЕМ WHERE-БЛОК (если есть)
        // Переменные из where тоже должны быть в таблице до анализа тела
        if (node->whereBlock) {
            analyzeDeclList(dynamic_cast<DeclListNode*>(node->whereBlock));
        }

        // 5. И ТОЛЬКО ТЕПЕРЬ АНАЛИЗИРУЕМ ТЕЛО
        if (node->expr) {
            analyzeExpr(node->expr); // Здесь переменные 'x', 'y', 'ys' уже будут найдены!
            
            // Проверка возвращаемого типа
            if (node->expr->inferredType != sig.returnType && node->expr->inferredType != SemanticType::Unknown) {
                std::cout << "[Warning] Return type mismatch in '" << node->name << "'\n";
            }
        }

        // 6. РЕГИСТРАЦИЯ В CONSTANT POOL
        int nameIdx = constPool.addUtf8(node->name);
        std::string desc = "(";
        for (auto t : sig.paramTypes) desc += getJvmDescriptor(t);
        desc += ")" + getJvmDescriptor(sig.returnType);
        constPool.addUtf8(desc);
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

        case EXPR_VAR:
        case EXPR_PATTERN_VAR: {
            if (symbolTable.count(node->name)) {
                auto& info = symbolTable[node->name];
                node->inferredType = info.type;
                node->localVarIndex = info.index;
                
                std::cout << "[Semantic] Linked Ref '" << node->name 
                        << "' to LocVar #" << node->localVarIndex 
                        << " (" << typeToString(node->inferredType) << ")\n";
            } else {
                // Если это не переменная, возможно это имя функции?
                if (functionSignatures.count(node->name)) {
                    // Имена функций в JVM не имеют индекса локальной переменной
                    node->inferredType = SemanticType::Unknown; 
                } else {
                    std::cerr << "[Semantic Error] Undefined: " << node->name << "\n";
                }
            }
            break;
        }

        case EXPR_BINARY: {
            // 1. Анализируем поддеревья
            analyzeExpr(node->left);
            analyzeExpr(node->right);

            SemanticType lType = node->left->inferredType;
            SemanticType rType = node->right->inferredType;

            // 2. Специфические операторы (списки и сравнения)
            if (node->op == ":") {
                // Оператор cons всегда возвращает список
                node->inferredType = SemanticType::List;
                // Здесь можно добавить проверку: (lType == Int && rType == List)
            } 
            else if (node->op == "<=" || node->op == "==" || node->op == ">" || node->op == "<") {
                // Операторы сравнения всегда возвращают Bool
                node->inferredType = SemanticType::Bool;
                
                // Но внутри них всё равно может понадобиться кастинг (например, 1 <= 1.5)
                if (lType == SemanticType::Float || rType == SemanticType::Float) {
                    if (lType == SemanticType::Int) node->left = createCastNode(node->left, SemanticType::Float);
                    if (rType == SemanticType::Int) node->right = createCastNode(node->right, SemanticType::Float);
                }
            }
            // 3. Арифметические операторы (+, -, *, /)
            else {
                // Логика неявного преобразования (Int -> Float)
                if (lType == SemanticType::Float || rType == SemanticType::Float) {
                    node->inferredType = SemanticType::Float;
                    
                    if (lType == SemanticType::Int) {
                        node->left = createCastNode(node->left, SemanticType::Float);
                        std::cout << "[Semantic] Auto-cast LEFT to Float in '" << node->op << "'\n";
                    }
                    if (rType == SemanticType::Int) {
                        node->right = createCastNode(node->right, SemanticType::Float);
                        std::cout << "[Semantic] Auto-cast RIGHT to Float in '" << node->op << "'\n";
                    }
                } 
                else if (lType == SemanticType::Int && rType == SemanticType::Int) {
                    node->inferredType = SemanticType::Int;
                }
                else {
                    // Если типы не числовые и это не спец. оператор
                    node->inferredType = lType; // Или Unknown
                }
            }
            break;
        }

        case EXPR_TYPE_LIST:
            node->inferredType = SemanticType::List;
            break;

        case EXPR_LIST:
        case EXPR_ARRAY: // или EXPR_LIST
            // Если элементы хранятся в векторе 'block' или через 'left'
            for (auto* item : node->block) {
                analyzeExpr(item);
            }
            if (node->left) analyzeExpr(node->left);
            node->inferredType = SemanticType::List;
            break;

        case EXPR_IF:
            analyzeExpr(node->cond); // Должен быть Bool
            analyzeExpr(node->expr_true);
            analyzeExpr(node->expr_false);
            // Тип if-else равен типу его веток
            node->inferredType = node->expr_true->inferredType; 
            break;

        case EXPR_FUNC_CALL: {
            // 1. Анализируем саму функцию (поле function)
            // Это заполнит атрибуты во вложенных узлах (например, узел 42, 40)
            if (node->function) {
                analyzeExpr(node->function);
            }

            // 2. Анализируем все аргументы в векторе
            for (ExprNode* arg : node->arguments) {
                analyzeExpr(arg);
            }

            // 3. Также на всякий случай проверяем left/right, 
            // если парсер иногда кладет аргументы туда (судя по DOT, там может быть Argument)
            if (node->left) analyzeExpr(node->left);
            if (node->right) analyzeExpr(node->right);

            // 4. Очистка имени для поиска сигнатуры (берём первое слово)
            std::string pureName = node->name;
            size_t spacePos = pureName.find(' ');
            if (spacePos != std::string::npos) {
                pureName = pureName.substr(0, spacePos);
            }

            if (functionSignatures.count(pureName)) {
                node->inferredType = functionSignatures[pureName].returnType;
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

void SemanticAnalyzer::analyzePattern(ExprNode* pattern, SemanticType type, int sourceLocalIndex) {
    if (!pattern) return;

    if (pattern->type == EXPR_PATTERN_CONS) {
        // pattern->left — это 'y', pattern->right — это 'ys'
        if (pattern->left) {
            LocalVariable vHead;
            vHead.index = nextLocalIndex++;
            vHead.type = SemanticType::Int; // Предполагаем Int для [Int]
            symbolTable[pattern->left->name] = vHead;
            std::cout << "[Semantic] Pattern Var '" << pattern->left->name << "' -> #" << vHead.index << "\n";
        }
        if (pattern->right) {
            LocalVariable vTail;
            vTail.index = nextLocalIndex++;
            vTail.type = SemanticType::List;
            symbolTable[pattern->right->name] = vTail;
            std::cout << "[Semantic] Pattern Var '" << pattern->right->name << "' -> #" << vTail.index << "\n";
        }
    } 
    // Если это просто одиночная переменная 'x' в паттерне
    else if (pattern->type == EXPR_PATTERN_VAR || pattern->type == EXPR_VAR) {
        LocalVariable v;
        v.index = sourceLocalIndex; // Используем уже выделенный индекс
        v.type = type;
        symbolTable[pattern->name] = v;
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
    } 
    else if (expr->type == EXPR_TYPE_PRIMITIVE) {
        if (expr->value == "Int") types.push_back(SemanticType::Int);
        else if (expr->value == "Float") types.push_back(SemanticType::Float);
        // ... другие типы
    }
    else if (expr->type == EXPR_TYPE_LIST) {
        // Если узел типа выглядит как [Int], то внутри него есть под-узел с типом Int.
        // Но для сигнатуры нам просто нужно знать, что это "Список".
        // Если вы хотите поддерживать [Int] vs [Float], нужно усложнить SemanticType
        // до структуры, хранящей subtype.
        
        // Для текущей задачи просто регистрируем как List
        types.push_back(SemanticType::List);        
    }
}

// Создание узла кастинга. Оборачиваем старый узел в новый.
ExprNode* SemanticAnalyzer::createCastNode(ExprNode* target, SemanticType toType) {
    ExprNode* castNode = new ExprNode(NodeType::EXPR_CASTING);
    castNode->left = target; // Оборачиваемый узел кладем в left
    castNode->inferredType = toType;
    castNode->isCastNode = true;
    return castNode;
}