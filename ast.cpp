#include <iostream>
#include "ast.h"

size_t ASTNode::nextNodeId = 0;


/* --- HELPER_FUNCTIONS --- */


std::string getPatternName(ExprNode* patternNode) {
    if (patternNode) {
        return patternNode->name;
    }
    return "NULL_PATTERN";
}

std::string nodeTypeToString(NodeType type) {
    switch (type) {
        // --- Главные узлы и объявления (Declarations) ---
        case NODE_PROGRAM:              return "PROGRAM";
        case DECL_VAR:                  return "VAR_DECL";
        case DECL_FUNC:                 return "FUNC_DECL";
        case DECL_FUNC_SIGN:            return "FUNC_SIGNATURE";
        case DECL_DATA:                 return "DATA_DECL";
        case DECL_TYPE:                 return "TYPE_ALIAS_DECL";
        case DECL_CONSTRUCTOR:          return "DATA_CONSTRUCTOR";
        case DECL_LIST:                 return "DECL_LIST";
        case DECL_BLOCK:                return "DECL_BLOCK"; // Блок объявлений (напр., 'where' или 'let')
        
        // --- Выражения (Expressions) ---
        case EXPR_LITERAL:              return "EXPR_LITERAL";
        case EXPR_VAR:                  return "EXPR_VAR_REF";
        case EXPR_BINARY:               return "EXPR_BINARY_OP";
        case EXPR_FUNC_CALL:            return "EXPR_FUNC_CALL";
        case EXPR_IF:                   return "EXPR_IF_THEN_ELSE";
        case EXPR_ARRAY:                return "EXPR_ARRAY";
        case EXPR_TUPLE:                return "EXPR_TUPLE";
        case EXPR_LAMBDA:               return "EXPR_LAMBDA";
        case EXPR_CASE:                 return "EXPR_CASE_OF";
        case EXPR_LETIN:                return "EXPR_LET_IN";
        case EXPR_DO:                   return "EXPR_DO_BLOCK";
        case EXPR_RETURN:               return "EXPR_RETURN";
        case EXPR_LIST:                 return "EXPR_LIST";
        case EXPR_LET_IN:               return "EXPR_LET_IN";
        
        // --- Паттерны (Patterns) ---
        case EXPR_PATTERN_VAR:          return "PATT_VAR";
        case EXPR_PATTERN_LITERAL:      return "PATT_LITERAL";
        case EXPR_PATTERN_CONSTRUCTOR:  return "PATT_CONSTRUCTOR";
        case EXPR_PATTERN_TUPLE:        return "PATT_TUPLE";
        case EXPR_PATTERN_LIST:         return "PATT_LIST";
        case EXPR_PATTERN_CONS:         return "PATT_CONS"; // (a:b)
        
        // --- Типовые выражения (Type Expressions) ---
        case EXPR_TYPE_PRIMITIVE:       return "TYPE_PRIMITIVE"; // Int, Bool
        case EXPR_TYPE_CONSTRUCTOR:     return "TYPE_CONSTRUCTOR"; // List, Maybe
        case EXPR_TYPE_VAR:             return "TYPE_VAR"; // a, b
        case EXPR_TYPE_FUNCTION:        return "TYPE_FUNCTION"; // A -> B
        case EXPR_TYPE_ANNOTATION:      return "TYPE_ANNOTATION"; // expr :: type
        case EXPR_TYPE_LIST:            return "TYPE_LIST"; // [type_expr]
        
        // --- Ветки Case/Alternative ---
        case EXPR_CASE_BRANCH:          return "CASE_BRANCH";
        case EXPR_CASE_BRANCH_LIST:     return "CASE_BRANCH_LIST";

        default:                        return "UNKNOWN_NODE_TYPE";
    }
}

std::string getDeclName(DeclNode* node) {
    if (!node) {
        return "NULL";
    }

    // 1. Имя, явно присвоенное узлу (для func_signature)
    if (!node->name.empty()) {
        return node->name; 
    }

    // 2. Имя извлечено из связанного выражения/паттерна (для параметров/связываний)
    // Это должно сработать для {VAR_DECL} a и b.
    if (node->expr) {
        // Требуется, чтобы ExprNode::getName() возвращал "a", "b", "x" и т.д.
        std::string exprName = node->expr->name; 
        if (!exprName.empty()) {
            return exprName;
        }
        return "name_empty_somehow";
    }

    // 3. Узел-контейнер/блок без имени
    // Для лучшей читаемости используйте более информативный тип, 
    // например, NODE_LET_BLOCK вместо DECL_VAR, если он используется для блока where/let.
    return "{" + nodeTypeToString(node->type) + "}";
}


/* --- AST_NODE --- */


std::string ASTNode::getDotLabel() const {
    // Базовый класс просто отображает тип
    return nodeTypeToString(type);
}

std::string ASTNode::toDotString() const {
    // Базовый класс просто объявляет себя, но не имеет детей
    std::stringstream ss;
    ss << "    " << nodeId << " [label=\"" << getDotLabel() << "\"];\n";
    return ss.str();
}


/* --- PROGRAM --- */


ProgramNode* ProgramNode::create(DeclListNode* declList) {
    ProgramNode* node = new ProgramNode(); 
    
    if (declList) {
        node->decls = declList->decls; 
        delete declList; 
    }
    
    std::cout << "ProgramNode created with " << node->decls.size() 
              << " top-level declarations.\n";
    return node;
}

std::string ProgramNode::toDotString() const {
    std::stringstream ss;
    
    ss << "    node [shape=box];\n";
    ss << "    " << nodeId << " [label=\"" << getDotLabel() << "\"];\n";
    
    std::cerr << "--- DEBUG: ProgramNode::toDotString STARTED ---\n";

    for (size_t i = 0; i < decls.size(); ++i) {
        DeclNode* decl = decls[i];
        if (decl) {
            ss << "    " << nodeId << " -> " << decl->nodeId 
               << " [label=\"Decl " << i + 1 << "\"];\n";
            
            std::cerr << "--- DEBUG: Attempting to process declaration " << i << " (ID: " << decl->nodeId << ") ---\n";
            ss << decl->toDotString(); 
            std::cerr << "--- DEBUG: Successfully processed declaration " << i << " ---\n";
        }
    }
    std::cerr << "--- DEBUG: ProgramNode::toDotString ENDED ---\n";
    return(ss.str());
}

std::string ProgramNode::getDotLabel() const {
    return("Program (ROOT)");
}


/* --- DECL_NODE --- */


DeclNode* DeclNode::createVarDecl(const std::string& name, ExprNode* expr) {
    DeclNode* node = new DeclNode(NodeType::DECL_VAR); 
    
    node->name = name;
    node->expr = expr;
    
    std::cout << "createVarDecl(" << name << ") -> Expression: " 
              << (expr ? "present" : "NULL") << "\n";
    return node;
}

DeclNode* DeclNode::createFuncDef(const std::string& name, 
                                  ASTNode* params, 
                                  ExprNode* body, 
                                  ASTNode* whereBlock) 
{
    DeclNode* node = new DeclNode(NodeType::DECL_FUNC);
    
    node->name = name;
    node->expr = body; // правая часть '='
    node->paramsList = params; 
    node->whereBlock = whereBlock;
    
    // --- Отладочный вывод ---
    
    size_t paramCount = 0;
    DeclListNode* paramList = static_cast<DeclListNode*>(params);
    if (paramList) {
        paramCount = paramList->decls.size();
    }
    
    std::cout << "createFuncDef(" << name << ") -> "
              << "Params: " << paramCount
              << ", Body: " << (body ? "present" : "NULL") 
              << ", Where: " << (whereBlock ? "present" : "NULL") << "\n";
              
    return node;
}

DeclNode* DeclNode::createFuncSignature(const std::string& name, ASTNode* typeExpr) {
    DeclNode* node = new DeclNode(NodeType::DECL_FUNC_SIGN); 
    
    node->name = name;
    node->typeExpr = typeExpr;
    
    std::cout << "createFuncSignature(" << name << ") -> Type: " 
              << (typeExpr ? "present" : "NULL") << "\n";
    return node;
}

DeclNode* DeclNode::createDataDecl(const std::string& name, ASTNode* constructors) {
    DeclNode* node = new DeclNode(NodeType::DECL_DATA);
    
    node->name = name;
    node->enumData = constructors;
    
    std::cout << "createDataDecl(" << name << ") -> Constructors: " 
              << (constructors ? "present" : "NULL") << "\n";
    return node;
}

DeclNode* DeclNode::createTypeDecl(const std::string& name, ASTNode* typeExpr) {
    DeclNode* node = new DeclNode(NodeType::DECL_TYPE);
    
    node->name = name;
    node->typeExpr = typeExpr; // Выражение, описывающее тип (правая часть '=')
    
    std::cout << "createTypeDecl(" << name << ") -> Type: " 
              << (typeExpr ? "present" : "NULL") << "\n";
    return node;
}

DeclNode* DeclNode::createLetBlock(DeclListNode* declList) {
    DeclNode* node = new DeclNode(NodeType::DECL_BLOCK); 
    
    node->letBlock = declList;
    node->name = "LET_BLOCK";

    size_t size = declList ? declList->decls.size() : 0;
    std::cout << "createLetBlock: created node containing list of size " 
              << size << "\n";
    return node;
}

DeclNode* DeclNode::createParameter(ExprNode* patternNode) {
    // Параметр — это объявление, где имя может быть извлечено из паттерна.
    DeclNode* node = new DeclNode(NodeType::DECL_VAR);
    
    // Храним паттерн в поле expr.
    node->expr = patternNode;
    
    // NOTE: Если паттерн является простым ID, мы можем извлечь имя для node->name:
    // if (patternNode && patternNode->type == NodeType::EXPR_PATTERN_VAR) {
    //     node->name = patternNode->name;
    // }
    
    std::cout << "createParameter: created DeclNode for parameter.\n";
    return node;
}

std::string DeclNode::getDotLabel() const {
    std::stringstream ss;
    ss << nodeTypeToString(type); // DECL_VAR, DECL_FUNC и т.д.
    if (!name.empty()) {
        ss << "\\nName: " << name;
    }
    // Дополнительные детали можно добавить сюда, если нужно
    return ss.str();
}

std::string DeclNode::toDotString() const {
    std::stringstream ss;
    ss << "    " << nodeId << " [label=\"" << getDotLabel() << "\"];\n";
    
    if (paramsList) {
        ss << "    " << nodeId << " -> " << paramsList->nodeId << " [label=\"Parameters\"];\n";
        ss << paramsList->toDotString();
    }

    if (expr) {
        ss << "    " << nodeId << " -> " << expr->nodeId << " [label=\"Expr\"];\n";
        std::cerr << "DEBUG DeclNode " << nodeId << ": Starting expr->toDotString()...\n";
        ss << expr->toDotString();
        std::cerr << "DEBUG DeclNode " << nodeId << ": Finished expr->toDotString().\n";
    }

    if (typeExpr) {
        ss << "    " << nodeId << " -> " << typeExpr->nodeId << " [label=\"Type/Params\"];\n";
        ss << typeExpr->toDotString();
    }
    
    if (letBlock) {
        ss << "    " << nodeId << " -> " << letBlock->nodeId << " [label=\"Let Block\"];\n";
        ss << letBlock->toDotString();
    }

    if (whereBlock) {
        ss << "    " << nodeId << " -> " << whereBlock->nodeId << " [label=\"Where Block\"];\n";
        ss << whereBlock->toDotString();
    }

    if (enumData) {
        ss << "    " << nodeId << " -> " << enumData->nodeId << " [label=\"Data Constr (Enum)\"];\n";
        ss << enumData->toDotString();
    }


    
    return ss.str();
}


/* --- DECL_LIST_NODE --- */


DeclListNode* DeclListNode::create(DeclNode* first) {
    DeclListNode* list = new DeclListNode();
    std::string declName = getDeclName(first); 

    if (first) {
        list->decls.push_back(first);
    }
    
    std::cout << "createDeclList. Contains: [" << declName << "]. Size: 1\n";
    return list;
}

DeclListNode* DeclListNode::addDecl(DeclListNode* list, DeclNode* new_decl) {
    if (list && new_decl) {
        list->decls.push_back(new_decl);
    }
    
    // Получаем полное содержимое списка ПОСЛЕ добавления
    std::string content = list ? list->listToString() : "NULL";
    size_t currentSize = list ? list->decls.size() : 0;
    
    std::cout << "addDeclToList. Added: " << getDeclName(new_decl) 
              << ". Contains: [" << content << "]. Size: " << currentSize << "\n";
    
    return list;
}

DeclListNode* DeclListNode::createParamList(ExprNode* patternNode) {
    DeclNode* param_decl = DeclNode::createParameter(patternNode); 
    return DeclListNode::create(param_decl);
}

DeclListNode* DeclListNode::addParamToList(DeclListNode* list, ExprNode* patternNode) {
    DeclNode* param_decl = DeclNode::createParameter(patternNode); 
    return DeclListNode::addDecl(list, param_decl);
}

DeclListNode* DeclListNode::createConstructorList(char* name) {
    DeclNode* constructor_decl = new DeclNode(NodeType::DECL_CONSTRUCTOR);
    constructor_decl->name = name;

    return DeclListNode::create(constructor_decl);
}

DeclListNode* DeclListNode::addConstructorToList(DeclListNode* list, char* name) {
    DeclNode* new_constructor_decl = new DeclNode(NodeType::DECL_CONSTRUCTOR);
    new_constructor_decl->name = name;

    return DeclListNode::addDecl(list, new_constructor_decl);
}

std::string DeclListNode::listToString() const {
    std::string result = "";
    for (size_t i = 0; i < decls.size(); ++i) {
        if (i > 0) {
            result += ", ";
        }
        result += getDeclName(decls[i]);
    }
    return result;
}

std::string DeclListNode::getDotLabel() const {
    std::stringstream ss;
    ss << "Decl List\\n(" << decls.size() << " items)";
    return ss.str();
}

std::string DeclListNode::toDotString() const {
    std::stringstream ss;
    
    // 1. Объявление узла
    ss << "    " << nodeId << " [label=\"" << getDotLabel() << "\" shape=oval style=filled fillcolor=\"#f0f0ff\"];\n";
    
    // 2. Связь с дочерними DeclNode
    for (size_t i = 0; i < decls.size(); ++i) {
        DeclNode* decl = decls[i];
        if (decl) {
            ss << "    " << nodeId << " -> " << decl->nodeId 
               << " [label=\"Item " << i + 1 << "\"];\n";
            ss << decl->toDotString(); 
        }
    }
    
    return ss.str();
}


/* --- EXPR_NODE --- */


std::string getExprDescription(ExprNode* node) {
    if (!node) return "NULL";

    // 1. FuncCall
    if (node->function != nullptr) {
        if (!node->arguments.empty()) {
            return getExprDescription(node->function) + " " + getExprDescription(node->arguments[0]);
        }
        return getExprDescription(node->function);
    }

    // 2. VarRef
    if (node->type == EXPR_VAR) return node->name; // Убираем 'VarRef()' для чистого имени

    // 3. Literal
    if (node->type == EXPR_LITERAL) return node->value; // Убираем 'Literal()'

    // 4. Более сложные узлы (для полноты)
    if (!node->op.empty()) return "Binary(" + node->op + ")";

    // Заглушка
    return "Complex/Pattern";
}

std::string listExprContent(ExprNode* list) {
    if (!list || list->block.empty()) return "";
    
    std::string content;
    for (size_t i = 0; i < list->block.size(); ++i) {
        if (i > 0) content += ", ";
        content += getExprDescription(list->block[i]);
    }
    return content;
}

ExprNode* ExprNode::createLiteral(const std::string& val) {
    std::cout << "createLiteral(" << val << ")\n";
    ExprNode* node = new ExprNode(NodeType::EXPR_LITERAL);
    node->value = val;
    return node;
}

ExprNode* ExprNode::createVarRef(const std::string& name) {
    std::cout << "createVarRef(" << name << ")\n";
    ExprNode* node = new ExprNode(NodeType::EXPR_VAR); // Используем EXPR_VAR для VarRef
    node->name = name;
    return node;
}

ExprNode* ExprNode::createBinaryExpr(const std::string& op, ExprNode* left, ExprNode* right) {
    ExprNode* node = new ExprNode(NodeType::EXPR_BINARY);
    node->op = op;
    node->left = left;
    node->right = right;
    
    std::cout << "createBinaryExpr(" << op << ") -> L: " 
              << getExprDescription(left) << ", R: " 
              << getExprDescription(right) << "\n";
    return node;
}

ExprNode* ExprNode::createIfExpr(ExprNode* cond, ExprNode* t, ExprNode* f) {
    ExprNode* node = new ExprNode(NodeType::EXPR_IF);
    node->cond = cond;
    node->expr_true = t;
    node->expr_false = f;
    
    std::cout << "createIfExpr -> Cond: " << getExprDescription(cond) << "\n";
    return node;
}

ExprNode* ExprNode::createArrayExpr() {
    ExprNode* node = new ExprNode(NodeType::EXPR_ARRAY);
    std::cout << "createArrayExpr()\n";
    return node;
}

ExprNode* ExprNode::createArrayExpr(ASTNode* elements) {
    ExprNode* node = new ExprNode(NodeType::EXPR_ARRAY);
    // Предполагаем, что elements — это ExprNode* (список элементов)
    ExprNode* elementList = static_cast<ExprNode*>(elements); 

    // Если elementList — это узел списка, мы можем скопировать его block или сохранить ссылку
    if (elementList && elementList->type == NodeType::EXPR_LIST) {
        node->block = elementList->block; // Копируем или присваиваем элементы
        // В реальном коде: удаляем узел elementList, если он больше не нужен
    } else {
        // Или просто сохраняем сам список как левую ветвь для простоты
        node->left = elementList; 
    }
    
    std::cout << "createArrayExpr(args)\n";
    return node;
}

ExprNode* ExprNode::createTupleExpr() {
    ExprNode* node = new ExprNode(NodeType::EXPR_TUPLE); 
    return node;
}

ExprNode* ExprNode::createTupleExpr(ASTNode* elements) {
    ExprNode* node = new ExprNode(NodeType::EXPR_TUPLE);
    node->left = static_cast<ExprNode*>(elements);
    
    std::cout << "createTupleExpr\n";
    return node;
}

ExprNode* ExprNode::createLambda(ExprNode* params, ExprNode* body) {
    ExprNode* node = new ExprNode(NodeType::EXPR_LAMBDA);
    
    // params (ExprNode*), который, вероятно, является EXPR_LIST или DeclNode* // со списком параметров (паттернов).
    node->left = params; 
    
    // body
    node->right = body; 
    
    std::cout << "createLambda -> Params: " 
              << (params ? getExprDescription(params) : "NULL") 
              << ", Body: " << (body ? getExprDescription(body) : "NULL") << "\n";
    return node;
}

ExprNode* ExprNode::createCaseExpr(ExprNode* cond, ASTNode* branches) {
    ExprNode* node = new ExprNode(NodeType::EXPR_CASE);
    node->cond = cond;
    // Предполагаем, что branches является ExprNode* или контейнером, 
    // который можно сохранить в одном из полей. Используем 'right' для списка ветвей.
    node->right = static_cast<ExprNode*>(branches); 
    
    std::cout << "createCaseExpr -> Cond: " << getExprDescription(cond) << "\n";
    return node;
}

ExprNode* ExprNode::createLetInExpr(ASTNode* decls, ExprNode* expr) {
    ExprNode* node = new ExprNode(NodeType::EXPR_LET_IN);
    // Предполагаем, что decls (список DeclNode*) можно сохранить в 'left'.
    node->left = static_cast<ExprNode*>(decls); 
    node->right = expr; // Выражение 'in'
    
    std::cout << "createLetInExpr -> Body: " << getExprDescription(expr) << "\n";
    return node;
}

ExprNode* ExprNode::createDoExpr(ASTNode* block) {
    ExprNode* node = new ExprNode(NodeType::EXPR_DO);
    
    // Блок DO-выражений (binds/exprs) лучше всего хранить в `block`.
    // Предполагаем, что входящий ASTNode* block — это ExprNode* типа EXPR_LIST.
    ExprNode* blockList = static_cast<ExprNode*>(block);
    
    if (blockList && blockList->type == NodeType::EXPR_LIST) {
        node->block = blockList->block; // Копируем список выражений/связываний
    } else {
        node->left = blockList; // Если это не EXPR_LIST, сохраняем как отдельный узел
    }
    
    std::cout << "createDoExpr -> Block: " 
              << (blockList ? "present" : "NULL") << "\n";
    return node;
}

ExprNode* ExprNode::createReturnExpr(ExprNode* expr) {
    ExprNode* node = new ExprNode(NodeType::EXPR_RETURN);
    node->left = expr; 
    std::cout << "createReturnExpr -> Expr: " 
              << (expr ? getExprDescription(expr) : "NULL") << "\n";
    return node;
}

ExprNode* ExprNode::createExprList(ExprNode* first) {
    ExprNode* list = new ExprNode(NodeType::EXPR_LIST);
    list->block.push_back(first);
    
    std::cout << "createExprList. Start: [" << getExprDescription(first) << "]\n";
    return list;
}

ExprNode* ExprNode::addExprToList(ExprNode* list, ExprNode* newExpr) {
    // В отличие от заглушки, мы теперь предполагаем, что list — это контейнер EXPR_LIST
    
    std::string added = getExprDescription(newExpr);
    std::string oldContent = (list && !list->block.empty()) 
                            ? getExprDescription(list->block[0]) : "Empty"; 

    if (list && list->type == NodeType::EXPR_LIST) {
        list->block.push_back(newExpr);
    }
    
    std::cout << "addExprToList. Added: " << added 
              << " to list starts with: [" << oldContent << "]\n";
    
    return list;
}

ExprNode* ExprNode::createFuncCall(ExprNode* funcExpr, ExprNode* argExpr) {
    ExprNode* res = new ExprNode(NodeType::EXPR_FUNC_CALL);
    res->function = funcExpr;

    // Добавляем новый (последний) аргумент

    res->arguments.clear();
    res->arguments.push_back(argExpr);
    res->name = getExprDescription(res);

    std::cout << "createFuncCall. New Call (Name): " << res->name << "\n";

    return(res);
}

ExprNode* ExprNode::createVarPattern(const std::string& identifier) {
    ExprNode* node = new ExprNode(NodeType::EXPR_PATTERN_VAR);
    node->name = identifier;
    std::cout << "createVarPattern:" << identifier << "\n";
    return node;
}

ExprNode* ExprNode::createLiteralPattern(const std::string& literalValue) {
    ExprNode* node = new ExprNode(NodeType::EXPR_PATTERN_LITERAL);
    node->value = literalValue;
    std::cout << "createLiteralPattern:" << literalValue << "\n";
    return node;
}

ExprNode* ExprNode::createConstructorPattern(const std::string& constructorName, ExprNode* args) {
    ExprNode* node = new ExprNode(NodeType::EXPR_PATTERN_CONSTRUCTOR);
    node->name = constructorName;
    node->arguments.push_back(args); // Аргументы конструктора (если есть)
    std::cout << "createConstructorPattern:" << constructorName 
			  << ", Arg: " << getExprDescription(args) << "\n";
    return node;
}

ExprNode* ExprNode::addArgumentToConstructor(ExprNode* constructor, ExprNode* args) {
    constructor->arguments.push_back(args); // Аргументы конструктора (если есть)
    std::cout << "addConstructorPattern:" << constructor->name 
			  << ", Arg: " << getExprDescription(args) << "\n";
    return constructor;
}

ExprNode* ExprNode::createTuplePattern(ExprNode* patternList) {
    ExprNode* node = new ExprNode(NodeType::EXPR_PATTERN_TUPLE);
    node->left = patternList;
    std::cout << "createTuplePattern(args)\n";
    return node;
}

ExprNode* ExprNode::createListPattern(ExprNode* patternList) {
    ExprNode* node = new ExprNode(NodeType::EXPR_PATTERN_LIST);
    node->left = patternList;
    std::cout << "createListPattern(args)\n";
    return node;
}

ExprNode* ExprNode::createTuplePattern() {
    ExprNode* node = new ExprNode(NodeType::EXPR_PATTERN_TUPLE);
    std::cout << "createTuplePattern()\n";
    return node;
}

ExprNode* ExprNode::createListPattern() {
    ExprNode* node = new ExprNode(NodeType::EXPR_PATTERN_LIST);
    std::cout << "createListPattern()\n";
    return node;
}

ExprNode* ExprNode::createConsPattern(ExprNode* headPattern, ExprNode* tailPattern) {
    ExprNode* node = new ExprNode(NodeType::EXPR_PATTERN_CONS);
    node->left = headPattern;
    node->right = tailPattern;
    std::cout << "createConsPattern\n";
    return node;
}

ExprNode* ExprNode::addPatternToList(ExprNode* existingList, ExprNode* newPattern) {
    std::string added = getExprDescription(newPattern);
    std::string currentHead = (existingList && !existingList->block.empty()) 
                              ? getExprDescription(existingList->block[0]) : "Empty"; 
    
    if (existingList && existingList->type == NodeType::EXPR_LIST) {
        existingList->block.push_back(newPattern);
    }
    
    std::cout << "addPatternToList. New element: " << added 
              << ". After element: [" << currentHead << "]\n"; 
    
    return existingList; 
}

ExprNode* ExprNode::createPatternList(ExprNode* singlePattern) {
    ExprNode* list = new ExprNode(NodeType::EXPR_LIST); // Используем EXPR_LIST для паттерн-списков
    list->block.push_back(singlePattern);
    std::cout << "createPatternList. Start: [" << getExprDescription(singlePattern) << "]\n";
    return list; 
}

// Базовые типы (KW_INT, KW_FLOAT, ...)
ExprNode* ExprNode::createPrimitiveType(const std::string& name) {
    ExprNode* node = new ExprNode(NodeType::EXPR_TYPE_PRIMITIVE);
    node->value = name;
    std::cout << "createPrimitiveType(" << name << ")\n";
    return node;
}

ExprNode* ExprNode::createListType(ExprNode* elementType) {
    ExprNode* node = new ExprNode(NodeType::EXPR_TYPE_LIST); // Или новый тип, например, EXPR_TYPE_LIST
    node->name = "[]"; // Имя конструктора
    node->right = elementType; // Храним тип элемента списка (Int в [Int])
    std::cout << "createListType()\n";
    return node;
}

// Конструктор типа (ID_CAP, напр., Maybe, List)
ExprNode* ExprNode::createTypeConstructor(char* name) {
    ExprNode* node = new ExprNode(NodeType::EXPR_TYPE_CONSTRUCTOR);
    node->name = name;
    std::cout << "createTypeConstructor(" << name << ")\n";
    return node;
}

// Переменная типа (ID, напр., a, b)
ExprNode* ExprNode::createTypeVar(char* name) {
    ExprNode* node = new ExprNode(NodeType::EXPR_TYPE_VAR);
    node->name = name;
    std::cout << "createTypeVar(" << name << ")\n";
    return node;
}

// Функциональный тип (type_expr -> type_expr)
ExprNode* ExprNode::createFunctionType(ExprNode* argType, ExprNode* returnType) {
    ExprNode* node = new ExprNode(NodeType::EXPR_TYPE_FUNCTION);
    node->left = argType;
    node->right = returnType;
    std::cout << "createFunctionType\n";
    return node;
}

// Аннотация типа (expr :: type_expr)
ExprNode* ExprNode::createTypeAnnotation(ExprNode* expr, ExprNode* typeExpr) {
    ExprNode* node = new ExprNode(NodeType::EXPR_TYPE_ANNOTATION);
    node->left = expr;
    node->right = typeExpr;
    std::cout << "createTypeAnnotation\n";
    return node;
}

// Создание одной ветви (pattern -> expr)
ExprNode* ExprNode::createCaseBranch(ExprNode* pattern, ExprNode* expr) {
    ExprNode* node = new ExprNode(NodeType::EXPR_CASE_BRANCH);
    node->left = pattern;
    node->right = expr;
    std::cout << "createCaseBranch\n";
    return node;
}

// Создание списка веток (Базовый случай)
ExprNode* ExprNode::createCaseBranchList(ExprNode* branch) {
    ExprNode* list = new ExprNode(NodeType::EXPR_CASE_BRANCH_LIST);
    
    // Ветка (branch) должна быть узлом EXPR_CASE_BRANCH
    list->block.push_back(branch); 
    std::cout << "createCaseBranchList. Start\n";
    return list;
}

// Добавление ветви в список
ExprNode* ExprNode::addCaseBranchToList(ExprNode* list, ExprNode* newBranch) {
    if (list && list->type == NodeType::EXPR_CASE_BRANCH_LIST) {
        list->block.push_back(newBranch);
    }
    std::cout << "addCaseBranchToList. Added branch. Total size: " 
              << (list ? list->block.size() : 0) << "\n";
    return list;
}

std::string ExprNode::getDotLabel() const {
    std::stringstream ss;
    ss << nodeTypeToString(type); // EXPR_LITERAL, EXPR_BINARY, etc.
    
    if (!op.empty()) ss << "\\nOp: " << op;
    if (!value.empty()) ss << "\\nValue: " << value;
    if (!name.empty()) ss << "\\nName: " << name;

    return ss.str();
}

std::string ExprNode::toDotString() const {
    std::stringstream ss;
    
    // 1. Объявление узла
    ss << "    " << nodeId << " [label=\"" << getDotLabel() << "\"];\n";
    
    // 2. Связи, зависящие от типа узла

    // A. Бинарные и функциональные
    if (left) {
        std::string label = (type == EXPR_TYPE_FUNCTION) ? "Arg Type" : "Left";
        ss << "    " << nodeId << " -> " << left->nodeId << " [label=\"" << label << "\"];\n";
        ss << left->toDotString();
    }
    if (right) {
        std::string label = (type == EXPR_TYPE_FUNCTION) ? "Return Type" : "Right";
        ss << "    " << nodeId << " -> " << right->nodeId << " [label=\"" << label << "\"];\n";
        ss << right->toDotString();
    }

    // B. If/Case
    if (cond) {
        ss << "    " << nodeId << " -> " << cond->nodeId << " [label=\"Condition\"];\n";
        ss << cond->toDotString();
    }
    if (expr_true) {
        ss << "    " << nodeId << " -> " << expr_true->nodeId << " [label=\"Then\"];\n";
        ss << expr_true->toDotString();
    }
    if (expr_false) {
        ss << "    " << nodeId << " -> " << expr_false->nodeId << " [label=\"Else\"];\n";
        ss << expr_false->toDotString();
    }
    
    // C. Вызовы функций
    if (function) {
        // Связь с функцией (предыдущим вызовом или именем функции)
        ss << "    " << nodeId << " -> " << function->nodeId << " [label=\"Function\"];\n";
        ss << function->toDotString();

        if (!arguments.empty() && arguments[0]) {
            ExprNode* arg = arguments[0];
            ss << "    " << nodeId << " -> " << arg->nodeId << " [label=\"Argument\"];\n";
            ss << arg->toDotString();
        }
    }

    // D. Списки (Array, Tuple, Case Branches, Do block, ExprList)
    for (size_t i = 0; i < block.size(); ++i) {
        ExprNode* child = block[i];
        if (child) {
            ss << "    " << nodeId << " -> " << child->nodeId 
               << " [label=\"Elem/Branch " << i + 1 << "\"];\n";
            ss << child->toDotString();
        }
    }
    return(ss.str());
}
