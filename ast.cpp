#include <iostream>
#include "ast.h"

std::string getPatternName(ExprNode* patternNode) {
    if (patternNode) {
        return patternNode->name;
    }
    return "NULL_PATTERN";
}

std::string nodeTypeToString(NodeType type) {
    switch (type) {
        case NODE_PROGRAM:     return "PROGRAM";
        case DECL_VAR:         return "VAR_DECL";
        case DECL_FUNC:        return "FUNC_DECL";
        case DECL_LIST:        return "DECL_LIST";
        case DECL_DATA:        return "DECL_DATA";
        case DECL_TYPE:        return "DECL_TYPE";
        default:               return "UNKNOWN";
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

ProgramNode* ProgramNode::create(DeclListNode* declList) {
    ProgramNode* root = new ProgramNode();
    if (declList) {
        root->decls = std::move(declList->decls); 
    }
    std::cout << "createProgram\n";
    return root;
}

DeclNode* DeclNode::createVarDecl(const std::string& name, ExprNode* expr) {
    std::cout << "createVarDecl(" << name << ")\n";
    return new DeclNode(name);
}

DeclNode* DeclNode::createFuncDef(const std::string& name, 
                                  ASTNode* params, 
                                  ExprNode* body, 
                                  ASTNode* whereBlock) 
{
    DeclNode* node = new DeclNode(NodeType::DECL_FUNC);
    node->name = name;
    node->expr = body; 
    std::cout << "createFuncDef(" << name << ")\n";
    return node;
}

DeclNode* DeclNode::createFuncSignature(const std::string& name, ASTNode* typeExpr) {
    std::cout << "createFuncSignature(" << name << ")\n";
    return new DeclNode(name);
}

// Заглушка для addParamToList выше
DeclNode* DeclNode::createParamList(ExprNode* patternNode) {
    
    // Поскольку у нас есть только узел, мы не можем вывести имя, как раньше.
    std::cout << "createParamList\n";
    
    //Реализация: 
    // Создаем новый узел DeclNode, который будет выступать контейнером (списком) для параметров.
    DeclNode* list = new DeclNode(NodeType::NODE_PROGRAM); // Используем ваш тип для списка

    //ВАЖНО: 
    // В отличие от старой версии, где мы создавали новый DeclNode для имени, 
    // здесь мы должны добавить уже существующий ExprNode* (patternNode) в список.
    // Если list->block ожидает только DeclNode*, здесь возникнет проблема типа!
    // Для заглушки мы просто добавляем указатель, притворяясь, что 'block' его примет.

    if (patternNode) {
        // Мы предполагаем, что существует способ добавить ExprNode* в список DeclNode'ов.
        // Если list->block — это std::vector<DeclNode*>, этот код вызовет ошибку компиляции 
        // или undefined behavior, если вы используете reinterpret_cast! 
        // Это чистая заглушка.
        // list->block.push_back(patternNode); // <-- Ошибка типа
    }
    
    // Возвращаем контейнер (список)
    return list;
}

// Заглушка для addParamToList выше
DeclNode* DeclNode::addParamToList(DeclNode* list, ExprNode* patternNode) {
    std::cout << "addParamToList:\n";
    return list;
}

DeclNode* DeclNode::createDataDecl(const std::string& name, ASTNode* constructors) {
    std::cout << "createDataDecl(" << name << ")\n";
    return new DeclNode(name);
}

DeclNode* DeclNode::createTypeDecl(const std::string& name, ASTNode* typeExpr) {
    DeclNode* node = new DeclNode(NodeType::DECL_TYPE);
    node->name = name;
    node->typeExpr = typeExpr;
    std::cout << "createTypeDecl(" << name << ")\n";
    return node;
}

DeclNode* DeclNode::createLetBlock(DeclListNode* declList) {
    DeclNode* node = new DeclNode(NodeType::DECL_VAR);
    node->whereBlock = declList; 
    node->name = ""; // Имя не требуется для всего блока

    std::cout << "createLetBlock: created node containing list of size " 
               << (declList ? declList->decls.size() : 0) << "\n";
    return node;
}

DeclNode* DeclNode::createParameter(ExprNode* patternNode) {
    // Параметр — это, по сути, объявление переменной в локальной области видимости.
    DeclNode* node = new DeclNode(NodeType::DECL_VAR);
    
    // В DeclNode мы используем поле expr для хранения паттерна параметра.
    // Если паттерн является простым ID, мы извлекаем имя.
    // node->name = patternNode->name;
    node->expr = patternNode;
    // NOTE: Если patternNode является VarRef/ID, мы должны извлечь имя для node->name.
    // Если ExprNode имеет поле name:
    /*
    if (patternNode->type == EXPR_VAR) {
        node->name = static_cast<VarRefNode*>(patternNode)->name; 
    }
    */
    std::cout << "createParameter: created DeclNode for parameter.\n";
    return node;
}





/* --- DeclList --- */
DeclListNode* DeclListNode::create(DeclNode* first) {
    DeclListNode* list = new DeclListNode();
    std::string declName = getDeclName(first); 

    if (first) {
        list->decls.push_back(first);
    }
    
    // Вывод: "Создан DeclList. Содержимое: [calc]"
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
    
    // Вывод: "Добавлен {result}. Содержимое: [calc, {DECL_FUNC_DEF}, result]. Size: 3"
    std::cout << "addDeclToList. Added: " << getDeclName(new_decl) 
              << ". Contains: [" << content << "]. Size: " << currentSize << "\n";
    
    return list;
}

DeclListNode* DeclListNode::createParamList(ExprNode* patternNode) {
    DeclNode* param_decl = DeclNode::createParameter(patternNode); 
    return DeclListNode::create(param_decl);
}

// Обновляем addParamToList, чтобы он передавал управление обновленной addDecl()
DeclListNode* DeclListNode::addParamToList(DeclListNode* list, ExprNode* patternNode) {
    DeclNode* param_decl = DeclNode::createParameter(patternNode); 
    return DeclListNode::addDecl(list, param_decl);
}


/* --- Expr --- */

std::string getExprDescription(ExprNode* node) {
    if (!node) return "NULL";

    // 1. FuncCall (проверяем function != nullptr)
    if (node->function != nullptr) return "FuncCall(" + getExprDescription(node->function) + ")";

    // 2. VarRef
    if (node->type == EXPR_VAR) return "VarRef(" + node->name + ")";
    
    // 3. Literal
    if (node->type == EXPR_LITERAL) return "Literal(" + node->value + ")";
    
    // Для более сложных узлов (например, BINARY, IF)
    if (!node->op.empty()) return "Binary(" + node->op + ")"; 

    // Заглушка, если нет полей:
    return "Complex/Pattern"; 
}

// --- Новый метод для вывода содержимого списка ---
// Предполагаем, что list — это список, который содержит элементы в list->block
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
    ExprNode* node = new ExprNode(NodeType::EXPR_VAR);
    node->name = name;
    return node;
}

ExprNode* ExprNode::createBinaryExpr(const std::string& op, ExprNode* left, ExprNode* right) {
    std::cout << "createBinaryExpr(" << op << ")\n";
    return new ExprNode("binary");
}

ExprNode* ExprNode::createIfExpr(ExprNode* cond, ExprNode* t, ExprNode* f) {
    std::cout << "createIfExpr\n";
    return new ExprNode("if");
}

ExprNode* ExprNode::createArrayExpr(ASTNode* elements) {
    std::cout << "createArrayExpr\n";
    return new ExprNode("array");
}

ExprNode* ExprNode::createTupleExpr(ASTNode* elements) {
    std::cout << "createTupleExpr\n";
    return new ExprNode("tuple");
}

ExprNode* ExprNode::createLambda(ExprNode* params, ExprNode* body) {
    std::cout << "createLambda\n";
    return new ExprNode("lambda");
}

ExprNode* ExprNode::createCaseExpr(ExprNode* cond, ASTNode* branches) {
    std::cout << "createCaseExpr\n";
    return new ExprNode("case");
}

ExprNode* ExprNode::createLetInExpr(ASTNode* decls, ExprNode* expr) {
    std::cout << "createLetInExpr\n";
    return new ExprNode("let-in");
}

ExprNode* ExprNode::createDoExpr(ASTNode* block) {
    std::cout << "createDoExpr\n";
    return new ExprNode("do");
}

ExprNode* ExprNode::createReturnExpr(ExprNode* expr) {
    std::cout << "createReturnExpr\n";
    return new ExprNode("return");
}

ExprNode* ExprNode::createExprList(ExprNode* first) {
    // В реальной реализации: Создается новый узел списка, first добавляется в list->block
    // ExprNode* list = new ExprNode(NodeType::EXPR_LIST);
    // list->block.push_back(first);
    
    // В заглушке: first становится списком.
    std::cout << "createExprList. Start: [" << getExprDescription(first) << "]\n";
    return first;
}

ExprNode* ExprNode::addExprToList(ExprNode* list, ExprNode* newExpr) {
    // В реальной реализации: newExpr добавляется в list->block.
    // list->block.push_back(newExpr);

    // ВНИМАНИЕ: Если list является одиночным узлом (как в вашей заглушке), 
    // это не совсем корректно. Но для трассировки:
    
    // Моделируем добавление
    std::string added = getExprDescription(newExpr);
    
    // Моделируем старое содержимое (это очень приблизительно в вашей заглушке)
    std::string oldContent = getExprDescription(list); 

    std::cout << "addExprToList. Added: " << added 
              << " to list starts with: [" << oldContent << "]\n";
    
    return list;
}


// ------------------------ Заглушки ---------------------------------
ExprNode* ExprNode::createFuncCall(ExprNode* funcExpr, ExprNode* argExpr) {
    ExprNode* res = new ExprNode("FuncCall");
    res->function = funcExpr;
    res->argument = argExpr;
    std::cout << "createFuncCall. Func: " << getExprDescription(funcExpr) 
              << ", Arg: " << getExprDescription(argExpr) << "\n";
    return res;
}

ExprNode* ExprNode::createVarPattern(char* identifier) {
    std::cout << "createVarPattern:" << identifier << "\n";
	// В реальной реализации здесь должно быть: 
    // return new PatternNode(identifier, PatternType::Variable);
    return nullptr;
}

ExprNode* ExprNode::createLiteralPattern(char* literalValue) {
    std::cout << "createLiteralPattern:" << literalValue << "\n";
    // В реальной реализации здесь должно быть: 
    // return new PatternNode(literalValue, PatternType::Literal);
    return nullptr;
}

ExprNode* ExprNode::createConstructorPattern(char* constructorName, ExprNode* args) {
    std::cout << "createConstructorPattern:" << constructorName << "\n";
    // В реальной реализации здесь должно быть: 
    // return new PatternNode(constructorName, args, PatternType::Constructor);
    return nullptr;
}

ExprNode* ExprNode::createTuplePattern(ExprNode* patternList) {
    std::cout << "createTuplePattern\n";
    // В реальной реализации здесь должно быть: 
    // return new PatternNode(patternList, PatternType::Tuple);
    return nullptr;
}

ExprNode* ExprNode::createListPattern(ExprNode* patternList) {
    std::cout << "createListPattern\n";
    // В реальной реализации здесь должно быть: 
    // return new PatternNode(patternList, PatternType::List);
    return nullptr;
}

ExprNode* ExprNode::createConsPattern(ExprNode* headPattern, ExprNode* tailPattern) {
    std::cout << "createConsPattern\n";
    // В реальной реализации здесь должно быть: 
    // return new PatternNode(headPattern, tailPattern, PatternType::Cons);
    return nullptr;
}

ExprNode* ExprNode::addPatternToList(ExprNode* newPattern, ExprNode* existingList) {
    // newPatternPattern становится новым началом списка
    // В реальной реализации: newPattern->setNext(existingList);

    std::string added = getExprDescription(newPattern);
    std::string currentHead = getExprDescription(existingList);
    
    std::cout << "addPatternToList. New element: " << added 
              << ". After element: [" << currentHead << "]\n"; 
    
    // Возвращаем существующий список, чтобы показать, что это новый узел списка.
    // Примечание: В Yacc/Bison это обычно возвращает новый head ($1).
    // Ваша заглушка возвращает $2.
    return existingList; 
}

ExprNode* ExprNode::createPatternList(ExprNode* singlePattern) {
    std::cout << "createPatternList. Start: [" << getExprDescription(singlePattern) << "]\n";
    return singlePattern; 
}