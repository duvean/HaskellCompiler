#pragma once
#include <vector>
#include <string>

enum NodeType {
    NODE_PROGRAM,
    DECL_VAR,
    DECL_FUNC,
    DECL_DATA,
    DECL_TYPE,
    DECL_LIST,
    EXPR_LITERAL,
    EXPR_VAR,
    EXPR_BINARY,
    EXPR_FUNC_CALL,
    EXPR_IF,
    EXPR_ARRAY,
    EXPR_TUPLE,
    EXPR_LAMBDA,
    EXPR_CASE,
    EXPR_LETIN,
    EXPR_DO,
    EXPR_RETURN
    // etc
};

std::string nodeTypeToString(NodeType type);

class DeclListNode;

class ASTNode {
public:
    NodeType type;
    ASTNode(NodeType t) : type(t) {}
    virtual ~ASTNode() = default;
};

class DeclNode : public ASTNode {
public:
    std::string name;
    class ExprNode* expr = nullptr;        // выражение справа от '='
    std::vector<std::string> params;       // Список имен параметров для func_definition
    ASTNode* typeExpr = nullptr;           // Для func_type_signature и type_decl
    ASTNode* whereBlock = nullptr;         // Для func_definition

    DeclNode(NodeType t) : ASTNode(t) {}

    static DeclNode* createVarDecl(const std::string& name, ExprNode* expr);
    static DeclNode* createFuncDef(const std::string& name, 
                                  ASTNode* params, 
                                  ExprNode* body, 
                                  ASTNode* whereBlock);
    static DeclNode* createFuncSignature(const std::string& name, ASTNode* typeExpr);
    /*
	static DeclNode* createParamList(const std::string& name);
    static DeclNode* addParamToList(DeclNode* list, const std::string& name);
    */
	static DeclNode* createParamList(ExprNode* patternNode);
	static DeclNode* addParamToList(DeclNode* list, ExprNode* patternNode);
	static DeclNode* createDataDecl(const std::string& name, ASTNode* constructors);
    static DeclNode* createTypeDecl(const std::string& name, ASTNode* typeExpr);
    static DeclNode* createParameter(ExprNode* patternNode);
    static DeclNode* createLetBlock(DeclListNode* declList);
    DeclNode(const std::string& name) : ASTNode(NodeType::DECL_VAR), name(name) {}
};

class DeclListNode : public ASTNode {
public:
    std::vector<DeclNode*> decls;

    DeclListNode() : ASTNode(DECL_LIST) {}
    std::string listToString() const;

    static DeclListNode* create(DeclNode* first);
    static DeclListNode* addDecl(DeclListNode* list, DeclNode* new_decl);
    static DeclListNode* getDeclsFromNode(ASTNode* node); 
    static DeclListNode* createParamList(ExprNode* patternNode);
    static DeclListNode* addParamToList(DeclListNode* list, ExprNode* patternNode);
};

class ProgramNode : public ASTNode {
public:
    std::vector<DeclNode*> decls;
    ProgramNode() : ASTNode(NODE_PROGRAM) {}
    static ProgramNode* create(DeclListNode* declList);
};

class ExprNode : public ASTNode {
public:
    std::string op;       // бинарный оператор
    std::string value;    // литерал
    std::string name;     // переменная
    std::vector<ExprNode*> block;  // для expr_list

    ExprNode* left = nullptr;
    ExprNode* right = nullptr;
    ExprNode* cond = nullptr;
    ExprNode* expr_true = nullptr;
    ExprNode* expr_false = nullptr;
    ExprNode* function = nullptr;
    ExprNode* argument = nullptr;

    ExprNode(NodeType t) : ASTNode(t) {}

    static ExprNode* createLiteral(const std::string& val);
    static ExprNode* createVarRef(const std::string& name);
    static ExprNode* createBinaryExpr(const std::string& op, ExprNode* left, ExprNode* right);
    static ExprNode* createIfExpr(ExprNode* cond, ExprNode* t, ExprNode* f);
    static ExprNode* createArrayExpr(ASTNode* elements);
    // (a, b, c)
    static ExprNode* createTupleExpr(ASTNode* elements);
    // \x -> expr
    static ExprNode* createLambda(ExprNode* params, ExprNode* body);
    // case expr of { ... }
    static ExprNode* createCaseExpr(ExprNode* cond, ASTNode* branches);
    // let decls in expr
    static ExprNode* createLetInExpr(ASTNode* decls, ExprNode* expr);
    // do { ... }
    static ExprNode* createDoExpr(ASTNode* block);
    // return expr
    static ExprNode* createReturnExpr(ExprNode* expr);

    static ExprNode* createExprList(ExprNode* first);
    static ExprNode* addExprToList(ExprNode* list, ExprNode* new_expr);
    ExprNode(const std::string& val) : ASTNode(NodeType::EXPR_LITERAL), value(val) {}
	
	
	
	// ------------------------ Заглушки ---------------------------------
	static ExprNode* createFuncCall(ExprNode* funcExpr, ExprNode* argExpr);
	static ExprNode* createVarPattern(char* identifier); 
	static ExprNode* createLiteralPattern(char* literalValue);
	static ExprNode* createConstructorPattern(char* constructorName, ExprNode* args);
	static ExprNode* createTuplePattern(ExprNode* patternList);
	static ExprNode* createListPattern(ExprNode* patternList);
	static ExprNode* createConsPattern(ExprNode* headPattern, ExprNode* tailPattern);
	static ExprNode* addPatternToList(ExprNode* newPatternPattern, ExprNode* existingListPattern);
	static ExprNode* createPatternList(ExprNode* singlePattern);
};