#pragma once
#include <vector>
#include <string>
#include <sstream>
#include "jvm_spec.h"

enum NodeType {
    NODE_PROGRAM,
    DECL_VAR,
    DECL_FUNC,
    DECL_FUNC_SIGN,
    DECL_DATA,
    DECL_CONSTRUCTOR,
    DECL_TYPE,
    DECL_LIST,
    DECL_BLOCK,
    DECL_MONADIC_BIND,
    DECL_ACTION,
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
    EXPR_DO_BLOCK,
    EXPR_RETURN,
    EXPR_LIST,
    EXPR_LET_IN,
    EXPR_PATTERN_VAR,
    EXPR_PATTERN_LITERAL,
    EXPR_PATTERN_CONSTRUCTOR,
    EXPR_PATTERN_TUPLE,
    EXPR_PATTERN_LIST,
    EXPR_PATTERN_CONS,
    EXPR_TYPE_PRIMITIVE,
    EXPR_TYPE_LIST,
    EXPR_TYPE_CONSTRUCTOR,
    EXPR_TYPE_VAR,
    EXPR_TYPE_FUNCTION,
    EXPR_TYPE_ANNOTATION,
    EXPR_CASE_BRANCH,
    EXPR_CASE_BRANCH_LIST,
    EXPR_CASTING
};

std::string nodeTypeToString(NodeType type);

class DeclListNode;

class ASTNode {
public:
    NodeType type;
    size_t nodeId;
    static size_t nextNodeId;

    virtual std::string toDotString() const = 0; 
    virtual std::string getDotLabel() const = 0; 

    // --- Семантические атрибуты ---
    SemanticType* inferredType = SemanticType::Unknown();
    int constPoolIndex = 0;
    int localVarIndex = -1;
    bool isCastNode = false;

    ASTNode(NodeType t) : type(t), nodeId(nextNodeId++) {}
    virtual ~ASTNode() = default;
};

class DeclNode : public ASTNode {
public:
    std::string name;
    class ExprNode* expr = nullptr;        // выражение справа от '='
    std::vector<std::string> params;       // Список имен параметров для func_definition
    ASTNode* typeExpr = nullptr;           // Для func_type_signature и type_decl
    ASTNode* paramsList = nullptr;         // Для параметров в объявлении функции
    ASTNode* letBlock = nullptr;           // Локлаьные объявления let
    ASTNode* whereBlock = nullptr;         // Локлаьные объявления where
    ASTNode* enumData = nullptr;           // data enumLike = A | B | C

    static DeclNode* createVarDecl(const std::string& name, ExprNode* expr);
    static DeclNode* createFuncDef(const std::string& name, 
                                  ASTNode* params, 
                                  ExprNode* body, 
                                  ASTNode* whereBlock);
    static DeclNode* createFuncSignature(const std::string& name, ASTNode* typeExpr);
	static DeclNode* createDataDecl(const std::string& name, ASTNode* constructors);
    static DeclNode* createTypeDecl(const std::string& name, ASTNode* typeExpr);
    static DeclNode* createMonadicBind(const std::string& name, ExprNode* expr);
    static DeclNode* createAction(ExprNode* expr);
    static DeclNode* createParameter(ExprNode* patternNode);
    static DeclNode* createLetBlock(DeclListNode* declList);

    virtual std::string toDotString() const override;
    virtual std::string getDotLabel() const override;

    DeclNode(NodeType t) : ASTNode(t) {}
    DeclNode(const std::string& name) : ASTNode(NodeType::DECL_VAR), name(name) {}
};



class DeclListNode : public ASTNode {
public:
    std::vector<DeclNode*> decls;

    DeclListNode() : ASTNode(DECL_LIST) {}
    std::string listToString() const;

    virtual std::string toDotString() const override;
    virtual std::string getDotLabel() const override;

    static DeclListNode* create(DeclNode* first);
    static DeclListNode* addDecl(DeclListNode* list, DeclNode* new_decl);
    static DeclListNode* getDeclsFromNode(ASTNode* node); 
    static DeclListNode* createParamList(ExprNode* patternNode);
    static DeclListNode* addParamToList(DeclListNode* list, ExprNode* patternNode);
    static DeclListNode* createConstructorList(char* name);
    static DeclListNode* addConstructorToList(DeclListNode* list, char* name);
};



class ProgramNode : public ASTNode {
public:
    std::vector<DeclNode*> decls;

    static ProgramNode* create(DeclListNode* declList);

    virtual std::string toDotString() const override;
    virtual std::string getDotLabel() const override;

    ProgramNode() : ASTNode(NODE_PROGRAM) {}

};



class ExprNode : public ASTNode {
public:
    std::string op;       // бинарный оператор
    std::string value;    // литерал
    std::string name;     // переменная
    std::vector<ExprNode*> block;  // для expr_list
	std::vector<ExprNode*> arguments;
    
    ExprNode* left = nullptr;
    ExprNode* right = nullptr;
    ExprNode* cond = nullptr;
    ExprNode* expr_true = nullptr;
    ExprNode* expr_false = nullptr;
    ExprNode* function = nullptr;
    DeclListNode* decls = nullptr;
    bool isFunctionRef = false;
    bool isBuiltinFunciton = false;
    
    static ExprNode* createLiteral(const std::string& val);
    static ExprNode* createVarRef(const std::string& name);
    static ExprNode* createBinaryExpr(const std::string& op, ExprNode* left, ExprNode* right);
    static ExprNode* createIfExpr(ExprNode* cond, ExprNode* t, ExprNode* f);
    static ExprNode* createArrayExpr();
    static ExprNode* createArrayExpr(ASTNode* elements);
    // (a, b, c), ()
    static ExprNode* createTupleExpr();
    static ExprNode* createTupleExpr(ASTNode* elements);
    // \x -> expr
    static ExprNode* createLambda(ExprNode* params, ExprNode* body);
    // case expr of { ... }
    static ExprNode* createCaseExpr(ExprNode* cond, ASTNode* branches);
    // let decls in expr
    static ExprNode* createLetInExpr(ASTNode* decls, ExprNode* expr);
    // do { ... }
    static ExprNode* createDoExpr(DeclListNode* decls);
    // return expr
    static ExprNode* createReturnExpr(ExprNode* expr);

    static ExprNode* createExprList(ExprNode* first);
    static ExprNode* addExprToList(ExprNode* list, ExprNode* new_expr);

	static ExprNode* createFuncCall(ExprNode* funcExpr, ExprNode* argExpr);
	static ExprNode* createVarPattern(const std::string& identifier); 
	static ExprNode* createLiteralPattern(const std::string& literalValue);
	static ExprNode* createConstructorPattern(const std::string& constructorName, ExprNode* args);
	static ExprNode* addArgumentToConstructor(ExprNode* constructor, ExprNode* args);
	static ExprNode* createTuplePattern(ExprNode* patternList);
	static ExprNode* createListPattern(ExprNode* patternList);
    static ExprNode* createTuplePattern();
	static ExprNode* createListPattern();
	static ExprNode* createConsPattern(ExprNode* headPattern, ExprNode* tailPattern);
	static ExprNode* addPatternToList(ExprNode* newPatternPattern, ExprNode* existingListPattern);
	static ExprNode* createPatternList(ExprNode* singlePattern);

    static ExprNode* createPrimitiveType(const std::string& name);
    static ExprNode* createUnitType();
    static ExprNode* createListType(ExprNode* elementType);
    static ExprNode* createTypeConstructor(char* name);
    static ExprNode* createTypeVar(char* name);
    static ExprNode* createFunctionType(ExprNode* argType, ExprNode* returnType);
    static ExprNode* createTypeAnnotation(ExprNode* expr, ExprNode* typeExpr);
    static ExprNode* createConstructorList(char* name);
    static ExprNode* addConstructorToList(ExprNode* list, char* name);
    static ExprNode* createCaseBranch(ExprNode* pattern, ExprNode* expr);
    static ExprNode* createCaseBranchList(ExprNode* branch);
    static ExprNode* addCaseBranchToList(ExprNode* list, ExprNode* newBranch);

    virtual std::string toDotString() const override; // Добавить вывод аргументов!
    virtual std::string getDotLabel() const override;

    ExprNode(NodeType t) : ASTNode(t) {}
    ExprNode(const std::string& val) : ASTNode(NodeType::EXPR_LITERAL), value(val) {}
};