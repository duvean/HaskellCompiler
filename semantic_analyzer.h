#pragma once
#include <map>
#include <string>
#include <vector>
#include "ast.h"
#include "constant_pool.h"

struct LocalVariable {
    int index;
    SemanticType* type;
    LocalVariable(SemanticType* t = nullptr, int i = -1) : type(t), index(i) {}
};

struct FunctionSignature {
    std::vector<SemanticType*> paramTypes;
    SemanticType* returnType;
};

class SemanticAnalyzer {
private:
    ConstantPool& constPool;
    
    std::map<std::string, LocalVariable> symbolTable;
    std::map<std::string, FunctionSignature> functionSignatures;
    std::map<std::string, FunctionSignature> builtinSignatures;

    int nextLocalIndex = 1;

public:
    SemanticAnalyzer(ConstantPool& cp) : constPool(cp) { initBuiltins(); }
    void analyze(ProgramNode* root);

private:
    void initBuiltins();

    void analyzeDeclList(DeclListNode* list);
    void analyzeDecl(DeclNode* node);
    void analyzeExpr(ExprNode* node);
    void analyzePattern(ExprNode* pattern, SemanticType* expectedType, int& sourceLocalIndex);

    void collectTypes(ASTNode* node, std::vector<SemanticType*>& types);
    void flattenCall(ExprNode* node, std::vector<ExprNode*>& args, ExprNode** finalFunc);
    
    ExprNode* createCastNode(ExprNode* target, SemanticType* toType);
};