#pragma once
#include <map>
#include <string>
#include <vector>
#include "ast.h"
#include "constant_pool.h"

struct LocalVariable {
    int index;
    SemanticType type;
};

struct FunctionSignature {
    std::vector<SemanticType> paramTypes;
    SemanticType returnType;
};

class SemanticAnalyzer {
private:
    ConstantPool& constPool;
    
    // Таблица символов: Имя переменной -> Информаця
    std::map<std::string, LocalVariable> symbolTable;
    // Хранилище сигнатур: ИмяФункции -> СписокТипов
    std::map<std::string, FunctionSignature> functionSignatures;

    int nextLocalIndex = 1;

public:
    SemanticAnalyzer(ConstantPool& cp) : constPool(cp) {}
    void analyze(ProgramNode* root);

private:
    void analyzeDeclList(DeclListNode* list);
    void analyzeDecl(DeclNode* node);
    void analyzeExpr(ExprNode* node);
    void analyzePattern(ExprNode* pattern, SemanticType type, int sourceLocalIndex);

    std::string makeMethodDescriptor(DeclNode* funcNode);
    void collectTypes(ASTNode* node, std::vector<SemanticType>& types);

    ExprNode* createCastNode(ExprNode* target, SemanticType toType);
};