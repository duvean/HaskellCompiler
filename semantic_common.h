#pragma once
#include <vector>
#include "semantic_type.h"

struct LocalVariable {
    int index;
    SemanticType* type;
    LocalVariable(SemanticType* t = nullptr, int i = -1) : type(t), index(i) {}
};

struct FunctionSignature {
    std::vector<SemanticType*> paramTypes;
    SemanticType* returnType;
    std::string getDescriptor() const {
    std::string res = "(";
        for (auto* t : paramTypes) {
            if (t) {
                std::string d = t->getDescriptor();
                if (d != "V") res += d;
            }
        }
        res += ")";
        if (returnType) res += returnType->getDescriptor();
        else res += "V";
        return res;
    }
};