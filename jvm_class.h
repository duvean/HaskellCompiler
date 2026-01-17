#pragma once    
#include <string>
#include <vector>
#include "ast.h"
#include "constant_pool.h"
#include "semantic_common.h"

struct JvmMethod {
    // Данные для идентификации (для отладки)
    std::string name;
    std::string descriptor;

    int nameIdx = 0;    // Индекс в Constant Pool (Utf8)
    int descIdx = 0;    // Индекс в Constant Pool (Utf8)
    int accessFlags = 0x0009; // ACC_PUBLIC | ACC_STATIC
    std::vector<LocalVariable> locals;
    DeclNode* body;

    // Для синтетических вызовов (например main -> haskellMain)
    std::string callTargetName;

    void syncWithPool(ConstantPool& cp) {
        nameIdx = cp.addUtf8(name);
        descIdx = cp.addUtf8(descriptor);
    }
    
    JvmMethod(std::string n, std::string d, DeclNode* node) 
        : name(n), descriptor(d), body(node) {}
};

struct JvmClass {
    std::string className;
    int classIdx = 0;    // Индекс в Constant Pool (Class info)
    int superIdx = 0;    // Индекс java/lang/Object
    std::vector<JvmMethod> methods;
    ConstantPool& cp;

    JvmClass(std::string name, ConstantPool& pool) 
        : className(name), cp(pool) {
    }
};