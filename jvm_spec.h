#pragma once
#include <string>

enum class SemanticType {
    Void,
    Int,
    Float,
    Bool,
    Char,
    String,
    List,   
    Unknown
};

inline std::string typeToString(SemanticType t) {
    switch(t) {
        case SemanticType::Int:    return "Int";
        case SemanticType::Float:  return "Float";
        case SemanticType::Bool:   return "Bool";
        case SemanticType::String: return "String";
        case SemanticType::Void:   return "Void";
        case SemanticType::List:   return "[List]";
        default:                   return "Unknown";
    }
}

enum JvmConstantTag {
    CONSTANT_Utf8 = 1,
    CONSTANT_Integer = 3,
    CONSTANT_Float = 4,
    CONSTANT_Class = 7,
    CONSTANT_String = 8,
    CONSTANT_Fieldref = 9,
    CONSTANT_Methodref = 10,
    CONSTANT_NameAndType = 12
};

inline std::string getJvmDescriptor(SemanticType type, SemanticType elementType = SemanticType::Unknown) {
    switch (type) {
        case SemanticType::Int:    return "I";
        case SemanticType::Float:  return "F";
        case SemanticType::Bool:   return "Z";
        case SemanticType::String: return "Ljava/lang/String;";
        case SemanticType::List: {
            if (elementType == SemanticType::Float) return "[F";
            if (elementType == SemanticType::Bool)  return "[Z";
            return "[I";
        }
        default: return "V";
    }
}