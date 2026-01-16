#pragma once
#include <string>
#include <vector>

enum class TypeKind { PRIMITIVE, LIST, FUNCTION, CONSTRUCTOR, UNKNOWN };
enum class BaseType { INT, FLOAT, BOOL, STRING, VOID, IO };

class SemanticType {
public:
    TypeKind kind;
    BaseType base;           // Для примитивов
    SemanticType* subType;   // Для списков (тип элементов)
    std::string typeName;    // Для монад

    // Для функций:
    std::vector<SemanticType*> paramTypes;
    SemanticType* returnType;

    SemanticType(BaseType b) : kind(TypeKind::PRIMITIVE), base(b), subType(nullptr) {}
    SemanticType(TypeKind k) : kind(k), base(BaseType::VOID), subType(nullptr), returnType(nullptr) {}
    SemanticType(TypeKind k, SemanticType* sub) : kind(k), base(BaseType::VOID), subType(sub) {}
    SemanticType() : kind(TypeKind::UNKNOWN), base(BaseType::VOID), subType(nullptr) {}
    SemanticType(std::vector<SemanticType*> params, SemanticType* ret) 
        : kind(TypeKind::FUNCTION), base(BaseType::VOID), 
          subType(nullptr), paramTypes(params), returnType(ret) {}

    static SemanticType* Int    ()  { return new SemanticType(BaseType::INT); }
    static SemanticType* Float  ()  { return new SemanticType(BaseType::FLOAT); }
    static SemanticType* Bool   ()  { return new SemanticType(BaseType::BOOL); }
    static SemanticType* String ()  { return new SemanticType(BaseType::STRING); }
    static SemanticType* Void   ()  { return new SemanticType(BaseType::VOID); }
    static SemanticType* Unknown()  { return new SemanticType(); }
    static SemanticType* List(SemanticType* inner) {
        return new SemanticType(TypeKind::LIST, inner);
    }
    static SemanticType* Function(std::vector<SemanticType*> params, SemanticType* ret) {
        return new SemanticType(params, ret);
    }
    static SemanticType* IO() {
        SemanticType* t = new SemanticType(TypeKind::CONSTRUCTOR);
        t->typeName = "IO"; 
        return t;
    }
    
    bool equals(SemanticType* other) {
        if (!other || kind != other->kind) return false;

        switch (kind) {
            case TypeKind::PRIMITIVE:
                return base == other->base;
            case TypeKind::LIST:
                if (!subType || !other->subType) return subType == other->subType;
                return subType->equals(other->subType);
            case TypeKind::FUNCTION:
                if (paramTypes.size() != other->paramTypes.size()) return false;
                for (size_t i = 0; i < paramTypes.size(); ++i) {
                    if (!paramTypes[i]->equals(other->paramTypes[i])) return false;
                }
                return returnType->equals(other->returnType);
            case TypeKind::CONSTRUCTOR:
                return typeName == other->typeName;
            case TypeKind::UNKNOWN:
                // UNKNOWN не равен даже другому UNKNOWN для безопасности
                return false;
            default:
                return true;
        }
    }

    std::string getDescriptor() {
        if (kind == TypeKind::PRIMITIVE) {
            switch (base) {
                case BaseType::INT:    return "I";
                case BaseType::BOOL:   return "Z";
                case BaseType::FLOAT:  return "F";
                case BaseType::STRING: return "Ljava/lang/String;";
                default:               return "V";
            }
        }
        if (kind == TypeKind::LIST) return "[" + subType->getDescriptor();
        if (kind == TypeKind::CONSTRUCTOR && typeName == "IO") return "V";
        if (kind == TypeKind::CONSTRUCTOR) return "L" + typeName + ";";
        
        // Если функция передается как аргумент (лямбда), в JVM это обычно Object или спец интерфейс
        if (kind == TypeKind::FUNCTION) return "Ljava/lang/Object;"; 
        
        return "V";
    }

    std::string toString() {
        if (kind == TypeKind::UNKNOWN) return "?";
        if (kind == TypeKind::PRIMITIVE) {
            if (base == BaseType::INT) return "Int";
            if (base == BaseType::BOOL) return "Bool";
            if (base == BaseType::FLOAT) return "Float";
        }
        if (kind == TypeKind::LIST) return "[" + subType->toString() + "]";
        if (kind == TypeKind::CONSTRUCTOR) return typeName;

        if (kind == TypeKind::FUNCTION) {
            std::string res = "(";
            for (size_t i = 0; i < paramTypes.size(); ++i) {
                res += paramTypes[i]->toString() + (i == paramTypes.size() - 1 ? "" : ", ");
            }
            res += ") -> " + returnType->toString();
            return res;
        }
        return getDescriptor();
    }
};
