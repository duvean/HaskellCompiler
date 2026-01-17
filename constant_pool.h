#pragma once
#include <map>
#include <vector>
#include <string>
#include <iostream>
#include "semantic_type.h"

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

struct ConstantPoolEntry {
    JvmConstantTag  tag;
    std::string     stringValue;  // для Utf8
    int             intValue;     // для Integer
    float           floatValue;   // для Float
    int             refIndex1;    // class_index / string_index
    int             refIndex2;    // name_and_type_index
};

class ConstantPool {
private:
    std::vector<ConstantPoolEntry>  entries;
    std::map<std::string, int>      utf8Cache;   // Кэш для CONSTANT_utf8
    std::map<std::string, int>      stringCache; // Кэш для CONSTANT_String
    std::map<int, int>              intCache;    // Кэш для CONSTANT_Integer
    std::map<float, int>            floatCache;  // Кэш для CONSTANT_Float

public:
    int addEntry(const ConstantPoolEntry& entry);

    int addUtf8(const std::string& val);
    int addStringLiteral(const std::string& val);
    int addInteger(int val);
    int addFloat(float val);

    int addClass(const std::string& className);
    int addNameAndType(const std::string& name, const std::string& descriptor);
    int addMethodRef(const std::string& className, const std::string& methodName, const std::string& descriptor);
    
    int findMethodRef(const std::string& className, const std::string& methodName, const std::string& descriptor);

    const std::vector<ConstantPoolEntry>& getEntries() const { return entries; }
    ConstantPool() { entries.emplace_back(); }
};