#include "constant_pool.h"

int ConstantPool::addEntry(const ConstantPoolEntry& entry) {
    // 1. Поиск дубликатов
    for (size_t i = 1; i < entries.size(); ++i) {
        const auto& existing = entries[i];
        if (existing.tag == entry.tag &&
            existing.refIndex1 == entry.refIndex1 &&
            existing.refIndex2 == entry.refIndex2 &&
            existing.intValue == entry.intValue &&
            existing.floatValue == entry.floatValue &&
            existing.stringValue == entry.stringValue) {
            return (int)i; // Возвращаем индекс уже существующей записи
        }
    }

    // 2. Добавление новой записи
    entries.push_back(entry);
    
    // Индекс в JVM Constant Pool - это размер вектора -1 (т.к. 0-й элемент пустой)
    int newIndex = (int)(entries.size() - 1);

    return newIndex;
}

int ConstantPool::addUtf8(const std::string &val)
{
    if (utf8Cache.find(val) != utf8Cache.end()) 
        return utf8Cache[val];

    ConstantPoolEntry e;
    e.tag = CONSTANT_Utf8;
    e.stringValue = val;
    entries.push_back(e);

    int idx = (int)entries.size() - 1;
    utf8Cache[val] = idx;

    return idx;
}

int ConstantPool::addStringLiteral(const std::string &val)
{
    int utf8Index = addUtf8(val);

    ConstantPoolEntry e;
    e.tag = CONSTANT_String;
    e.refIndex1 = utf8Index;
    entries.push_back(e);

    return (int)entries.size() - 1;
}

int ConstantPool::addInteger(int val)
{
    ConstantPoolEntry e;
    e.tag = CONSTANT_Integer;
    e.intValue = val;
    entries.push_back(e);
    return (int)entries.size() - 1;
}

int ConstantPool::addFloat(float val)
{
    ConstantPoolEntry e;
    e.tag = CONSTANT_Float;
    e.floatValue = val;
    entries.push_back(e);
    
    return (int)entries.size() - 1;
}

int ConstantPool::addClass(const std::string &className)
{
    int nameIdx = addUtf8(className);
    return addEntry({JvmConstantTag::CONSTANT_Class, "", 0, 0.0f, nameIdx, 0});
}

int ConstantPool::addNameAndType(const std::string &name, const std::string &descriptor)
{
    int nameIdx = addUtf8(name);
    int descIdx = addUtf8(descriptor);
    return addEntry({JvmConstantTag::CONSTANT_NameAndType, "", 0, 0.0f, nameIdx, descIdx});
}

int ConstantPool::addMethodRef(const std::string &className, const std::string &methodName, const std::string &descriptor)
{
    int classIdx = addClass(className);
    int ntIdx = addNameAndType(methodName, descriptor);
    return addEntry({JvmConstantTag::CONSTANT_Methodref, "", 0, 0.0f, classIdx, ntIdx});
}