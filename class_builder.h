#include <iostream>
#include <fstream>
#include <vector>
#include <cstring>
#include <cstdint>
#include <algorithm>
#include "jvm_class.h"

class ClassBuilder {
private:
    std::string filename;
    JvmClass* jvmClass;
    std::ofstream out;

    // --- Низкоуровневая запись ---
    
    void writeU1(uint8_t val) {
        out.write(reinterpret_cast<const char*>(&val), 1);
    }

    void writeU2(uint16_t val) {
        // Big Endian: сначала старший байт
        uint8_t bytes[2];
        bytes[0] = (val >> 8) & 0xFF;
        bytes[1] = val & 0xFF;
        out.write(reinterpret_cast<const char*>(bytes), 2);
    }

    void writeU4(uint32_t val) {
        uint8_t bytes[4];
        bytes[0] = (val >> 24) & 0xFF;
        bytes[1] = (val >> 16) & 0xFF;
        bytes[2] = (val >> 8) & 0xFF;
        bytes[3] = val & 0xFF;
        out.write(reinterpret_cast<const char*>(bytes), 4);
    }

    void writeBytes(const std::vector<uint8_t>& bytes) {
        out.write(reinterpret_cast<const char*>(bytes.data()), bytes.size());
    }

    void writeString(const std::string& str) {
        out.write(str.c_str(), str.size());
    }

public:
    ClassBuilder(const std::string& fname, JvmClass* cls) 
        : filename(fname), jvmClass(cls) {}

    bool build();
    
private:
    void prepareConstants();
    void writeConstantPool();
    void writeInterfaces();
    void writeFields();
    void writeMethods();
    void writeAttributes(); // Атрибуты самого класса (SourceFile и т.д.)

    uint8_t getReturnOpcode(const std::string& descriptor);
};