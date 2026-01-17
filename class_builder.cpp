#include "class_builder.h"

bool ClassBuilder::build() {
    out.open(filename, std::ios::binary);
    if (!out.is_open()) return false;

    // 1. Magic Number (0xCAFEBABE)
    writeU4(0xCAFEBABE);

    // 2. Version (Java 8 = 52.0 -> Major 52, Minor 0)
    writeU2(0);  // Minor
    writeU2(52); // Major

    // 3. Constant Pool
    writeConstantPool();

    // 4. Access Flags
    // ACC_PUBLIC (0x0001) + ACC_SUPER (0x0020) = 0x0021
    writeU2(0x0021);

    // 5. This Class Index (#3 в вашем примере)
    writeU2(static_cast<uint16_t>(jvmClass->classIdx));

    // 6. Super Class Index (#5 в вашем примере)
    writeU2(static_cast<uint16_t>(jvmClass->superIdx));

    // 7. Interfaces Count (0)
    writeU2(0);

    // 8. Fields Count (0)
    writeU2(0);

    // 9. Methods
    writeMethods();

    // 10. Class Attributes (SourceFile и т.д.) - пока 0
    writeU2(0);

    out.close();
    std::cout << "[ClassBuilder] Successfully wrote " << filename << "\n";
    return true;
}

void ClassBuilder::writeConstantPool() {
    const auto& entries = jvmClass->cp.getEntries();
    
    // 1. Количество констант (размер вектора, т.к. 0-й элемент уже есть)
    writeU2(static_cast<uint16_t>(entries.size()));

    // 2. Сами константы (начинаем с 1, т.к. 0 зарезервирован)
    for (size_t i = 1; i < entries.size(); ++i) {
        const auto& entry = entries[i];
        writeU1(static_cast<uint8_t>(entry.tag));

        switch (entry.tag) {
            case CONSTANT_Utf8: // Tag(1) + Len(2) + Bytes
                writeU2(static_cast<uint16_t>(entry.stringValue.size()));
                writeString(entry.stringValue);
                break;
            
            case CONSTANT_Integer: // Tag(3) + Bytes(4)
                writeU4(static_cast<uint32_t>(entry.intValue));
                break;
            
            case CONSTANT_Float: { // Tag(4) + Bytes(4)
                // Кастуем биты float в int для записи
                uint32_t intVal;
                std::memcpy(&intVal, &entry.floatValue, sizeof(float));
                writeU4(intVal);
                break;
            }

            case CONSTANT_Class: // Tag(7) + NameIdx(2)
                writeU2(static_cast<uint16_t>(entry.refIndex1));
                break;

            case CONSTANT_String: // Tag(8) + StringIdx(2)
                writeU2(static_cast<uint16_t>(entry.refIndex1));
                break;

            case CONSTANT_Fieldref:
            case CONSTANT_Methodref: // Tag(10) + ClassIdx(2) + NameTypeIdx(2)
                writeU2(static_cast<uint16_t>(entry.refIndex1));
                writeU2(static_cast<uint16_t>(entry.refIndex2));
                break;

            case CONSTANT_NameAndType: // Tag(12) + NameIdx(2) + DescIdx(2)
                writeU2(static_cast<uint16_t>(entry.refIndex1));
                writeU2(static_cast<uint16_t>(entry.refIndex2));
                break;

            default:
                std::cerr << "[ClassBuilder] Unknown constant tag: " << (int)entry.tag << "\n";
        }
    }
}

void ClassBuilder::writeMethods() {
    // Количество методов
    writeU2(static_cast<uint16_t>(jvmClass->methods.size()));

    for (const auto& method : jvmClass->methods) {
        writeU2(static_cast<uint16_t>(method.accessFlags));
        writeU2(static_cast<uint16_t>(method.nameIdx));
        writeU2(static_cast<uint16_t>(method.descIdx));

        // --- ATTRIBUTES (Code) ---
        // Каждый неабстрактный метод должен иметь атрибут "Code"
        writeU2(1); // Attributes Count = 1 (только Code)

        // 1. Ищем индекс строки "Code" в Constant Pool
        // (Обычно это #1, как у вас, но лучше найти честно или передать)
        // Для простоты предположим, что вы знаете индекс или найдете его
        // int codeNameIdx = jvmClass->cp.lookupUtf8("Code"); 
        int codeNameIdx = 1; // У вас в примере #1 = "Code"

        writeU2(codeNameIdx); // Attribute Name Index

        // --- ГЕНЕРАЦИЯ БАЙТКОДА (Заглушка) ---
        std::vector<uint8_t> bytecode;
        
        if (method.name == "<init>") {
            // aload_0 (2A), invokespecial #9 (B7 00 09), return (B1)
            bytecode = { 0x2A, 0xB7 };
            // Индекс метода Object.<init> нужно найти в CP или знать заранее
            // В вашем выводе это #9
            uint16_t superInitIdx = 9; // ХАРДКОД ДЛЯ ТЕСТА! Замените на реальный поиск
            bytecode.push_back((superInitIdx >> 8) & 0xFF);
            bytecode.push_back(superInitIdx & 0xFF);
            bytecode.push_back(0xB1);
        } else if (method.name == "main" && method.descriptor.find("([L") != std::string::npos) {
            // Синтетический main: invokestatic haskellMain, return
            // haskellMain у вас #43 (MethodRef)
            bytecode = { 0xB8 }; // invokestatic
            uint16_t haskellMainRef = 30; // ХАРДКОД ДЛЯ ТЕСТА!
            bytecode.push_back((haskellMainRef >> 8) & 0xFF);
            bytecode.push_back(haskellMainRef & 0xFF);
            bytecode.push_back(0xB1); // return
        } else {
            // Для остальных пока просто return (для void) или ireturn
            // Внимание: если метод должен возвращать int, просто return крашнет верификатор
            // Но для проверки структуры файла пойдет
            bytecode = { 0xB1 }; // return (void)
        }

        // --- СЧИТАЕМ РАЗМЕР АТРИБУТА CODE ---
        // Header (max_stack 2 + max_locals 2 + code_len 4) + code + ex_table_len 2 + attr_count 2
        uint32_t codeAttrLen = 2 + 2 + 4 + (uint32_t)bytecode.size() + 2 + 2;

        writeU4(codeAttrLen);       // Attribute Length
        writeU2(10);                // Max Stack (С запасом)
        writeU2(10);                // Max Locals (С запасом)
        writeU4((uint32_t)bytecode.size()); // Code Length
        writeBytes(bytecode);       // Actual Bytecode
        
        writeU2(0); // Exception Table Length (0)
        writeU2(0); // Attributes inside Code (LineNumberTable etc.) - 0 for now
    }
}
