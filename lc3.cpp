/*
    @author yuanluo2
    @brief lc3 virtual machine written in C++11, for windows platform, learned from https://www.jmeiners.com/lc3-vm/
*/
#define WIN32_LEAN_AND_MEAN

#include <windows.h>
#include <iostream>
#include <fstream>
#include <string>
#include <array>
#include <cstdio>
#include <cstdint>

/*
    lc3 has 10 registers.

    8 general purpose registers: R0 ~ R7.
    program counter: PC.
    condition flag: COND.
*/
enum Register {
    REG_R0 = 0,
    REG_R1,
    REG_R2,
    REG_R3,
    REG_R4,
    REG_R5,
    REG_R6,
    REG_R7,
    REG_PC,
    REG_COND,
    REG_NUM
};

/*
    lc3 has 16 instructions. each instruction contains a operator and a set of parameters.
    see the instruction set at https://www.jmeiners.com/lc3-vm/supplies/lc3-isa.pdf Page 523
*/
enum Operator {
    OP_BR = 0,   // branch.
    OP_ADD,      // add.
    OP_LD,       // load.
    OP_ST,       // store.
    OP_JSR,      // jump register. `JSRR` shares this.
    OP_AND,      // bitwise and.
    OP_LDR,      // load register.
    OP_STR,      // store register.
    OP_RTI,      // unused.
    OP_NOT,      // bitwise not.
    OP_LDI,      // load indirect.
    OP_STI,      // store indirect.
    OP_JMP,      // jump. `RET` shares this.
    OP_RES,      // reserverd (unused).
    OP_LEA,      // load effective address.
    OP_TRAP      // execute trap.
};

/*
    lc3 has 3 condition flags: Negative, Zero, Positive, and they're ordered in N, Z, P
    this represent like:

    N Z P
    1 0 0  ->  0x4, negative. 
    0 1 0  ->  0x2, zero.
    0 0 1  ->  0x1, positive.
*/
enum CondFlag {
    COND_POSITIVE = 0x1,
    COND_ZERO     = 0x2,
    COND_NEGATIVE = 0x4
};

// lc3 has 6 trap routines in its own Trap Vector Table.
enum TrapRoutine {
    TRAP_GETC  = 0x20,   // get character from keyboard, not echoed onto the terminal.
    TRAP_OUT   = 0x21,   // output a character.
    TRAP_PUTS  = 0x22,   // output a word string of ASCII characters.
    TRAP_IN    = 0x23,   // get character from keyboard, echoed onto the terminal.
    TRAP_PUTSP = 0x24,   // output a byte string.
    TRAP_HALT  = 0x25    // halt the program.
};

// lc3 has 5 device registers.
enum DeviceReg {
    DEV_REG_KBSR = 0xfe00,   // keyboard status register, whether a key has been pressed.
    DEV_REG_KBDR = 0xfe02,   // keyboard data register, which key has been pressed.
    DEV_REG_DSR  = 0xfe04,   // display status register.
    DEV_REG_DDR  = 0xfe06,   // display data register.
    DEV_REG_MCR  = 0xfffe    // machine control register.
};

class ConsoleNotEchoed {
    HANDLE hIn;
    DWORD hInOldMode;
public:
    ConsoleNotEchoed() {
        hIn = GetStdHandle(STD_INPUT_HANDLE);
        
        // disable input buffering, this will make getchar() not echoed.
        GetConsoleMode(hIn, &hInOldMode);
        DWORD hInNewMode = hInOldMode ^ ENABLE_ECHO_INPUT ^ ENABLE_LINE_INPUT;
        SetConsoleMode(hIn, hInNewMode);

        FlushConsoleInputBuffer(hIn);
    }

    ~ConsoleNotEchoed() {
        SetConsoleMode(hIn, hInOldMode);
    }
};

class LC3 {
    std::array<uint16_t, 65536> memory;  // lc3 has 65536 addresses.
    std::array<uint16_t, REG_NUM> registers;  // lc3 has 10 registers.
    bool running;

    // lc3 is big-endian, but windows operating system is little-endian.
    uint16_t swap16(uint16_t x) {
        return (x << 8) | (x >> 8);
    }

    /*
        sign_extending() is needed when you want to extend a x-bit number to a 16-bit number.

        a positive number 5-bit 5 just adds '0':  0 0101  ->  0000 0000 0000 0101
        a negative number 5-bit -5 adds '1':      1 1011  ->  1111 1111 1111 1011
    */
    uint16_t sign_extending(uint16_t x, int bit_count) {
        if ((x >> (bit_count - 1)) & 1) {   // a 1 in the left-most bit indicates negative.
            x |= (0xffff << bit_count);
        }

        return x;
    }

    uint16_t read_memory(uint16_t address) {
        return memory[address];
    }

    void write_memory(uint16_t address, uint16_t value) {
        memory[address] = value;
    }

    void update_cond_flag(uint16_t reg) {
        if (registers[reg] == 0) {
            registers[REG_COND] = COND_ZERO;
        }
        else if (registers[reg] >> 15) {   // a 1 in the left-most bit indicates netagive.
            registers[REG_COND] = COND_NEGATIVE;
        }
        else {
            registers[REG_COND] = COND_POSITIVE;
        }
    }
    
    void op_br(uint16_t instruction) {
        uint16_t PCoffset9 = sign_extending(instruction & 0x1ff, 9);
        uint16_t cond = (instruction >> 9) & 0x7;

        if (cond & registers[REG_COND]) {
            registers[REG_PC] += PCoffset9;
        }
    }

    void op_add(uint16_t instruction) {
        uint16_t DR = (instruction >> 9) & 0x7;
        uint16_t SR1 = (instruction >> 6) & 0x7;
        uint16_t bit_5 = (instruction >> 5) & 0x1;

        if (bit_5) {   // immediate mode.
            uint16_t imm5 = sign_extending(instruction & 0x1f, 5);
            registers[DR] = registers[SR1] + imm5;
        }
        else {   // register mode.
            uint16_t SR2 = instruction & 0x7;
            registers[DR] = registers[SR1] + registers[SR2];
        }

        update_cond_flag(DR);
    }

    void op_ld(uint16_t instruction) {
        uint16_t DR = (instruction >> 9) & 0x7;
        uint16_t PCoffset9 = sign_extending(instruction & 0x1ff, 9);

        registers[DR] = read_memory(registers[REG_PC] + PCoffset9);
        update_cond_flag(DR);
    }

    void op_st(uint16_t instruction) {
        uint16_t SR = (instruction >> 9) & 0x7;
        uint16_t PCoffset9 = sign_extending(instruction & 0x1ff, 9);

        write_memory(registers[REG_PC] + PCoffset9, registers[SR]);
    }

    void op_jsr(uint16_t instruction) {
        registers[REG_R7] = registers[REG_PC];

        uint16_t bit_11 = (instruction >> 11) & 0x1;
        if (bit_11) {   // JSR.
            uint16_t PCoffset11 = sign_extending(instruction & 0x7ff, 11);
            registers[REG_PC] += PCoffset11;
        }
        else {   // JSRR.
            uint16_t BaseR = (instruction >> 6) & 0x7;
            registers[REG_PC] = registers[BaseR]; 
        }
    }

    void op_and(uint16_t instruction) {
        uint16_t DR = (instruction >> 9) & 0x7;
        uint16_t SR1 = (instruction >> 6) & 0x7;
        uint16_t bit_5 = (instruction >> 5) & 0x1;

        if (bit_5) {   // immediate mode.
            uint16_t imm5 = sign_extending(instruction & 0x1f, 5);
            registers[DR] = registers[SR1] & imm5;
        }
        else {   // register mode.
            uint16_t SR2 = instruction & 0x7;
            registers[DR] = registers[SR1] & registers[SR2];
        }

        update_cond_flag(DR);
    }

    void op_ldr(uint16_t instruction) {
        uint16_t offset6 = sign_extending(instruction & 0x3f, 6);
        uint16_t BaseR = (instruction >> 6) & 0x7;
        uint16_t DR = (instruction >> 9) & 0x7;

        registers[DR] = read_memory(registers[BaseR] + offset6);
        update_cond_flag(DR);
    }

    void op_str(uint16_t instruction) {
        uint16_t offset6 = sign_extending(instruction & 0x3f, 6);
        uint16_t BaseR = (instruction >> 6) & 0x7;
        uint16_t SR = (instruction >> 9) & 0x7;

        write_memory(registers[BaseR] + offset6, registers[SR]);
    }

    void op_not(uint16_t instruction) {
        uint16_t DR = (instruction >> 9) & 0x7;
        uint16_t SR = (instruction >> 6) & 0x7;

        registers[DR] = ~(registers[SR]);
        update_cond_flag(DR);
    }

    void op_ldi(uint16_t instruction) {
        uint16_t DR = (instruction >> 9) & 0x7;
        uint16_t PCoffset9 = sign_extending(instruction & 0x1ff, 9);
        uint16_t address = read_memory(registers[REG_PC] + PCoffset9);

        registers[DR] = read_memory(address);
        update_cond_flag(DR);
    }

    void op_sti(uint16_t instruction) {
        uint16_t SR = (instruction >> 9) & 0x7;
        uint16_t PCoffset9 = sign_extending(instruction & 0x1ff, 9);
        uint16_t address = read_memory(registers[REG_PC] + PCoffset9);

        write_memory(address, registers[SR]);
    }

    void op_jmp(uint16_t instruction) {
        // the `RET` also do this.
        uint16_t BaseR = (instruction >> 6) & 0x7;
        registers[REG_PC] = registers[BaseR];
    }

    void op_lea(uint16_t instruction) {
        uint16_t PCoffset9 = sign_extending(instruction & 0x1ff, 9);
        uint16_t DR = (instruction >> 9) & 0x7;

        registers[DR] = registers[REG_PC] + PCoffset9;
        update_cond_flag(DR);
    }

    void op_trap_getc() {
        registers[REG_R0] = (UINT16)getchar();
        update_cond_flag(REG_R0);
    }

    void op_trap_out() {
        putc((char)registers[REG_R0], stdout);
        fflush(stdout);
    }

    void op_trap_puts() {
        UINT16* ptr = memory.data() + registers[REG_R0];
        char c;
        
        while (*ptr) {
            c = (char)(*ptr);
            putc(c, stdout);
            ++ptr;
        }
        
        fflush(stdout);
    }

    void op_trap_in() {
        std::cout << "Enter a character: ";

        char c = getchar();
        putc(c, stdout);
        fflush(stdout);

        registers[REG_R0] = (UINT16)c;
        update_cond_flag(REG_R0);
    }

    void op_trap_putsp() {
        UINT16* ptr = memory.data() + registers[REG_R0];
        char c1, c2;

        while (*ptr) {
            c1 = (*ptr) & 0xff;
            c2 = (*ptr) >> 8;

            putc(c1, stdout);
            if (c2) {
                putc(c2, stdout);
            }

            ++ptr;
        }

        fflush(stdout);
    }

    void op_trap_halt() {
        running = false;

        std::cout << "\nprogram halt.\n";
        fflush(stdout);
    }

    bool op_trap(uint16_t instruction) {
        uint16_t trapvect8 = instruction & 0xff;
        registers[REG_R7] = registers[REG_PC];

        switch(trapvect8) {
            case TRAP_GETC:  op_trap_getc();  break;
            case TRAP_OUT:   op_trap_out();   break;
            case TRAP_PUTS:  op_trap_puts();  break;
            case TRAP_IN:    op_trap_in();    break;
            case TRAP_PUTSP: op_trap_putsp(); break;
            case TRAP_HALT:  op_trap_halt();  break;
            default:
                std::cerr << "unknown trap routine: " << trapvect8 << ", halt the program.\n";
                return false;
        }

        return true;
    }
public:
    LC3() : running{ true } {
        // PC starts from 0x3000.
        // The lower addresses are left empty to leave space for the trap routine code.
        registers[REG_PC] = 0x3000;
        registers[REG_COND] = COND_ZERO;
    }

    bool load_image_file(const std::string& imagePath) {
        std::ifstream in{ imagePath, std::ios::binary };
        if (!in.is_open()) {
            std::cerr << "can't open image file: " << imagePath << "\n";
            return false;
        }

        // in most case, origin is decimal 12288, which is 0x3000.
        uint16_t origin;
        in.read((char*)(&origin), sizeof(origin));
        origin = swap16(origin);

        // load the whole program data into the memory.
        uint16_t* ptr = memory.data() + origin;
        uint16_t maxProgramLen = memory.size() - origin;
        in.read((char*)ptr, maxProgramLen * sizeof(uint16_t));

        size_t bytes_read = in.gcount();
        while (bytes_read > 0) {
            *ptr = swap16(*ptr);

            ++ptr;
            bytes_read -= 2;   // uint16_t is 2 bytes.
        }

        return true;
    }

    bool execute() {
        uint16_t instruction;
        uint16_t op;

        while (running) {
            instruction = read_memory(registers[REG_PC]);
            ++registers[REG_PC];
            op = instruction >> 12;

            switch(op) {
                case OP_BR:  op_br(instruction);  break;
                case OP_ADD: op_add(instruction); break;
                case OP_LD:  op_ld(instruction);  break;
                case OP_ST:  op_st(instruction);  break;
                case OP_JSR: op_jsr(instruction); break;
                case OP_AND: op_and(instruction); break;
                case OP_LDR: op_ldr(instruction); break;
                case OP_STR: op_str(instruction); break;
                case OP_NOT: op_not(instruction); break;
                case OP_LDI: op_ldi(instruction); break;
                case OP_STI: op_sti(instruction); break;
                case OP_JMP: op_jmp(instruction); break;
                case OP_LEA: op_lea(instruction); break;
                case OP_TRAP:
                    if (!op_trap(instruction)) {
                        return false;
                    }

                    break;
                case OP_RTI:
                    std::cerr << "unsupported instruction: RTI, halt the program.\n";
                    return false;
                case OP_RES:
                    std::cerr << "unsupported instruction: reserved, halt the program.\n";
                    return false;
                default:
                    std::cerr << "unknown instruction: " << op << ", halt the program.\n";
                    return false;
            }
        }

        return true;
    }
};

/*
    g++ lc3.cpp -O3 -o lc3

    usage: lc3 your.obj

    lc3 assembler can download from: https://highered.mheducation.com/sites/0072467509/student_view0/lc-3_simulator.html
    this website provides a LC301.exe, it will give you a LC3 editor and a LC3 simulator. you can write the LC3 assembly program
    and use the editor to assemble it, then you would get a .obj file, which is the needed image file.
*/
int main(int argc, char* argv[]) {
    if (argc != 2) {
        std::cerr << "if you have a image file called your.obj, ";
        std::cerr << "then usage: " << argv[0] << " your.obj\n";
        return 1;
    }

    ConsoleNotEchoed cne;
    LC3 lc3;

    lc3.load_image_file(argv[1]);
    lc3.execute();
    return 0;
}
