/*
    @author yuanluo2
    @brief LC3 virtual machine written in ANSI C, works for windows platform. This program is learned from https://www.jmeiners.com/lc3-vm/
*/
#define WIN32_LEAN_AND_MEAN

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>

/*
    LC3 has 65536 memory addresses, each of which stores 16-bit value.
    so 65536 x 16 = 1048576 bit, which is 128 KB.
*/
#define MEMORY_ADDRESSES_NUM  65536

/*
    Program counter starts at 0x3000.
*/
#define PC_START  0x3000

/*
    LC3 has 10 registers, every one is 16-bit.

    8 general purpose registers: R0 ~ R7
    program counter: PC,
    condition flag: COND.
*/
#define REG_NUM  10

#define REG_0     0
#define REG_1     1
#define REG_2     2
#define REG_3     3
#define REG_4     4
#define REG_5     5
#define REG_6     6
#define REG_7     7
#define REG_PC    8
#define REG_COND  9

/*
    LC3 has 16 instructions. each instruction contains a `opcode` and a set of `parameters`.
    this is learned from: https://www.jmeiners.com/lc3-vm/supplies/lc3-isa.pdf A.3 The Instruction Set

    the op code are listed below, and they are orderd.
*/
#define OP_BR    0x0       /* branch. */
#define OP_ADD   0x1       /* add. */
#define OP_LD    0x2       /* load. */
#define OP_ST    0x3       /* store. */
#define OP_JSR   0x4       /* jump register. `JSRR` shares this op code. */
#define OP_AND   0x5       /* bitwise and. */
#define OP_LDR   0x6       /* load register. */
#define OP_STR   0x7       /* store register. */
#define OP_RTI   0x8       /* unused. */
#define OP_NOT   0x9       /* bitwise not. */
#define OP_LDI   0xa       /* load indirect. */
#define OP_STI   0xb       /* store indirect. */
#define OP_JMP   0xc       /* jump. `RET` shares this op code. */
#define OP_RES   0xd       /* reserved (unused). */
#define OP_LEA   0xe       /* load effective address. */
#define OP_TRAP  0xf       /* execute trap. */

/*
    LC3 has 3 condition flags: Negative, Zero, Positive, and they're ordered in N, Z, P
    this represent like:

    N Z P
    1 0 0  ->  0x4, negative. 
    0 1 0  ->  0x2, zero.
    0 0 1  ->  0x1, positive.
*/
#define COND_NEGATIVE  0x4
#define COND_ZERO      0x2
#define COND_POSITIVE   0x1

/*
    LC3 has 6 trap routines in its own Trap Vector Table.
*/
#define TRAP_GETC   0x20      /* get character from keyboard, not echoed onto the terminal. */
#define TRAP_OUT    0x21      /* output a character. */
#define TRAP_PUTS   0x22      /* output a word string of ASCII characters. */
#define TRAP_IN     0x23      /* get character from keyboard, echoed onto the terminal. */
#define TRAP_PUTSP  0x24      /* output a byte string. */
#define TRAP_HALT   0x25      /* halt the program. */

/*
    LC3 has 5 device registers.
*/
#define DEV_REG_KBSR  0xfe00      /* keyboard status register. whether a key has been pressed. */
#define DEV_REG_KBDR  0xfe02      /* keyboard data register. which key has been pressed. */
#define DEV_REG_DSR   0xfe04      /* display status register. */
#define DEV_REG_DDR   0xfe06      /* display data register. */
#define DEV_REG_MCR   0xfffe      /* machine control register. */

typedef struct {
    UINT16 memory[MEMORY_ADDRESSES_NUM];
    UINT16 registers[REG_NUM];
    DWORD inOldMode;   /* the old stdin console mode. */
    BOOL running;
} LC3;

LC3* lc3_create(void) {
    LC3* lc3 = (LC3*)malloc(sizeof(LC3));
    HANDLE hIn = GetStdHandle(STD_INPUT_HANDLE);
    DWORD inNewMode;

    /*
        PC starts from 0x3000.
        The lower addresses are left empty to leave space for the trap routine code
    */
    lc3->registers[REG_PC] = PC_START;
    lc3->registers[REG_COND] = COND_ZERO;
    lc3->running = TRUE;

    /* 
        disable input buffering, this will make getchar() not echoed.
    */
    GetConsoleMode(hIn, &(lc3->inOldMode)); /* save old mode */
    inNewMode = lc3->inOldMode ^ ENABLE_ECHO_INPUT ^ ENABLE_LINE_INPUT;
    SetConsoleMode(hIn, inNewMode);
    FlushConsoleInputBuffer(hIn);

    return lc3;
}

void lc3_destroy(LC3* lc3) {
    HANDLE hIn;

    if (lc3 != NULL) {
        hIn = GetStdHandle(STD_INPUT_HANDLE);
        SetConsoleMode(hIn, lc3->inOldMode);

        free(lc3);
    }
}

/*
    LC3 program is big-endian, but windows operating system is little-endian, so this conversion is needed.
*/
UINT16 swap16(UINT16 x) {
    return (x << 8) | (x >> 8);
}

/*
    sign_extending() is needed when you want to extend a x-bit number to a 16-bit number.

    a positive number 5-bit 5 just adds '0':  0 0101  ->  0000 0000 0000 0101
    a negative number 5-bit -5 adds '1':      1 1011  ->  1111 1111 1111 1011
*/
UINT16 sign_extending(UINT16 x, int bit_count) {
    if ((x >> (bit_count - 1)) & 1) {   /* a 1 in the left-most bit indicates negative. */
        x |= (0xffff << bit_count);
    }

    return x;
}

UINT16 lc3_read_memory(LC3* lc3, UINT16 address) {
    return lc3->memory[address];
}

void lc3_write_memory(LC3* lc3, UINT16 address, UINT16 value) {
    lc3->memory[address] = value;
}

void lc3_update_condition_flag(LC3* lc3, UINT16 r) {
    if (lc3->registers[r] == 0) {
        lc3->registers[REG_COND] = COND_ZERO;
    }
    else if (lc3->registers[r] >> 15) {   /* a 1 in the left-most bit indicates netagive. */
        lc3->registers[REG_COND] = COND_NEGATIVE;
    }
    else {
        lc3->registers[REG_COND] = COND_POSITIVE;
    }
}

void lc3_op_br(LC3* lc3, UINT16 instruction) {
    UINT16 PCoffset9 = sign_extending(instruction & 0x1ff, 9);
    UINT16 cond = (instruction >> 9) & 0x7;

    if (cond & lc3->registers[REG_COND]) {
        lc3->registers[REG_PC] += PCoffset9;
    }
}

void lc3_op_add(LC3* lc3, UINT16 instruction) {
    UINT16 DR = (instruction >> 9) & 0x7;
    UINT16 SR1 = (instruction >> 6) & 0x7;
    UINT16 bit_5 = (instruction >> 5) & 0x1;
    UINT16 SR2, imm5;

    if (bit_5) {   /* immediate mode. */
        imm5 = sign_extending(instruction & 0x1f, 5);
        lc3->registers[DR] = lc3->registers[SR1] + imm5;
    }
    else {   /* register mode. */
        SR2 = instruction & 0x7;
        lc3->registers[DR] = lc3->registers[SR1] + lc3->registers[SR2];
    }

    lc3_update_condition_flag(lc3, DR);
}

void lc3_op_ld(LC3* lc3, UINT16 instruction) {
    UINT16 DR = (instruction >> 9) & 0x7;
    UINT16 PCoffset9 = sign_extending(instruction & 0x1ff, 9);

    lc3->registers[DR] = lc3_read_memory(lc3, lc3->registers[REG_PC] + PCoffset9);
    lc3_update_condition_flag(lc3, DR);
}

void lc3_op_st(LC3* lc3, UINT16 instruction) {
    UINT16 SR = (instruction >> 9) & 0x7;
    UINT16 PCoffset9 = sign_extending(instruction & 0x1ff, 9);

    lc3_write_memory(lc3, lc3->registers[REG_PC] + PCoffset9, lc3->registers[SR]);
}

void lc3_op_jsr(LC3* lc3, UINT16 instruction) {
    UINT16 bit_11 = (instruction >> 11) & 0x1;
    UINT16 PCoffset11, BaseR;

    lc3->registers[REG_7] = lc3->registers[REG_PC];

    if (bit_11) {   /* JSR. */
        PCoffset11 = sign_extending(instruction & 0x7ff, 11);
        lc3->registers[REG_PC] += PCoffset11;
    }
    else {   /* JSRR. */
        BaseR = (instruction >> 6) & 0x7;
        lc3->registers[REG_PC] = lc3->registers[BaseR]; 
    }
}

void lc3_op_and(LC3* lc3, UINT16 instruction) {
    UINT16 DR = (instruction >> 9) & 0x7;
    UINT16 SR1 = (instruction >> 6) & 0x7;
    UINT16 bit_5 = (instruction >> 5) & 0x1;
    UINT16 SR2, imm5;

    if (bit_5) {   /* immediate mode. */
        imm5 = sign_extending(instruction & 0x1f, 5);
        lc3->registers[DR] = lc3->registers[SR1] & imm5;
    }
    else {   /* register mode. */
        SR2 = instruction & 0x7;
        lc3->registers[DR] = lc3->registers[SR1] & lc3->registers[SR2];
    }

    lc3_update_condition_flag(lc3, DR);
}

void lc3_op_ldr(LC3* lc3, UINT16 instruction) {
    UINT16 offset6 = sign_extending(instruction & 0x3f, 6);
    UINT16 BaseR = (instruction >> 6) & 0x7;
    UINT16 DR = (instruction >> 9) & 0x7;

    lc3->registers[DR] = lc3_read_memory(lc3, lc3->registers[BaseR] + offset6);
    lc3_update_condition_flag(lc3, DR);
}

void lc3_op_str(LC3* lc3, UINT16 instruction) {
    UINT16 offset6 = sign_extending(instruction & 0x3f, 6);
    UINT16 BaseR = (instruction >> 6) & 0x7;
    UINT16 SR = (instruction >> 9) & 0x7;

    lc3_write_memory(lc3, lc3->registers[BaseR] + offset6, lc3->registers[SR]);
}

void lc3_op_not(LC3* lc3, UINT16 instruction) {
    UINT16 DR = (instruction >> 9) & 0x7;
    UINT16 SR = (instruction >> 6) & 0x7;

    lc3->registers[DR] = ~(lc3->registers[SR]);
    lc3_update_condition_flag(lc3, DR);
}

void lc3_op_ldi(LC3* lc3, UINT16 instruction) {
    UINT16 DR = (instruction >> 9) & 0x7;
    UINT16 PCoffset9 = sign_extending(instruction & 0x1ff, 9);

    UINT16 address = lc3_read_memory(lc3, lc3->registers[REG_PC] + PCoffset9);
    lc3->registers[DR] = lc3_read_memory(lc3, address);
    lc3_update_condition_flag(lc3, DR);
}

void lc3_op_sti(LC3* lc3, UINT16 instruction) {
    UINT16 SR = (instruction >> 9) & 0x7;
    UINT16 PCoffset9 = sign_extending(instruction & 0x1ff, 9);
    UINT16 address = lc3_read_memory(lc3, lc3->registers[REG_PC] + PCoffset9);

    lc3_write_memory(lc3, address, lc3->registers[SR]);
}

void lc3_op_jmp(LC3* lc3, UINT16 instruction) {
    /* the `RET` also do this. */
    UINT16 BaseR = (instruction >> 6) & 0x7;
    lc3->registers[REG_PC] = lc3->registers[BaseR];
}

void lc3_op_lea(LC3* lc3, UINT16 instruction) {
    UINT16 PCoffset9 = sign_extending(instruction & 0x1ff, 9);
    UINT16 DR = (instruction >> 9) & 0x7;

    lc3->registers[DR] = lc3->registers[REG_PC] + PCoffset9;
    lc3_update_condition_flag(lc3, DR);
}

void lc3_op_trap_getc(LC3* lc3) {
    lc3->registers[REG_0] = (UINT16)getchar();
    lc3_update_condition_flag(lc3, REG_0);
}

void lc3_op_trap_out(LC3* lc3) {
    putc((char)lc3->registers[REG_0], stdout);
    fflush(stdout);
}

void lc3_op_trap_puts(LC3* lc3) {
    UINT16* ptr = lc3->memory + lc3->registers[REG_0];
    char c;
    
    while (*ptr) {
        c = (char)(*ptr);
        putc(c, stdout);
        ++ptr;
    }
    
    fflush(stdout);
}

void lc3_op_trap_in(LC3* lc3) {
    char c;

    printf("Enter a character: ");
    c = getchar();
    putc(c, stdout);
    fflush(stdout);

    lc3->registers[REG_0] = (UINT16)c;
    lc3_update_condition_flag(lc3, REG_0);
}

void lc3_op_trap_putsp(LC3* lc3) {
    UINT16* ptr = lc3->memory + lc3->registers[REG_0];
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

void lc3_op_trap_halt(LC3* lc3) {
    printf("\nprogram halt.\n");
    
    fflush(stdout);
    lc3->running = FALSE;
}

BOOL lc3_op_trap(LC3* lc3, UINT16 instruction) {
    UINT16 trapvect8 = instruction & 0xff;
    lc3->registers[REG_7] = lc3->registers[REG_PC];

    switch(trapvect8) {
        case TRAP_GETC:
            lc3_op_trap_getc(lc3);
            break;
        case TRAP_OUT:
            lc3_op_trap_out(lc3);
            break;
        case TRAP_PUTS:
            lc3_op_trap_puts(lc3);
            break;
        case TRAP_IN:
            lc3_op_trap_in(lc3);
            break;
        case TRAP_PUTSP:
            lc3_op_trap_putsp(lc3);
            break;
        case TRAP_HALT:
            lc3_op_trap_halt(lc3);
            break;
        default:
            fprintf(stderr, "unknown trap routine: %d, halt the program.\n", trapvect8);
            return FALSE;
    }

    return TRUE;
}

BOOL lc3_execute_program(LC3* lc3) {
    UINT16 instruction;
    UINT16 op;

    while (lc3->running) {
        instruction = lc3_read_memory(lc3, lc3->registers[REG_PC]);
        lc3->registers[REG_PC] += 1;
        op = instruction >> 12;

        switch(op) {
            case OP_BR:
                lc3_op_br(lc3, instruction);
                break;
            case OP_ADD:
                lc3_op_add(lc3, instruction);
                break;
            case OP_LD:
                lc3_op_ld(lc3, instruction);
                break;
            case OP_ST:
                lc3_op_st(lc3, instruction);
                break;
            case OP_JSR:
                lc3_op_jsr(lc3, instruction);
                break;
            case OP_AND:
                lc3_op_and(lc3, instruction);
                break;
            case OP_LDR:
                lc3_op_ldr(lc3, instruction);
                break;
            case OP_STR:
                lc3_op_str(lc3, instruction);
                break;
            case OP_NOT:
                lc3_op_not(lc3, instruction);
                break;
            case OP_LDI:
                lc3_op_ldi(lc3, instruction);
                break;
            case OP_STI:
                lc3_op_sti(lc3, instruction);
                break;
            case OP_JMP:
                lc3_op_jmp(lc3, instruction);
                break;
            case OP_LEA:
                lc3_op_lea(lc3, instruction);
                break;
            case OP_TRAP:
                if (!lc3_op_trap(lc3, instruction)) {
                    return FALSE;
                }

                break;
            case OP_RTI:
                fprintf(stderr, "unsupported instruction: RTI, halt the program.\n");
                return FALSE;
            case OP_RES:
                fprintf(stderr, "unsupported instruction: reserved, halt the program.\n");
                return FALSE;
            default:
                fprintf(stderr, "unknown instruction: %d, halt the program.\n", op);
                return FALSE;
        }
    }

    return TRUE;
}

BOOL lc3_load_image_file(LC3* lc3, const char* imageFile) {
    UINT16 origin, programLength;
    UINT16* ptr;
    size_t readLen;

    FILE* f = fopen(imageFile, "rb");
    if (f == NULL) {
        fprintf(stderr, "can't open file: %s\n", imageFile);
        return FALSE;
    }

    fread(&origin, sizeof(origin), 1, f);
    origin = swap16(origin);
    programLength = MEMORY_ADDRESSES_NUM - origin;

    /* read the whole program. */
    ptr = lc3->memory + origin;
    readLen = fread(ptr, sizeof(UINT16), programLength, f);

    while (readLen > 0) {
        *ptr = swap16(*ptr);

        ++ptr;
        --readLen;
    }

    fclose(f);
    return TRUE;
}

/*
    gcc lc3.c -O3 -o lc3

    usage: lc3 your.obj

    lc3 assembler can download from: https://highered.mheducation.com/sites/0072467509/student_view0/lc-3_simulator.html
    this website provides a LC301.exe, it will give you a LC3 editor and a LC3 simulator. you can write the LC3 assembly program
    and use the editor to assemble it, then you would get a .obj file, which is the needed image file.
*/
int main(int argc, char* argv[]) {
    LC3* lc3;

    if (argc != 2) {
        fprintf(stderr, "if you have a image file called your.obj, then usage: %s your.obj\n", argv[0]);
        return 1;
    }

    lc3 = lc3_create();

    if (lc3_load_image_file(lc3, argv[1])) {
        lc3_execute_program(lc3);
    }

    lc3_destroy(lc3);
    return 0;
}
