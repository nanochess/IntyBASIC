//
//  microcode.h
//  intybasic
//
//  Created by Oscar Toledo on 07/01/16.
//  Copyright (c) 2016 Oscar Toledo. All rights reserved.
//

#ifndef _intybasic_microcode_
#define _intybasic_microcode_

enum opcode {
    N_ADCR, N_ADD, N_ADDA, N_ADDI, N_ADDR, N_AND, N_ANDA, N_ANDI, N_ANDR,
    N_B, N_BC, N_BEQ, N_BGE, N_BGT, N_BLE, N_BLT, N_BMI, N_BNC, N_BNE, N_BPL,
    N_CALL, N_CLRC, N_CLRR, N_CMP, N_CMPA, N_CMPI, N_CMPR, N_COMR, N_DECLE, N_DECR,
    N_INCR, N_MOVR, N_MVI, N_MVIA, N_MVII, N_MVO, N_MVOA, N_MULT, N_NEGR, N_NOP,
    N_PSHR, N_PULR, N_RRC, N_RETURN, N_RSWD,
    N_SARC, N_SLL, N_SLR, N_SUB, N_SUBA, N_SUBI, N_SUBR, N_SWAP,
    N_TSTR, N_XOR, N_XORA, N_XORI, N_XORR
};

enum microcode_style {
    M_SINGLE, M_R, M_RR, M_NR, M_NNR, M_LR, M_RL,
    M_A, M_S, M_M, M_L, M_D, M_D2, M_DL, M_LITERAL
};

//
// Microcode structure and output to assembler
//
class microcode {
    enum microcode_style style;
    int type;
    int r1;
    int r2;
    string prefix;
    int value;
    int offset;
        
public:
    microcode(enum microcode_style style, int type, int r1, int r2, string prefix, int value, int offset);
    int get_type(void);
    int get_r1(void);
    int get_r2(void);
    string get_prefix(void);
    int get_value(void);
    void dump(void);
};

#endif /* defined(_intybasic_microcode_) */
