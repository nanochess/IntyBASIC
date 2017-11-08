//
//  microcode.cpp
//  This module keeps assembler instructions generated...
//  ...by IntyBASIC in a list ready to be output.
//
//  Created by Oscar Toledo on 07/01/16.
//  Copyright (c) 2016 Oscar Toledo. All rights reserved.
//

#include <string>

using namespace std;

#include "global.h"
#include "microcode.h"

static const char *opcode_list[] = {
    "ADCR", "ADD", "ADD@", "ADDI", "ADDR", "AND", "AND@", "ANDI", "ANDR",
    "B", "BC", "BEQ", "BGE", "BGT", "BLE", "BLT", "BMI", "BNC", "BNE", "BPL",
    "CALL", "CLRC", "CLRR", "CMP", "CMP@", "CMPI", "CMPR", "COMR", "DECLE", "DECR",
    "INCR", "MOVR", "MVI", "MVI@", "MVII", "MVO", "MVO@", "MULT", "NEGR", "NOP",
    "PSHR", "PULR", "RRC", "RETURN", "RSWD",
    "SARC", "SLL", "SLR", "SUB", "SUB@", "SUBI", "SUBR", "SWAP",
    "TSTR", "XOR", "XOR@", "XORI", "XORR",
};

//
// Saves microcode
//
microcode::microcode(enum microcode_style style, int type, int r1, int r2, string prefix, int value, int offset)
{
    this->style = style;
    this->type = type;
    this->r1 = r1;
    this->r2 = r2;
    this->prefix = prefix;
    this->value = value;
    this->offset = offset;
}

//
// Get type of microcode
//
int microcode::get_type(void)
{
    return this->type;
}

//
// Get register 1
//
int microcode::get_r1(void)
{
    return this->r1;
}

//
// Get register 2
//
int microcode::get_r2(void)
{
    return this->r2;
}

//
// Get prefix
//
string microcode::get_prefix(void)
{
    return this->prefix;
}

//
// Get value
//
int microcode::get_value(void)
{
    return this->value;
}

//
// Generate assembler code for a microcode
//
void microcode::dump(void) {
    switch (this->style) {
        case M_SINGLE:  // Single opcode: NOP
            asm_output << "\t" << opcode_list[this->type];
            break;
        case M_R:  // Single register: CLRR R0
            asm_output << "\t" << opcode_list[this->type] << " R" << this->r1;
            break;
        case M_RR:  // Double register: MOVR R0,R1
            asm_output << "\t" << opcode_list[this->type] << " R" << this->r1 << ",";
            if (this->r2 == 7)
                asm_output << "PC";
            else
                asm_output << "R" << this->r2;
            break;
        case M_NR:  // Constant: MVII #5,R0
            asm_output << "\t" << opcode_list[this->type] << " #";
            if (this->prefix == "")
                asm_output << this->value;
            else
                asm_output << this->prefix << this->value;
            if (this->offset > 0)
                asm_output << "+" << this->offset;
            else if (this->offset < 0)
                asm_output << "-" << (-this->offset);
            asm_output << ",R" << this->r1;
            break;
        case M_NNR: // Constant: ADDI #label-label,R0
            asm_output << "\t" << opcode_list[this->type] << " #(";
            asm_output << this->prefix << this->value;
            asm_output << "-";
            asm_output << this->prefix << this->r2;
            if (this->offset > 0)
                asm_output << "+" << this->offset;
            else if (this->offset < 0)
                asm_output << "-" << (-this->offset);
            asm_output << ") AND $FFFF,R" << this->r1;
            // Note how the AND in expression solves a bug when substraction creates a big negative
            // number, triggering an error in as1600
            break;
        case M_LR:  // Label and register: MVI V2,R0
            asm_output << "\t" << opcode_list[this->type] << " ";
            if (this->prefix == "") {
                asm_output << this->value;
            } else if (this->value == -1) {
                asm_output << this->prefix;
            } else {
                asm_output << this->prefix << this->value;
            }
            if (this->offset)
                asm_output << "+" << this->offset;
            asm_output << ",R" << this->r1;
            break;
        case M_RL:  // Register and label: MVO R0,V2
            asm_output << "\t" << opcode_list[this->type] << " R" << this->r1 << ",";
            if (this->prefix == "")
                asm_output << this->value;
            else if (this->value == -1)
                asm_output << this->prefix;
            else
                asm_output << this->prefix << this->value;
            if (this->offset > 0)
                asm_output << "+" << this->offset;
            else if (this->offset < 0)
                asm_output << "-" << (-this->offset);
            break;
        case M_A:  // Address (jumps): CALL CLRSCR
            asm_output << "\t" << opcode_list[this->type] << " ";
            if (this->prefix == "")
                asm_output << "$+" << this->value;
            else if (this->value == -1)
                asm_output << this->prefix;
            else
                asm_output << this->prefix << this->value;
            break;
        case M_S:  // Shifts: SLL R0,1
            asm_output << "\t" << opcode_list[this->type] << " R" << this->r1 << "," << this->value;
            break;
        case M_M:  // Multiply (macro): MULT R0,R4,5
            asm_output << "\t" << opcode_list[this->type] << " R" << this->r1 << ",R" << this->r2 << "," << this->value;
            break;
        case M_L:  // Label
            asm_output << this->prefix << this->value << ":";
            break;
        case M_D:  // Single word of data
            asm_output << "\t" << opcode_list[this->type] << " " << this->r1;
            break;
        case M_D2:  // Double word of data
            asm_output << "\t" << opcode_list[this->type] << " " << this->r1 << "," << this->r2;
            break;
        case M_DL:  // Label as data
            asm_output << "\t" << opcode_list[this->type] << " ";
            if (this->value == -1)
                asm_output << this->prefix;
            else
                asm_output << this->prefix << this->value;
            if (this->offset)
                asm_output << "+" << this->offset;
            break;
        case M_LITERAL:  // Literal assembler code
            asm_output << this->prefix;
            break;
    }
    asm_output << "\n";
}
