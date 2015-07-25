//
//  main.cpp
//  intybasic
//
//  Created by Oscar Toledo on 13/01/14.
//  Copyright (c) 2014 Oscar Toledo. All rights reserved.
//
//  Revision: Jan/14/2014. Small expression tree creator and code generator.
//  Revision: Jan/15/2014. Added more lexical components to be used. FOR/NEXT ready.
//  Revision: Jan/16/2014. Added AND/OR/XOR/NOT, negation and comparison operators.
//  Revision: Jan/17/2014. Added IF/ELSE, POKE, PEEK, mult/div/mod operators.
//  Revision: Jan/21/2014. Now puts PROC in same line as label. First working version :)
//  Revision: Jan/22/2014. Added CLS, WAIT, CONT0/CONT1. Debugged code generator.
//                         Added COL0-7, FRAME, RAND, RESTORE, READ and DATA.
//  Revision: Jan/27/2014. Added SOUND, SPRITE, PRINT, ASM, BITMAP. Integrated constant
//                         expression optimization. Further debugging of statements.
//	Revision: Jan/28/2014. Modified to be compilable under Visual C++ 2008. Relational
//                         expression optimization in IF. Support for 16-bits variables
//                         using hashtag as start of name. Solved bug where name was
//                         wrongly taken as label.
//  Revision: Feb/02/2014. Supports _ and . as zero in BITMAP.
//  Revision: Feb/03/2014. Removed warning when compiling in Linux.
//  Revision: Feb/04/2014. Solved bug when using NEXT with variable name. Now every
//                         comment and variable in English.
//  Revision: Feb/06/2014. Implemented indexed access to DATA and DIM for putting
//                         arrays in RAM memory. Solved bug where extra characters
//                         in line were not detected. Added memory limit detection.
//  Revision: Feb/11/2014. Solved bug in assignation to array with constant index.
//  Revision: Feb/26/2014. Added SCROLL and BORDER statements. Support for library
//                         path in arguments.
//  Revision: Mar/02/2014. Solved bug where FOR STEP would go beyond admisible value.
//                         Support for constants definition.
//  Revision: Mar/21/2014. Support to obtain character codes of GROM in expressions.
//  Revision: Apr/02/2014. Added MODE and SCREEN statements. Re-designed .b0-2 for
//                         true detection of controller buttons. Added .KEY syntax.
//                         Changed result of comparisons, now $ffff (-1) to ease use
//                         of NOT.
//  Revision: Jul/09/2014. Warns if unable to open prologue/epilogue files. Shows
//                         total variable space used if exceeds available space.
//  Revision: Aug/26/2014. Support for binary numbers. Added PLAY and MUSIC statements
//                         and NTSC variable.
//  Revision: Sep/12/2014. Corrected bug where SCREEN label was lost if variable used
//                         as origin offset.
//  Revision: Oct/09/2014. DEFINE now allows expressions in its first two parameters.
//                         Optimization for multiplication and division by 256, also
//                         for modulus with 32, 64, 128 and 256. It's implemented the
//                         ON GOTO/GOSUB statement. Added STACK_CHECK statement.
//  Revision: Oct/10/2014. Added fixed numbers and fractional add and substract
//                         using operator +. and -. Added INCLUDE statement.
//  Revision: Oct/16/2014. Warns if PROCEDURE is started without ending previous one.
//                         Warns if END is used without PROCEDURE. Removes extra
//                         RETURN instruction if the statement combination RETURN/END
//                         is used. MUSIC alone doesn't activate tracker module.
//  Revision: Nov/13/2014. Optimizes substraction to obviate need for register
//                         interchange, optimizes cases a+(-b) and a-(-b). PRINT now
//                         generates smaller code. Added ABS function. READ now can
//                         be used with array variables. Optimizes IF code generation
//                         Optimizes equal and non-equal comparison. Added support
//                         for printing numbers with PRINT (using intvnut routines)
//  Revision: Nov/17/2014. Added special comments in output proposed by intvnut to
//                         ease source code debugging and SRCFILE directive.
//                         Redesigned MODE to use a single variable.
//  Revision: Nov/18/2014. Supports hardware acceleration of multiplication and
//                         division operators.
//  Revision: Nov/19/2014. Added support for extra memory if using --jlp flag.
//  Revision: Nov/20/2014. Added USR function. Added DEFINE ALTERNATE.
//  Revision: Nov/21/2014. Added support for Intellivoice with VOICE.
//  Revision: Dec/11/2014. Updated number of variable availables (off by 1 both in
//                         8 and 16 bits variables). Statements DEFINE, SCREEN,
//                         PLAY and VOICE allow to use arrays as reference. DEFINE
//                         allows use of VARPTR to select between various definitions.
//                         Optimized PEEK (also arrays) and PRINT code generation.
//  Revision: Dec/12/2014. Support for DZ-Jay's macro for constant multiplication.
//                         Optimizes constant addition/substraction trees.
//  Revision: Jan/25/2015. Added support for Cuttle Cart 3 (no hardware acceleration
//                         but extra RAM). Added support for changing title in
//                         compiled code. Added support for CONT.* (checks both
//                         controllers at same time). Added support for ON FRAME GOSUB.
//  Revision: Feb/17/2015. Added PLAY NONE statement. Returns error code if happens a
//                         failure in compilation. It doesn't take some statements as a
//                         label if followed by colon. Added support for SGN function.
//                         Added WHILE/WEND.
//  Revision: Mar/05/2015. Added support for ECS secondary PSG.
//  Revision: Mar/11/2015. Solved bug in variable counting for 8-bits variables and in
//                         free space calculation for 16-bits variables.
//  Revision: Mar/12/2015. Solved another bug in free variable counting for 8-bits
//                         variables (was 16 less than possible)
//  Revision: Mar/23/2015. Slight acceleration in PRINT for spaces.
//  Revision: Apr/21/2015. Using SOUND 5-9 activates ECS specific code. Added support
//                         for modulus for all 2 powers. Added support for multiply by
//                         32 and 64. Added support for division by 32, 64 and 128.
//  Revision: Apr/22/2015. Now uses fast multiply routine (intvnut routine)
//  Revision: Apr/23/2015. Solved bug where IF containing RETURN was misoptimized.
//                         Added RAND() where right side can contain range. Added
//                         support for multiplication by 128.
//  Revision: May/14/2015. Start of rebuilt code output (not directly to std::cout but
//                         via class code)
//  Revision: Jun/04/2015. Solved bug where ECS code was always included because
//                         variable not initialized.
//  Revision: Jun/26/2015. Optimized code generation for reversed operand order.
//                         Solved bug in multiply operation where zero operand would
//                         cause 65536 cycles in common case. Enhanced code for
//                         multiply.
//  Revision: Jul/02/2015. Adjustment in class structure, now working again. Added
//                         peephole optimization for several common cases. Generates
//                         warnings for variables assigned but not read, or read but
//                         not assigned.
//  Revision: Jul/03/2015. Replaced SAR with SLR in constant division in order to
//                         make standard the unsigned division, also when both
//                         operands are constant. Peephole optimization NEGR/ADD to
//                         SUB/NEGR, and also NEGR/NEGR COMR/COMR. MVO with direct
//                         addressing is limited to two instructions. Peephole
//                         optimization for TSTR. Peephole optimization for jump
//                         conditional over jump absolute. Optimized multiplication
//                         by 128. Used ADDR for some cases of multiplication by
//                         power of two.
//  Revision: Jul/04/2015. Added some guard code for extreme cases per intvnut
//                         comments :). Simple subexpression optimization for
//                         index into array (common case a(x)=a(x) op expr) also
//                         for index plus offset. Avoids labels when generating
//                         boolean results to preserve registers contents in
//                         optimizer.
//  Revision: Jul/06/2015. More comments. RANDOM implemented.
//  Revision: Jul/08/2015. Replace succesive addition multiplication with faster
//                         shift algorithm. Solved bug in ASM statement when
//                         putting out labels (as1600 needs no space before).
//  Revision: Jul/10/2015. Solved bug where internal division/modulus by zero
//                         was possible (intvnut). Added support for my fast
//                         division/remainder routine. Implemented DEF FN as macro
//                         processor.
//  Revision: Jul/11/2015. Doesn't require FN to call macro. Added support for
//                         quotes in INCLUDE. Improvement in multiplication code
//                         courtesy of intvnut. Tries to locate INCLUDE files in
//                         library path.
//  Revision: Jul/13/2015. Implemented CONT3 and CONT4 (ECS). FN now can be called
//                         as statement. New #MOBSHADOW array allows to access
//                         MOB shadow buffer. Redesigned FOR for taking advantage
//                         of optimizer.
//  Revision: Jul/25/2015. Changed some flags to bool type. Some warnings changed
//                         to errors. Warnings now can be disable with option -w.
//

//  TODO:
//  Nothing :)

//  Development notes:
//  * Careful with everything.back(), behavior not defined when list is empty so
//    can trigger segmentation faults if not checking for everything.size() > 0

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <list>
#include <cctype>
#include <cstddef>
#include <cstring>
#include <ctime>

using namespace std;

const string VERSION = "v1.2 Jul/25/2015";      // Compiler version
const string LABEL_PREFIX = "Q";    // Prefix for BASIC labels
const string TEMP_PREFIX = "T";     // Prefix for temporal labels
const string VAR_PREFIX = "V";      // Prefix for BASIC variables
const string FUNC_PREFIX = "F";     // Prefix for USR functions

ofstream asm_output;
class code *output;
int next_local = 1;
bool optimized;      // Indicates if expression for IF statement jump was optimized
bool jlp_used;       // Indicates if JLP is used
bool cc3_used;       // Indicates if CC3 is used
bool fastmult_used;  // Indicates if fast multiplication is used
bool fastdiv_used;   // Indicates if fast division/remainder is used
bool warnings;       // Indicates if warnings are generated
int program_year;
char program_title[256];
int err_code;

enum opcode {
    N_ADCR, N_ADD, N_ADDA, N_ADDI, N_ADDR, N_AND, N_ANDA, N_ANDI, N_ANDR,
    N_B, N_BC, N_BEQ, N_BGE, N_BGT, N_BLE, N_BLT, N_BNC, N_BNE, N_BPL,
    N_CALL, N_CLRC, N_CLRR, N_CMP, N_CMPA, N_CMPI, N_CMPR, N_COMR, N_DECLE, N_DECR,
    N_INCR, N_MOVR, N_MVI, N_MVIA, N_MVII, N_MVO, N_MVOA, N_MULT, N_NEGR, N_NOP,
    N_PSHR, N_PULR, N_RRC, N_RETURN, N_SARC, N_SLL, N_SLR, N_SUB, N_SUBA, N_SUBI, N_SUBR, N_SWAP,
    N_TSTR, N_XOR, N_XORA, N_XORI, N_XORR,
};

static const char *opcode_list[] = {
    "ADCR", "ADD", "ADD@", "ADDI", "ADDR", "AND", "AND@", "ANDI", "ANDR",
    "B", "BC", "BEQ", "BGE", "BGT", "BLE", "BLT", "BNC", "BNE", "BPL",
    "CALL", "CLRC", "CLRR", "CMP", "CMP@", "CMPI", "CMPR", "COMR", "DECLE", "DECR",
    "INCR", "MOVR", "MVI", "MVI@", "MVII", "MVO", "MVO@", "MULT", "NEGR", "NOP",
    "PSHR", "PULR", "RRC", "RETURN", "SARC", "SLL", "SLR", "SUB", "SUB@", "SUBI", "SUBR", "SWAP",
    "TSTR", "XOR", "XOR@", "XORI", "XORR",
};

enum microcode_style {
    M_SINGLE, M_R, M_RR, M_NR, M_LR, M_RL,
    M_A, M_S, M_M, M_L, M_D, M_D2, M_DL, M_LITERAL,
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
    //
    // Saves microcode
    //
    microcode(enum microcode_style style, int type, int r1, int r2, string prefix, int value, int offset)
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
    int get_type(void)
    {
        return this->type;
    }
    
    //
    // Get register 1
    //
    int get_r1(void)
    {
        return this->r1;
    }
    
    //
    // Get register 2
    //
    int get_r2(void)
    {
        return this->r2;
    }
    
    //
    // Get prefix
    //
    string get_prefix(void)
    {
        return this->prefix;
    }
    
    //
    // Get value
    //
    int get_value(void)
    {
        return this->value;
    }
    
    //
    // Generate assembler code for a microcode
    //
    void dump(void) {
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
                break;
            case M_LITERAL:  // Literal assembler code
                asm_output << this->prefix;
                break;
        }
        asm_output << "\n";
    }
};

//
// Intermediate code generator with peephole optimization
//
class code {
    
    list <class microcode *> everything;
    
    int cycles;
    
    // Relates which registers contains what constants
    struct {
        int valid;
        string prefix;
        int value;
        int offset;
    } register_content[8];
    
    bool subexpression_valid;
    int subexpression_base;
    int subexpression_offset;
    int subexpression_index;
    
    // Relates which registers are in which memory locations
    struct {
        int valid;
        string prefix;
        int value;
        int offset;
    } register_memory[8];
    
public:
    // Startup of class code
    code(void) {
        trash_registers();
    }
    
    // Checks for subexpression available (currently only in R3)
    bool subexpression_available(int base, int offset, int index) {
        if (subexpression_valid && subexpression_base == base && subexpression_offset == offset && subexpression_index == index)
            return true;
        return false;
    }
    
    // Annotate for subexpression available (currently only in R3)
    void annotate_subexpression(int base, int offset, int index) {
        if (subexpression_valid)
            return;
        subexpression_valid = true;
        subexpression_base = base;
        subexpression_offset = offset;
        subexpression_index = index;
    }
    
    // Trash registers
    void trash_registers(void) {
        int c;
        
        for (c = 0; c < 8; c++) {
            register_content[c].valid = 0;
            register_memory[c].valid = 0;
        }
        subexpression_valid = false;
    }
    
    // Check if enough cycles for non-interruptable sequence of instructions
    void check_for_cycles(int how_many, int limit) {
        if (cycles + how_many >= limit) {
            everything.push_back(new microcode(M_SINGLE, N_NOP, 0, 0, "", 0, 0));
            cycles = 0;
        }
        cycles += how_many;
    }
    
    // Emits an instruction with no operands
    void emit(enum opcode type) {
        everything.push_back(new microcode(M_SINGLE, type, 0, 0, "", 0, 0));
        cycles = 0;
    }
    
    // Emits instruction with single register operand
    void emit_r(enum opcode type, int r1) {
        
        if (everything.size() > 0) {
            class microcode *previous;
            int c;
            
            previous = everything.back();
            c = previous->get_type();
            
            // Common pattern optimization for self nullifying instructions
            if (type == N_NEGR && c == N_NEGR && previous->get_r1() == r1) {
                everything.pop_back();
                delete previous;
                return;
            }
            if (type == N_COMR && c == N_COMR && previous->get_r1() == r1) {
                everything.pop_back();
                delete previous;
                return;
            }
            if (type == N_TSTR && (c == N_ADD || c == N_ADDA || c == N_ADDI
                                   || c == N_AND || c == N_ANDA || c == N_ANDI
                                   || c == N_SUB || c == N_SUBA || c == N_SUBI
                                   || c == N_XOR || c == N_XORA || c == N_XORI
                                   || c == N_INCR || c == N_DECR
                                   || c == N_COMR || c == N_NEGR
                                   || c == N_SARC || c == N_SLL || c == N_SLR || c == N_RRC
                                   || c == N_CLRR || c == N_SWAP)
                && previous->get_r1() == r1) {
                // There is no need to insert TSTR
                return;
            }
            if (type == N_TSTR && (c == N_ADDR || c == N_ANDR || c == N_SUBR || c == N_XORR || c == N_MOVR)
                && previous->get_r2() == r1) {
                // There is no need to insert TSTR
                return;
            }
        }
                                   
        // Common pattern optimization for zero in register (via CLRR)
        if (type == N_CLRR && register_content[r1].valid != 0 && register_content[r1].prefix == "" && register_content[r1].value == 0 && register_content[r1].offset == 0) {
            // Nothing to do =P
            return;
        }
        everything.push_back(new microcode(M_R, type, r1, 0, "", 0, 0));
        if (type == N_CLRR) {  // Annotate new available constant (zero)
            register_content[r1].valid = 1;
            register_content[r1].prefix = "";
            register_content[r1].value = 0;
            register_content[r1].offset = 0;
        } else {
            register_content[r1].valid = 0;
        }
        register_memory[r1].valid = 0;
        if (r1 == 3)
            subexpression_valid = false;
        cycles = 0;
    }
    
    // Emits instruction with register operands
    void emit_rr(enum opcode type, int r1, int r2) {
        
        // Common pattern optimization: MVI@ followed by ADDR/ANDR/CMPR/SUBR/XORR
        if ((type == N_ADDR || type == N_ANDR || type == N_CMPR || type == N_SUBR || type == N_XORR) && everything.size() > 0) {
            class microcode *previous;
        
            previous = everything.back();
            if (previous->get_type() == N_MVIA && previous->get_r2() == r1) {
                if (type == N_ADDR)
                    type = N_ADDA;
                else if (type == N_ANDR)
                    type = N_ANDA;
                else if (type == N_CMPR)
                    type = N_CMPA;
                else if (type == N_SUBR)
                    type = N_SUBA;
                else if (type == N_XORR)
                    type = N_XORA;
                r1 = previous->get_r1();
                everything.pop_back();
                delete previous;
            }
        }
        if (type == N_MVOA)
            check_for_cycles(9, 45);
        everything.push_back(new microcode(M_RR, type, r1, r2, "", 0, 0));
        if (type != N_CMPR && type != N_MVOA) {
            register_content[r2].valid = 0;
            register_memory[r2].valid = 0;
            if (r2 == 3)
                subexpression_valid = false;
        }
        if (type == N_MVIA && r2 == 7)  // Jump table
            trash_registers();
    }
    
    // Emits instruction with constant/address in left side
    void emit_nr(enum opcode type, string prefix, int value, int r) {
        emit_nor(type, prefix, value, 0, r);
    }
    
    // Emits instruction with constant/address plus offset in left side
    void emit_nor(enum opcode type, string prefix, int value, int offset, int r) {
        int c;
        int d;
        
        // Common pattern optimization: modulus plus number
        if (type == N_ADDI && prefix == "" && everything.size() > 0) {
            class microcode *previous;
            
            previous = everything.back();
            if (previous->get_type() == N_ADDI && previous->get_r1() == r && previous->get_prefix() == "") {
                value += previous->get_value();
                everything.pop_back();
                delete previous;
            }
        }
        
        // Common pattern optimization: constant in register
        for (c = 0; c < 4; c++) {
            d = (r + c) % 4;
            if (type == N_MVII && register_content[d].valid != 0 && register_content[d].prefix == prefix && register_content[d].value == value && register_content[d].offset == offset) {
                if (d == r) {
                    // Nothing to do =P
                } else {
                    everything.push_back(new microcode(M_RR, N_MOVR, d, r, "", 0, 0));
                    register_content[r].valid = 1;
                    register_content[r].prefix = prefix;
                    register_content[r].value = value;
                    register_content[r].offset = offset;
                    if (r == 3)
                        subexpression_valid = false;
                }
                return;
            }
        }
        everything.push_back(new microcode(M_NR, type, r, 0, prefix, value, offset));
        if (type == N_CMPI) {
            // CMPI doesn't change register so still valid
        } else if (type == N_MVII) {
            register_content[r].valid = 1;
            register_content[r].prefix = prefix;
            register_content[r].value = value;
            register_content[r].offset = offset;
            register_memory[r].valid = 0;
            if (r == 3)
                subexpression_valid = false;
        } else {
            register_content[r].valid = 0;
            register_memory[r].valid = 0;
            if (r == 3)
                subexpression_valid = false;
        }
        cycles = 0;
    }

    // Emits instruction with label in left side
    void emit_lr(enum opcode type, string prefix, int value, int r) {
        emit_lor(type, prefix, value, 0, r);
    }

    // Emits instruction with label plus offset in left side
    void emit_lor(enum opcode type, string prefix, int value, int offset, int r) {
        int c;
        int d;
        class microcode *previous;
        
        // Common optimization case: register just saved to memory and still available
        for (c = 0; c < 4; c++) {
            d = (r + c) % 4;
            if (register_memory[d].valid != 0 && register_memory[d].prefix == prefix && register_memory[d].value == value && register_memory[d].offset == offset) {
                if (type == N_ADD) {
                    type = N_ADDR;
                    everything.push_back(new microcode(M_RR, type, d, r, "", 0, 0));
                    register_memory[r].valid = 0;
                    if (r == 3)
                        subexpression_valid = false;
                } else if (type == N_AND) {
                    type = N_ANDR;
                    everything.push_back(new microcode(M_RR, type, d, r, "", 0, 0));
                    register_memory[r].valid = 0;
                    if (r == 3)
                        subexpression_valid = false;
                } else if (type == N_CMP) {
                    type = N_CMPR;
                    everything.push_back(new microcode(M_RR, type, d, r, "", 0, 0));
                    // Note register is still valid
                } else if (type == N_SUB) {
                    type = N_SUBR;
                    everything.push_back(new microcode(M_RR, type, d, r, "", 0, 0));
                    register_memory[r].valid = 0;
                    if (r == 3)
                        subexpression_valid = false;
                } else if (type == N_XOR) {
                    type = N_XORR;
                    everything.push_back(new microcode(M_RR, type, d, r, "", 0, 0));
                    register_memory[r].valid = 0;
                    if (r == 3)
                        subexpression_valid = false;
                } else {
                    if (d == r) {
                        // Nothing to do =P
                    } else {
                        everything.push_back(new microcode(M_RR, N_MOVR, d, r, "", 0, 0));
                        register_memory[r].valid = 1;
                        register_memory[r].prefix = register_memory[d].prefix;
                        register_memory[r].value = register_memory[d].value;
                        register_memory[r].offset = register_memory[d].offset;
                        if (r == 3)
                            subexpression_valid = false;
                    }
                }
                return;
            }
        }
        previous = everything.size() > 0 ? everything.back() : NULL;
        if (type == N_ADD && previous && previous->get_type() == N_NEGR && previous->get_r1() == r) {
            everything.pop_back();
            delete previous;
            type = N_SUB;
            everything.push_back(new microcode(M_LR, type, r, 0, prefix, value, offset));
            everything.push_back(new microcode(M_R, N_NEGR, r, 0, "", 0, 0));
        } else {
            everything.push_back(new microcode(M_LR, type, r, 0, prefix, value, offset));
        }
        if (type == N_CMP) {
            // register still valid
        } else if (type == N_ADD || type == N_AND || type == N_CMP || type == N_SUB || type == N_XOR) {
            register_content[r].valid = 0;
            register_memory[r].valid = 0;
            if (r == 3)
                subexpression_valid = false;
        } else {
            register_content[r].valid = 0;
            register_memory[r].valid = 1;
            register_memory[r].prefix = prefix;
            register_memory[r].value = value;
            register_memory[r].offset = offset;
            if (r == 3)
                subexpression_valid = false;
        }
        cycles = 0;
    }
    
    // Emits instruction with label in right side
    void emit_rl(enum opcode type, int r, string prefix, int value) {
        emit_rlo(type, r, prefix, value, 0);
    }

    // Emits instruction with label in right side (to 8 bits memory)
    void emit_rlo8(enum opcode type, int r, string prefix, int value, int offset) {
        if (type == N_MVO)
            check_for_cycles(11, 23);
        everything.push_back(new microcode(M_RL, type, r, 0, prefix, value, offset));
        register_memory[r].valid = 0;  // Not valid because: cuts upper 8 bits
        subexpression_valid = false;  // Possibly wrote index variable
    }
    
    // Emits instruction with label plus offset in right side
    void emit_rlo(enum opcode type, int r, string prefix, int value, int offset) {
        if (type == N_MVO)
            check_for_cycles(11, 23);
        everything.push_back(new microcode(M_RL, type, r, 0, prefix, value, offset));
        register_memory[r].valid = 1;
        register_memory[r].prefix = prefix;
        register_memory[r].value = value;
        register_memory[r].offset = offset;
        subexpression_valid = false;  // Possibly wrote index variable
    }
    
    // Emits instruction with single label operand
    void emit_a(enum opcode type, string prefix, int value) {
        class microcode *previous;
        int is_zero;
        
        is_zero = 0;
        if (type == N_BNE && everything.size() > 0) {
            previous = everything.back();
            if (previous->get_type() == N_TSTR)
                is_zero = 1;
        }
        everything.push_back(new microcode(M_A, type, 0, 0, prefix, value, 0));
        if (is_zero) {  // Common optimization: assume register contains zero
            register_content[previous->get_r1()].valid = 1;
            register_content[previous->get_r1()].prefix = "";
            register_content[previous->get_r1()].value = 0;
            register_content[previous->get_r1()].offset = 0;
        } else if (type == N_B || type == N_CALL) {
            trash_registers();
        }
        cycles = 0;
    }
    
    // Emits shift instruction
    void emit_s(enum opcode type, int r, int s) {
        if (s == 1)
            check_for_cycles(6, 45);
        else if (s == 2)
            check_for_cycles(8, 45);
        everything.push_back(new microcode(M_S, type, r, 0, "", s, 0));
        register_content[r].valid = 0;
        register_memory[r].valid = 0;
        if (r == 3)
            subexpression_valid = false;
    }
    
    // Emits multiply instruction (macro)
    void emit_m(enum opcode type, int r1, int r2, int v) {
        everything.push_back(new microcode(M_M, type, r1, r2, "", v, 0));
        register_content[r1].valid = 0;
        register_memory[r1].valid = 0;
        if (r1 == 3)
            subexpression_valid = false;
        cycles = 0;
    }
    
    // Emits label
    void emit_l(string prefix, int value) {
        everything.push_back(new microcode(M_L, 0, 0, 0, prefix, value, 0));
        trash_registers();
        cycles = 0;
    }
    
    // Emits label guaranteed to be IF and used only one time
    void emit_l3(string prefix, int value) {
        class microcode *previous;
        class microcode *previous2;
        int c;

        if (everything.size() > 0) {
            previous = everything.back();
            if (previous->get_type() == N_B) {
                everything.pop_back();
                previous2 = everything.back();
                c = previous2->get_type();
                if ((c == N_BEQ || c == N_BNE || c == N_BLT || c == N_BGT || c == N_BLE || c == N_BGE) && previous2->get_prefix() == prefix && previous2->get_value() == value) {
                    everything.pop_back();
                    delete previous2;
                    if (c == N_BEQ)
                        c = N_BNE;
                    else if (c == N_BNE)
                        c = N_BEQ;
                    else if (c == N_BLT)
                        c = N_BGE;
                    else if (c == N_BGT)
                        c = N_BLE;
                    else if (c == N_BLE)
                        c = N_BGT;
                    else if (c == N_BGE)
                        c = N_BLT;
                    everything.push_back(new microcode(M_A, c, 0, 0, previous->get_prefix(), previous->get_value(), 0));
                    delete previous;
                    cycles = 0;
                    // Small annoyance: register state cannot be restored (destroyed after inserting B)
                    // In case of restoring it, take in account that is_zero of emit_a wouldn't be valid
                    return;
                }
                everything.push_back(previous);
            }
        }
        everything.push_back(new microcode(M_L, 0, 0, 0, prefix, value, 0));
        trash_registers();
        cycles = 0;
    }
    
    // Emits data
    void emit_d(enum opcode type, int d) {
        everything.push_back(new microcode(M_D, type, d, 0, "", 0, 0));
        cycles = 0;
    }
    
    // Emits data (2 words)
    void emit_d2(enum opcode type, int d1, int d2) {
        everything.push_back(new microcode(M_D2, type, d1, d2, "", 0, 0));
        cycles = 0;
    }
    
    // Emits data (label)
    void emit_dl(enum opcode type, string prefix, int value) {
        everything.push_back(new microcode(M_DL, type, 0, 0, prefix, value, 0));
        cycles = 0;
    }
    
    // Emits literal assembler (user provided)
    void emit_literal(string text) {
        everything.push_back(new microcode(M_LITERAL, 0, 0, 0, text, 0, 0));
        trash_registers();
        cycles = 0;
    }
    
    // Dump microcode as assembler
    void dump(void)
    {
        class microcode *current;
        
        while (everything.size() > 0) {
            current = everything.front();
            everything.pop_front();
            if (current == NULL)
                break;
            current->dump();
            delete current;
        }
    }
};

// Lexical components
enum lexical_component {C_END, C_NAME, C_NAME_R, C_NAME_RO,
    C_STRING, C_LABEL, C_NUM,
    C_OR, C_XOR, C_AND, C_NOT, C_NEG, C_PEEK, C_ABS, C_SGN,
    C_READ, C_VAR, C_USR, C_RAND, C_RANDOM,
    C_ASSIGN,
    C_EQUAL, C_NOTEQUAL, C_LESS, C_LESSEQUAL, C_GREATER, C_GREATEREQUAL,
    C_PLUS, C_MINUS, C_PLUSF, C_MINUSF, C_MUL, C_DIV, C_MOD,
    C_LPAREN, C_RPAREN, C_COLON, C_PERIOD, C_COMMA,
    C_ERR};

//#define DEBUG_FN

#if defined(DEBUG_FN)
// For debugging purposes
static const char *lexical_names[] = {
    "[end]", "[name]", "[name_r]", "[name_ro]",
    "[string]", "[label]", "[num]",
    "OR", "XOR", "AND", "NOT", "NEG", "PEEK", "ABS", "SGN",
    "READ", "VAR", "USR", "RAND", "RANDOM",
    "[assign]",
    "=", "<>", "<", "<=", ">", ">=",
    "+", "-", "+.", "-.", "*", "/", "%",
    "(", ")", ":", ".", ",",
    "[err]",
};
#endif

//
// Expression tree builder
//
class node {
    enum lexical_component type;
    int value;
    int regs;
    class node *left;
    class node *right;
    
public:

    //
    // Builds an expression node
    //
    node(enum lexical_component type, int value, class node *left, class node *right) {
        this->type = type;
        this->value = value;
        this->regs = 0;
        this->left = left;
        this->right = right;
        
        // Optimizes trees of addition/substraction operators
        if (type == C_PLUS && right->type == C_NUM && left->type == C_MINUS && left->right->type == C_NUM) {
            right->value -= left->right->value;
            this->left = left->left;
            left->left = NULL;
            delete left;
            left = this->left;
        }
        if (type == C_PLUS && left->type == C_NUM && right->type == C_MINUS && right->right->type == C_NUM) {
            left->value -= right->right->value;
            this->right = right->left;
            right->left = NULL;
            delete right;
            right = this->right;
        }
        if (type == C_PLUS && right->type == C_NUM && left->type == C_PLUS && left->right->type == C_NUM) {
            right->value += left->right->value;
            this->left = left->left;
            left->left = NULL;
            delete left;
            left = this->left;
        }
        if (type == C_PLUS && left->type == C_NUM && right->type == C_PLUS && right->right->type == C_NUM) {
            left->value += right->right->value;
            this->right = right->left;
            right->left = NULL;
            delete right;
            right = this->right;
        }
        if (type == C_PLUS && right->type == C_NUM && left->type == C_PLUS && left->left->type == C_NUM) {
            right->value += left->left->value;
            this->left = left->right;
            left->right = NULL;
            delete left;
            left = this->left;
        }
        if (type == C_PLUS && left->type == C_NUM && right->type == C_PLUS && right->left->type == C_NUM) {
            left->value += right->left->value;
            this->right = right->right;
            right->right = NULL;
            delete right;
            right = this->right;
        }
        // Optimize common array access case a(c+1)
        if (type == C_PLUS && left->type == C_NAME_RO && right->type == C_PLUS && right->right->type == C_NUM) {
            class node *temp;
            
            // Pass constant to left side (in order to be added to address in one instruction)
            temp = right->left;
            right->left = left;
            this->left = right;
            this->right = temp;
        }
        // Optimize common array access case a(c-1)
        if (type == C_PLUS && left->type == C_NAME_RO && right->type == C_MINUS && right->right->type == C_NUM) {
            class node *temp;
            
            // Pass constant to left side (in order to be added to address in one instruction)
            temp = right->left;
            right->type = C_PLUS;
            right->right->value = - right->right->value;
            right->left = left;
            this->left = right;
            this->right = temp;
        }
        // Optimizes constant expressions
        if (type == C_PLUS && left->type == C_NUM && right->type == C_NUM) {
            this->type = C_NUM;
            this->value = left->value + right->value;
            delete this->left;
            this->left = NULL;
            delete this->right;
            this->right = NULL;
        }
        if (type == C_MINUS && left->type == C_NUM && right->type == C_NUM) {
            this->type = C_NUM;
            this->value = left->value - right->value;
            delete this->left;
            this->left = NULL;
            delete this->right;
            this->right = NULL;
        }
        if (type == C_MUL) {
            if (left->type == C_NUM && right->type == C_NUM) {
                this->type = C_NUM;
                this->value = left->value * right->value;
                delete this->left;
                this->left = NULL;
                delete this->right;
                this->right = NULL;
            } else if (left->type == C_NUM) {
                this->left = right;
                this->right = left;
            }
        }
        if (type == C_DIV && left->type == C_NUM && right->type == C_NUM && (right->value & 0xffff) != 0) {
            this->type = C_NUM;
            this->value = (left->value & 0xffff) / (right->value & 0xffff);
            delete this->left;
            this->left = NULL;
            delete this->right;
            this->right = NULL;
        }
        if (type == C_MOD && left->type == C_NUM && right->type == C_NUM && (right->value & 0xffff) != 0) {
            this->type = C_NUM;
            this->value = (left->value & 0xffff) % (right->value & 0xffff);
            delete this->left;
            this->left = NULL;
            delete this->right;
            this->right = NULL;
        }
        if (type == C_AND && left->type == C_NUM && right->type == C_NUM) {
            this->type = C_NUM;
            this->value = left->value & right->value;
            delete this->left;
            this->left = NULL;
            delete this->right;
            this->right = NULL;
        }
        if (type == C_OR && left->type == C_NUM && right->type == C_NUM) {
            this->type = C_NUM;
            this->value = left->value | right->value;
            delete this->left;
            this->left = NULL;
            delete this->right;
            this->right = NULL;
        }
        if (type == C_XOR && left->type == C_NUM && right->type == C_NUM) {
            this->type = C_NUM;
            this->value = left->value ^ right->value;
            delete this->left;
            this->left = NULL;
            delete this->right;
            this->right = NULL;
        }
        if (type == C_NOT && left->type == C_NUM) {
            this->type = C_NUM;
            this->value = ~left->value;
            delete this->left;
            this->left = NULL;
        }
        if (type == C_NEG && left->type == C_NUM) {
            this->type = C_NUM;
            this->value = -left->value;
            delete this->left;
            this->left = NULL;
        }
        if (type == C_ABS && left->type == C_NUM) {
            this->type = C_NUM;
            this->value = ((left->value & 0xffff) >= 0x8000) ? 0x10000 - (left->value & 0xffff) : left->value;
            delete this->left;
            this->left = NULL;
        }
        if (type == C_SGN && left->type == C_NUM) {
            this->type = C_NUM;
            this->value = (left->value & 0xffff) ? (((left->value & 0xffff) >= 0x8000) ? 0xFFFF : 1) : 0;
            delete this->left;
            this->left = NULL;
        }
    }
    
    //
    // Destructor for node
    //
    ~node() {
        if (left) {
            delete left;
            left = NULL;
        }
        if (right) {
            delete right;
            right = NULL;
        }
    }
    
    //
    // Get node type
    //
    enum lexical_component node_type(void) {
        return type;
    }
    
    //
    // Get node value
    //
    int node_value(void) {
        return value;
    }
    
    //
    // Set right node
    //
    void set_right(class node *right) {
        this->right = right;
    }
    
    //
    // Check for valid array with or without offset
    //
    bool valid_array(void) {
        if (this->type == C_NAME_RO)
            return true;
        if (this->type == C_PLUS && left->type == C_NAME_RO && right->type == C_NUM)
            return true;
        return false;
    }
    
    //
    // Analyze each node and calculate number of registers used
    //
    void label(void) {
        
        // Terminal nodes
        if (left == NULL && right == NULL) {
            regs = 1;
            return;
        }
        
        // Unary nodes
        if (right == NULL) {
            left->label();
            if (type == C_USR || type == C_RAND || type == C_RANDOM)
                regs = 10;
            else
                regs = left->regs;
            return;
        }
        
        // Binary nodes
        left->label();
        right->label();
        
        if (right->type == C_NUM || right->type == C_NAME) {
            regs = left->regs;
        } else {
            
            // Optimize case a+(-b) to a-b
            if (type == C_PLUS) {
                if (right->type == C_NEG) {
                    class node *temp;
                    
                    temp = right;
                    right = right->left;
                    temp->left = NULL;
                    temp->right = NULL;
                    delete temp;
                    type = C_MINUS;
                }
            }
            // Optimize case a-(-b) to a+b
            if (type == C_MINUS) {
                if (right->type == C_NEG) {
                    class node *temp;
                    
                    temp = right;
                    right = right->left;
                    temp->left = NULL;
                    temp->right = NULL;
                    delete temp;
                    type = C_PLUS;
                }
            }
            // Optimize heavy case a-b
            if (type == C_MINUS && left->regs < right->regs) {
                class node *temp;
                
                temp = new node(C_NEG, 0, right, NULL);
                temp->regs = right->regs;
                right = temp;
                type = C_PLUS;
            }
            // Switch arguments for addition operators in order to optimize them
            if (type == C_PLUS || type == C_PLUSF) {
                if (right->type != C_NUM && right->type != C_NAME
                    && (left->type == C_NUM || left->type == C_NAME)) {
                    class node *temp;
                    
                    temp = left;
                    left = right;
                    right = temp;
                }
            }
            if (type == C_MUL && left->type == C_NAME && right->type == C_NAME && !jlp_used)
                regs = 10;
            else if (type == C_DIV && left->type == C_NAME && right->type == C_NAME && !jlp_used)
                regs = 10;
            else if (type == C_MOD && left->type == C_NAME && right->type == C_NAME && !jlp_used)
                regs = 10;
            else if (left->regs > right->regs)
                regs = left->regs;
            else if (right->regs > left->regs)
                regs = right->regs;
            else
                regs = left->regs + 1;
        }
    }
    
    //
    // Generates code for an expression tree
    //
    void generate(int reg, int decision) {
        if (reg >= 4) {
            std::cerr << "Too complex expression\n";
            err_code = 1;
            return;
        }
        switch (type) {
            default:        // Non-defined node, never should happen
                output->emit_literal("\t; >>> Houston, we have a problem <<<");
                break;
            case C_NUM:     // Number
                if ((value & 0xffff) == 0)
                    output->emit_r(N_CLRR, reg);
                else
                    output->emit_nr(N_MVII, "", value & 0xffff, reg);
                break;
            case C_NAME:    // Variable
                output->emit_lr(N_MVI, VAR_PREFIX, value, reg);
                break;
            case C_NAME_R:    // Access to variable address
                output->emit_nr(N_MVII, VAR_PREFIX, value, reg);
                break;
            case C_NAME_RO: // Access to label
                output->emit_nr(N_MVII, LABEL_PREFIX, value, reg);
                break;
            case C_READ:    // Read from DATA
                output->emit_lr(N_MVI, "_read", -1, 4);
                output->emit_rr(N_MVIA, 4, reg);
                output->emit_rl(N_MVO, 4, "_read", -1);
                break;
            case C_USR:     // Call user's function
            {
                int reg;
               
                reg = 0;
                for (class node *explore = left; explore != NULL; explore = explore->right) {
                    explore->left->generate(reg, 0);
                    reg++;
                }
                output->emit_a(N_CALL, FUNC_PREFIX, value);
            }
                if (reg)
                    output->emit_rr(N_MOVR, 0, reg);
                break;
            case C_RAND:    // Generate random number in range
                left->generate(0, 0);
                output->emit_lr(N_MVI, "_rand", -1, 1);
                output->emit_a(N_CALL, "qs_mpy8", -1);
                output->emit_r(N_SWAP, 0);
                output->emit_nr(N_ANDI, "", 0x00ff, 0);
                if (reg)
                    output->emit_rr(N_MOVR, 0, reg);
                fastmult_used = true;
                break;
            case C_RANDOM:    // Generate random number in range
                if (left->type == C_NUM && (left->value == 2 || left->value == 4 || left->value == 8 || left->value == 16 || left->value == 32 || left->value == 64 || left->value == 128 || left->value == 256)) {
                    output->emit_a(N_CALL, "_next_random", -1);
                    output->emit_nr(N_ANDI, "", left->value - 1, 0);
                    if (reg)
                        output->emit_rr(N_MOVR, 0, reg);
                } else {
                    left->generate(1, 0);
                    output->emit_a(N_CALL, "_next_random", -1);
                    output->emit_a(N_CALL, "qs_mpy8", -1);
                    output->emit_r(N_SWAP, 0);
                    output->emit_nr(N_ANDI, "", 0x00ff, 0);
                    if (reg)
                        output->emit_rr(N_MOVR, 0, reg);
                    fastmult_used = true;
                }
                break;
            case C_NOT:     // NOT
                left->generate(reg, 0);
                output->emit_r(N_COMR, reg);
                break;
            case C_NEG:     // Negation
                left->generate(reg, 0);
                output->emit_r(N_NEGR, reg);
                break;
            case C_VAR:     // Special variable
                if (value == 0)
                    output->emit_lr(N_MVI, "_col0", -1, reg);
                if (value == 1)
                    output->emit_lr(N_MVI, "_col1", -1, reg);
                if (value == 2)
                    output->emit_lr(N_MVI, "_col2", -1, reg);
                if (value == 3)
                    output->emit_lr(N_MVI, "_col3", -1, reg);
                if (value == 4)
                    output->emit_lr(N_MVI, "_col4", -1, reg);
                if (value == 5)
                    output->emit_lr(N_MVI, "_col5", -1, reg);
                if (value == 6)
                    output->emit_lr(N_MVI, "_col6", -1, reg);
                if (value == 7)
                    output->emit_lr(N_MVI, "_col7", -1, reg);
                if (value == 8)
                    output->emit_lr(N_MVI, "_frame", -1, reg);
                if (value == 9)
                    output->emit_lr(N_MVI, "_rand", -1, reg);
                if (value == 10)
                    output->emit_lr(N_MVI, "_cnt1_key", -1, reg);
                if (value == 11)
                    output->emit_lr(N_MVI, "_cnt2_key", -1, reg);
                if (value == 12)
                    output->emit_lr(N_MVI, "_ntsc", -1, reg);
                if (value == 13) {  // Read both controllers
                    output->emit_lr(N_MVI, "", 0x01fe, reg);
                    output->emit_lr(N_XOR, "", 0x01ff, reg);
                }
                if (value == 14) {  // Read both controllers keypad
                    output->emit_lr(N_MVI, "_cnt1_key", -1, reg);
                    output->emit_nr(N_CMPI, "", 12, reg);
                    output->emit_a(N_BNE, "", 4);   // two words of jump and two words of MVI
                    output->emit_lr(N_MVI, "_cnt2_key", -1, reg);
                }
                break;
            case C_PEEK:    // PEEK()
                if (left->type == C_NUM) {  // Peek directly from memory location
                    output->emit_lr(N_MVI, "", left->value, reg);
                } else if (left->type == C_PLUS && left->left->type == C_NAME_RO && left->right->type == C_NUM) {  // Constant index into array
                    output->emit_lor(N_MVI, LABEL_PREFIX, left->left->value, left->right->value, reg);
                } else if (left->type == C_PLUS && left->left->valid_array() && left->right->type == C_NAME) {  // Simple index into array
                    if (left->left->type == C_NAME_RO) {  // Without offset
                        if (!output->subexpression_available(left->left->value, 0, left->right->value)) {
                            left->generate(3, 0);
                            output->annotate_subexpression(left->left->value, 0, left->right->value);
                        }
                    } else {  // With offset into array
                        if (!output->subexpression_available(left->left->left->value, left->left->right->value, left->right->value)) {
                            left->generate(3, 0);
                            output->annotate_subexpression(left->left->left->value, left->left->right->value, left->right->value);
                        }
                    }
                    output->emit_rr(N_MVIA, 3, reg);
                } else {
                    if (reg == 0) {  // R0 cannot be used as source of address
                        left->generate(reg + 1, 0);
                        output->emit_rr(N_MVIA, reg + 1, reg);
                    } else {
                        left->generate(reg, 0);
                        output->emit_rr(N_MVIA, reg, reg);
                    }
                }
                break;
            case C_ABS:    // ABS()
                left->generate(reg, 0);
                output->emit_r(N_TSTR, reg);
                output->emit_a(N_BPL, "", 3);  // two words of jump and one word of NEGR
                output->emit_r(N_NEGR, reg);
                break;
            case C_SGN:    // SGN()
                left->generate(reg, 0);
                output->emit_r(N_TSTR, reg);
                output->emit_a(N_BEQ, "", 7);  // 2 two words of jump and two words of MVI
                output->emit_nr(N_MVII, "", 1, reg);    // 2
                output->emit_a(N_BPL, "", 3);   // 2 two words of jump and one word of NEGR
                output->emit_r(N_NEGR, reg);    // 1
                break;
            case C_ASSIGN:
            case C_PLUS:
            case C_MINUS:
            case C_PLUSF:
            case C_MINUSF:
            case C_AND:
            case C_XOR:
            case C_OR:
            case C_EQUAL:
            case C_NOTEQUAL:
            case C_LESS:
            case C_LESSEQUAL:
            case C_GREATER:
            case C_GREATEREQUAL:
            case C_MUL:
            case C_DIV:
            case C_MOD:
                
                // Ultra-special cases
                if (type == C_MUL && left->type == C_NAME && right->type == C_NAME && !jlp_used) {
                    left->generate(0, 0);
                    right->generate(1, 0);
                    output->emit_a(N_CALL, "qs_mpy16", -1);
                    if (reg != 1) {
                        output->emit_rr(N_MOVR, 1, reg);
                    }
                    fastmult_used = true;
                } else if (type == C_DIV && left->type == C_NAME && right->type == C_NAME && !jlp_used) {
                    left->generate(0, 0);
                    right->generate(1, 0);
                    output->emit_a(N_CALL, "uf_udiv16", -1);
                    if (reg != 0) {
                        output->emit_rr(N_MOVR, 0, reg);
                    }
                    fastdiv_used = true;
                } else if (type == C_MOD && left->type == C_NAME && right->type == C_NAME && !jlp_used) {
                    left->generate(0, 0);
                    right->generate(1, 0);
                    output->emit_a(N_CALL, "uf_udiv16", -1);
                    if (reg != 2) {
                        output->emit_rr(N_MOVR, 2, reg);
                    }
                    fastdiv_used = true;
                // Get address of array with constant index
                } else if (type == C_PLUS && left->type == C_NAME_RO && right->type == C_NUM) {
                    output->emit_nor(N_MVII, LABEL_PREFIX, left->value, right->value, reg);
                // Optimization for assignation to array with simple index
                } else if (type == C_ASSIGN && right->type == C_PLUS && right->left->valid_array() && right->right->type == C_NAME) {
                    left->generate(0, 0);
                    if (right->left->type == C_NAME_RO) {
                        if (!output->subexpression_available(right->left->value, 0, right->right->value)) {
                            right->generate(3, 0);
                            output->annotate_subexpression(right->left->value, 0, right->right->value);
                        }
                    } else {
                        if (!output->subexpression_available(right->left->left->value, right->left->right->value, right->right->value)) {
                            right->generate(3, 0);
                            output->annotate_subexpression(right->left->left->value, right->left->right->value, right->right->value);
                        }
                    }
                    output->emit_rr(N_MVOA, 0, 3);
                // Optimize right side when it's constant
                } else if (right->type == C_NUM && type != C_ASSIGN) {
                    left->generate(reg, 0);
                    if (type == C_PLUS) {
                        if ((right->value & 0xffff) == 1)
                            output->emit_r(N_INCR, reg);
                        else if ((right->value & 0xffff) != 0)
                            output->emit_nr(N_ADDI, "", right->value & 0xffff, reg);
                    } else if (type == C_MINUS) {
                        if ((right->value & 0xffff) == 1)
                            output->emit_r(N_DECR, reg);
                        else if ((right->value & 0xffff) != 0)
                            output->emit_nr(N_SUBI, "", right->value & 0xffff, reg);
                    } else if (type == C_PLUSF) {
                        if ((right->value & 0xffff) != 0) {
                            output->emit_nr(N_ADDI, "", right->value & 0xffff, reg);
                            output->emit_r(N_ADCR, reg);
                        }
                    } else if (type == C_MINUSF) {
                        if ((right->value & 0xffff) != 0) {
                            output->emit_nr(N_SUBI, "", right->value & 0xffff, reg);
                            output->emit_r(N_ADCR, reg);
                            output->emit_r(N_DECR, reg);
                        }
                    } else if (type == C_AND) {
                        if ((right->value & 0xffff) != 0xffff)
                            output->emit_nr(N_ANDI, "", right->value & 0xffff, reg);
                    } else if (type == C_XOR) {
                        if ((right->value & 0xffff) != 0x0000)
                            output->emit_nr(N_XORI, "", right->value & 0xffff, reg);
                    } else if (type == C_OR) {
                        if ((right->value & 0xffff) != 0x0000) {
                            output->emit_nr(N_ANDI, "", ~right->value & 0xffff, reg);
                            output->emit_nr(N_XORI, "", right->value & 0xffff, reg);
                        }
                    } else if (type == C_EQUAL) {
                        if ((right->value) & 0xffff)
                            output->emit_nr(N_CMPI, "", right->value & 0xffff, reg);
                        else if (left->type != C_AND && left->type != C_OR && left->type != C_XOR
                                 && left->type != C_PLUS && left->type != C_MINUS)
                            output->emit_r(N_TSTR, reg);
						if (decision) {
                            output->emit_a(N_BNE, TEMP_PREFIX, decision);
							optimized = true;
						} else {
                            output->emit_nr(N_MVII, "", -1, reg);
                            output->emit_a(N_BEQ, "", 3);   // two words of jump and one word of INCR
                            output->emit_r(N_INCR, reg);
						}
                    } else if (type == C_NOTEQUAL) {
                        if ((right->value) & 0xffff)
                            output->emit_nr(N_CMPI, "", right->value & 0xffff, reg);
                        else if (left->type != C_AND && left->type != C_OR && left->type != C_XOR
                              && left->type != C_PLUS && left->type != C_MINUS)
                            output->emit_r(N_TSTR, reg);
						if (decision) {
                            output->emit_a(N_BEQ, TEMP_PREFIX, decision);
							optimized = true;
						} else {
                            output->emit_nr(N_MVII, "", -1, reg);
                            output->emit_a(N_BNE, "", 3);   // two words of jump and one word of INCR
                            output->emit_r(N_INCR, reg);
						}
                    } else if (type == C_LESS) {
                        output->emit_nr(N_CMPI, "", right->value & 0xffff, reg);
						if (decision) {
                            output->emit_a(N_BGE, TEMP_PREFIX, decision);
							optimized = true;
						} else {
                            output->emit_nr(N_MVII, "", -1, reg);
                            output->emit_a(N_BLT, "", 3);   // two words of jump and one word of INCR
                            output->emit_r(N_INCR, reg);
						}
                    } else if (type == C_LESSEQUAL) {
                        output->emit_nr(N_CMPI, "", right->value & 0xffff, reg);
						if (decision) {
                            output->emit_a(N_BGT, TEMP_PREFIX, decision);
							optimized = true;
						} else {
                            output->emit_nr(N_MVII, "", -1, reg);
                            output->emit_a(N_BLE, "", 3);   // two words of jump and one word of INCR
                            output->emit_r(N_INCR, reg);
						}
                    } else if (type == C_GREATER) {
                        output->emit_nr(N_CMPI, "", right->value & 0xffff, reg);
						if (decision) {
                            output->emit_a(N_BLE, TEMP_PREFIX, decision);
							optimized = true;
						} else {
                            output->emit_nr(N_MVII, "", -1, reg);
                            output->emit_a(N_BGT, "", 3);   // two words of jump and one word of INCR
                            output->emit_r(N_INCR, reg);
						}
                    } else if (type == C_GREATEREQUAL) {
                        output->emit_nr(N_CMPI, "", right->value & 0xffff, reg);
						if (decision) {
                            output->emit_a(N_BLT, TEMP_PREFIX, decision);
							optimized = true;
						} else {
                            output->emit_nr(N_MVII, "", -1, reg);
                            output->emit_a(N_BGE, "", 3);   // two words of jump and one word of INCR
                            output->emit_r(N_INCR, reg);
						}
                    } else if (type == C_MUL) {
                        if ((right->value & 0xffff) == 0) {
                            output->emit_r(N_CLRR, reg);
                        } if ((right->value & 0xffff) == 1) {
                            // Nothing to do
                        } if ((right->value & 0xffff) == 2) {
                            output->emit_s(N_SLL, reg, 1);
                        } else if ((right->value & 0xffff) == 4) {
                            output->emit_s(N_SLL, reg, 2);
                        } else if ((right->value & 0xffff) == 8) {
                            output->emit_s(N_SLL, reg, 2);
                            output->emit_rr(N_ADDR, reg, reg);
                        } else if ((right->value & 0xffff) == 16) {
                            output->emit_s(N_SLL, reg, 2);
                            output->emit_s(N_SLL, reg, 2);
                        } else if ((right->value & 0xffff) == 32) {
                            output->emit_s(N_SLL, reg, 2);
                            output->emit_s(N_SLL, reg, 2);
                            output->emit_rr(N_ADDR, reg, reg);
                        } else if ((right->value & 0xffff) == 64) {
                            output->emit_s(N_SLL, reg, 2);
                            output->emit_s(N_SLL, reg, 2);
                            output->emit_s(N_SLL, reg, 2);
                        } else if ((right->value & 0xffff) == 128) {
                            output->emit_s(N_SLL, reg, 2);
                            output->emit_s(N_SLL, reg, 2);
                            output->emit_s(N_SLL, reg, 2);
                            output->emit_rr(N_ADDR, reg, reg);
                        } else if ((right->value & 0xffff) == 256) {
                            output->emit_r(N_SWAP, reg);
                            output->emit_nr(N_ANDI, "", 0xff00, reg);
                        } else if ((right->value & 0xffff) == 512) {
                            output->emit_s(N_SLL, reg, 1);
                            output->emit_r(N_SWAP, reg);
                            output->emit_nr(N_ANDI, "", 0xfe00, reg);
                        } else if ((right->value & 0xffff) == 1024) {
                            output->emit_s(N_SLL, reg, 2);
                            output->emit_r(N_SWAP, reg);
                            output->emit_nr(N_ANDI, "", 0xfc00, reg);
                        } else if ((right->value & 0xffff) == 2048) {
                            output->emit_rr(N_ADDR, reg, reg);
                            output->emit_s(N_SLL, reg, 2);
                            output->emit_r(N_SWAP, reg);
                            output->emit_nr(N_ANDI, "", 0xf800, reg);
                        } else if ((right->value & 0xffff) == 4096) {
                            output->emit_s(N_SLL, reg, 2);
                            output->emit_s(N_SLL, reg, 2);
                            output->emit_r(N_SWAP, reg);
                            output->emit_nr(N_ANDI, "", 0xf000, reg);
                        } else {
                            if (jlp_used) {
                                output->emit_nr(N_MVII, "", right->value & 0xffff, 4);
                                output->emit_rl(N_MVO, reg, "", 0x9f86);
                                output->emit_rl(N_MVO, 4, "", 0x9f87);
                                output->emit_lr(N_MVI, "", 0x9f8e, reg);
                            } else if ((right->value & 0xffff) <= 127) {  // DZ-Jay's macro
                                output->emit_m(N_MULT, reg, 4, right->value & 0xffff);
                            } else {
                                int label = next_local++;
                                int label2 = next_local++;
                        
                                output->emit_nr(N_MVII, "", right->value & 0xffff, 5);
                                output->emit_r(N_CLRR, 4);
                                output->emit(N_CLRC);
                                output->emit_s(N_RRC, reg, 1);
                                output->emit_a(N_BEQ, TEMP_PREFIX, label2);
                                output->emit_l(TEMP_PREFIX, label);
                                output->emit_a(N_BNC, "", 3);  // Two words of jump and one of ADDR
                                output->emit_rr(N_ADDR, 5, 4);
                                output->emit_rr(N_ADDR, 5, 5);
                                output->emit_s(N_SARC, reg, 1);
                                output->emit_a(N_BNE, TEMP_PREFIX, label);
                                output->emit_l(TEMP_PREFIX, label2);
                                output->emit_a(N_BNC, "", 3);  // Two words of jump and one of ADDR
                                output->emit_rr(N_ADDR, 5, 4);
                                output->emit_rr(N_MOVR, 4, reg);
                            }
                        }
                    } else if (type == C_DIV) {
                        if ((right->value & 0xffff) == 0) {
                            std::cerr << "division by zero in expression\n";
                            err_code = 1;
                        } else if ((right->value & 0xffff) == 1) {
                            // Nada que hacer
                        } else if ((right->value & 0xffff) == 2) {
                            output->emit_s(N_SLR, reg, 1);
                        } else if ((right->value & 0xffff) == 4) {
                            output->emit_s(N_SLR, reg, 2);
                        } else if ((right->value & 0xffff) == 8) {
                            output->emit_s(N_SLR, reg, 2);
                            output->emit_s(N_SLR, reg, 1);
                        } else if ((right->value & 0xffff) == 16) {
                            output->emit_s(N_SLR, reg, 2);
                            output->emit_s(N_SLR, reg, 2);
                        } else if ((right->value & 0xffff) == 32) {
                            output->emit_s(N_SLR, reg, 2);
                            output->emit_s(N_SLR, reg, 2);
                            output->emit_s(N_SLR, reg, 1);
                        } else if ((right->value & 0xffff) == 64) {
                            output->emit_s(N_SLR, reg, 2);
                            output->emit_s(N_SLR, reg, 2);
                            output->emit_s(N_SLR, reg, 2);
                        } else if ((right->value & 0xffff) == 128) {
                            output->emit_r(N_SWAP, reg);
                            output->emit_rr(N_ADDR, reg, reg);
                            output->emit_r(N_ADCR, reg);
                            output->emit_nr(N_ANDI, "", 0x01ff, reg);
                        } else if ((right->value & 0xffff) == 256) {
                            output->emit_r(N_SWAP, reg);
                            output->emit_nr(N_ANDI, "", 0x00ff, reg);
                        } else {
                            if (jlp_used) {
                                output->emit_nr(N_MVII, "", right->value & 0xffff, 4);
                                output->emit_rl(N_MVO, reg, "", 0x9f8a);
                                output->emit_rl(N_MVO, 4, "", 0x9f8b);
                                output->emit_lr(N_MVI, "", 0x9f8e, reg);
                            } else {
                                int label = next_local++;
                        
                                output->emit_nr(N_MVII, "", -1, 4);
                                output->emit_l(TEMP_PREFIX, label);
                                output->emit_r(N_INCR, 4);
                                output->emit_nr(N_SUBI, "", right->value & 0xffff, reg);
                                output->emit_a(N_BC, TEMP_PREFIX, label);
                                output->emit_rr(N_MOVR, 4, reg);
                            }
                        }
                    } else if (type == C_MOD) {
                        if ((right->value & 0xffff) == 0) {
                            std::cerr << "modulus by zero in expression\n";
                            err_code = 1;
                        } else if ((right->value & 0xffff) == 1) {
                            output->emit_r(N_CLRR, reg);
                        } else if ((right->value & 0xffff) == 2
                                || (right->value & 0xffff) == 4
                                || (right->value & 0xffff) == 8
                                || (right->value & 0xffff) == 16
                                || (right->value & 0xffff) == 32
                                || (right->value & 0xffff) == 64
                                || (right->value & 0xffff) == 128
                                || (right->value & 0xffff) == 256
                                || (right->value & 0xffff) == 512
                                || (right->value & 0xffff) == 1024
                                || (right->value & 0xffff) == 2048
                                || (right->value & 0xffff) == 4096
                                || (right->value & 0xffff) == 8192
                                || (right->value & 0xffff) == 16384
                                || (right->value & 0xffff) == 32768) {
                            output->emit_nr(N_ANDI, "", (right->value & 0xffff) - 1, reg);
                        } else {
                            if (jlp_used) {
                                output->emit_nr(N_MVII, "", right->value & 0xffff, 4);
                                output->emit_rl(N_MVO, reg, "", 0x9f8a);
                                output->emit_rl(N_MVO, 4, "", 0x9f8b);
                                output->emit_lr(N_MVI, "", 0x9f8f, reg);
                            } else {
                                int label = next_local++;
                            
                                output->emit_l(TEMP_PREFIX, label);
                                output->emit_nr(N_SUBI, "", (right->value & 0xffff), reg);
                                output->emit_a(N_BC, TEMP_PREFIX, label);
                                output->emit_nr(N_ADDI, "", (right->value & 0xffff), reg);
                            }
                        }
                    }
                    
                // Optimize right side when it's variable
                } else if (right->type == C_NAME && type != C_ASSIGN) {
                    left->generate(reg, 0);
                    if (type == C_PLUS) {
                        output->emit_lr(N_ADD, VAR_PREFIX, right->value, reg);
                    } else if (type == C_MINUS) {
                        output->emit_lr(N_SUB, VAR_PREFIX, right->value, reg);
                    } else if (type == C_PLUSF) {
                        output->emit_lr(N_ADD, VAR_PREFIX, right->value, reg);
                        output->emit_r(N_ADCR, reg);
                    } else if (type == C_MINUSF) {
                        output->emit_lr(N_SUB, VAR_PREFIX, right->value, reg);
                        output->emit_r(N_ADCR, reg);
                        output->emit_r(N_DECR, reg);
                    } else if (type == C_AND) {
                        output->emit_lr(N_AND, VAR_PREFIX, right->value, reg);
                    } else if (type == C_XOR) {
                        output->emit_lr(N_XOR, VAR_PREFIX, right->value, reg);
                    } else if (type == C_OR) {
                        output->emit_lr(N_MVI, VAR_PREFIX, right->value, 4);
                        output->emit_r(N_COMR, 4);
                        output->emit_rr(N_ANDR, 4, reg);
                        output->emit_lr(N_XOR, VAR_PREFIX, right->value, reg);
                    } else if (type == C_EQUAL) {
                        output->emit_lr(N_CMP, VAR_PREFIX, right->value, reg);
						if (decision) {
                            output->emit_a(N_BNE, TEMP_PREFIX, decision);
							optimized = true;
						} else {
                            output->emit_nr(N_MVII, "", -1, reg);
                            output->emit_a(N_BEQ, "", 3);   // two words of jump and one word of INCR
                            output->emit_r(N_INCR, reg);
						}
                    } else if (type == C_NOTEQUAL) {
                        output->emit_lr(N_CMP, VAR_PREFIX, right->value, reg);
   						if (decision) {
                            output->emit_a(N_BEQ, TEMP_PREFIX, decision);
							optimized = true;
						} else {
                            output->emit_nr(N_MVII, "", -1, reg);
                            output->emit_a(N_BNE, "", 3);   // two words of jump and one word of INCR
                            output->emit_r(N_INCR, reg);
						}
                    } else if (type == C_LESS) {
                        output->emit_lr(N_CMP, VAR_PREFIX, right->value, reg);
						if (decision) {
                            output->emit_a(N_BGE, TEMP_PREFIX, decision);
							optimized = true;
						} else {
                            output->emit_nr(N_MVII, "", -1, reg);
                            output->emit_a(N_BLT, "", 3);   // two words of jump and one word of INCR
                            output->emit_r(N_INCR, reg);
						}
                    } else if (type == C_LESSEQUAL) {
                        output->emit_lr(N_CMP, VAR_PREFIX, right->value, reg);
						if (decision) {
                            output->emit_a(N_BGT, TEMP_PREFIX, decision);
							optimized = true;
						} else {
                            output->emit_nr(N_MVII, "", -1, reg);
                            output->emit_a(N_BLE, "", 3);   // two words of jump and one word of INCR
                            output->emit_r(N_INCR, reg);
						}
                    } else if (type == C_GREATER) {
                        output->emit_lr(N_CMP, VAR_PREFIX, right->value, reg);
						if (decision) {
                            output->emit_a(N_BLE, TEMP_PREFIX, decision);
							optimized = true;
						} else {
                            output->emit_nr(N_MVII, "", -1, reg);
                            output->emit_a(N_BGT, "", 3);   // two words of jump and one word of INCR
                            output->emit_r(N_INCR, reg);
						}
                    } else if (type == C_GREATEREQUAL) {
                        output->emit_lr(N_CMP, VAR_PREFIX, right->value, reg);
						if (decision) {
                            output->emit_a(N_BLT, TEMP_PREFIX, decision);
							optimized = true;
						} else {
                            output->emit_nr(N_MVII, "", -1, reg);
                            output->emit_a(N_BGE, "", 3);   // two words of jump and one word of INCR
                            output->emit_r(N_INCR, reg);
						}
                    } else if (type == C_MUL) {
                        if (jlp_used) {
                            output->emit_lr(N_MVI, VAR_PREFIX, right->value, 4);
                            output->emit_rl(N_MVO, reg, "", 0x9f86);
                            output->emit_rl(N_MVO, 4, "", 0x9f87);
                            output->emit_lr(N_MVI, "", 0x9f8e, reg);
                        } else {
                            int label = next_local++;
                            int label2 = next_local++;
                        
                            output->emit_lr(N_MVI, VAR_PREFIX, right->value, 5);
                            output->emit_r(N_CLRR, 4);
                            output->emit(N_CLRC);
                            output->emit_s(N_RRC, reg, 1);
                            output->emit_a(N_BEQ, TEMP_PREFIX, label2);
                            output->emit_l(TEMP_PREFIX, label);
                            output->emit_a(N_BNC, "", 3);  // Two words of jump and one of ADDR
                            output->emit_rr(N_ADDR, 5, 4);
                            output->emit_rr(N_ADDR, 5, 5);
                            output->emit_s(N_SARC, reg, 1);
                            output->emit_a(N_BNE, TEMP_PREFIX, label);
                            output->emit_l(TEMP_PREFIX, label2);
                            output->emit_a(N_BNC, "", 3);  // Two words of jump and one of ADDR
                            output->emit_rr(N_ADDR, 5, 4);
                            output->emit_rr(N_MOVR, 4, reg);
                            
                        }
                    } else if (type == C_DIV) {
                        if (jlp_used) {
                            output->emit_lr(N_MVI, VAR_PREFIX, right->value, 4);
                            output->emit_rl(N_MVO, reg, "", 0x9f8a);
                            output->emit_rl(N_MVO, 4, "", 0x9f8b);
                            output->emit_lr(N_MVI, "", 0x9f8e, reg);
                        } else {
                            int label = next_local++;
                            int label2 = next_local++;
                        
                            output->emit_lr(N_MVI, VAR_PREFIX, right->value, 4);
                            output->emit_rr(N_MOVR, reg, 5);
                            output->emit_r(N_TSTR, 4);
                            output->emit_a(N_BEQ, TEMP_PREFIX, label);
                            output->emit_nr(N_MVII, "", -1, reg);
                            output->emit_l(TEMP_PREFIX, label2);
                            output->emit_r(N_INCR, reg);
                            output->emit_rr(N_SUBR, 4, 5);
                            output->emit_a(N_BC, TEMP_PREFIX, label2);
                            output->emit_l(TEMP_PREFIX, label);
                        }
                    } else if (type == C_MOD) {
                        if (jlp_used) {
                            output->emit_lr(N_MVI, VAR_PREFIX, right->value, 4);
                            output->emit_rl(N_MVO, reg, "", 0x9f8a);
                            output->emit_rl(N_MVO, 4, "", 0x9f8b);
                            output->emit_lr(N_MVI, "", 0x9f8f, reg);
                        } else {
                            int label = next_local++;
                            int label2 = next_local++;
                        
                            output->emit_lr(N_MVI, VAR_PREFIX, right->value, 4);
                            output->emit_r(N_TSTR, 4);
                            output->emit_a(N_BEQ, TEMP_PREFIX, label);
                            output->emit_l(TEMP_PREFIX, label2);
                            output->emit_rr(N_SUBR, 4, reg);
                            output->emit_a(N_BC, TEMP_PREFIX, label2);
                            output->emit_rr(N_ADDR, 4, reg);
                            output->emit_l(TEMP_PREFIX, label);
                        }
                    }
                } else if (type == C_ASSIGN && right->type == C_PLUS
                           && right->left->type == C_NAME_RO && right->right->type == C_NUM) {
                    left->generate(reg, 0);
                    if (value == 0)
                        output->emit_rlo8(N_MVO, reg, LABEL_PREFIX, right->left->value, right->right->value);
                    else
                        output->emit_rlo(N_MVO, reg, LABEL_PREFIX, right->left->value, right->right->value);
                } else {
                    int reversed = 0;
                    
                    // Common tree generation
                    if (left->regs > 4 - reg && right->regs > 4 - reg) {
                        right->generate(reg, 0);
                        output->emit_r(N_PSHR, reg);
                        left->generate(reg, 0);
                        output->emit_r(N_PULR, reg + 1);
                    } else if (left->regs >= right->regs) {
                        left->generate(reg, 0);
                        right->generate(reg + 1, 0);
                    } else {
                        right->generate(reg, 0);
                        left->generate(reg + 1, 0);
                        reversed = 1;
                    }
                    if (type == C_PLUS) {
                        output->emit_rr(N_ADDR, reg + 1, reg);
                    } else if (type == C_MINUS) {
                        if (reversed) {
                            output->emit_rr(N_SUBR, reg, reg + 1);
                            output->emit_rr(N_MOVR, reg + 1, reg);
                        } else {
                            output->emit_rr(N_SUBR, reg + 1, reg);
                        }
                    } else if (type == C_PLUSF) {
                        output->emit_rr(N_ADDR, reg + 1, reg);
                        output->emit_r(N_ADCR, reg);
                    } else if (type == C_MINUSF) {
                        if (reversed) {
                            output->emit_rr(N_SUBR, reg, reg + 1);
                            output->emit_r(N_ADCR, reg + 1);
                            output->emit_r(N_DECR, reg + 1);
                            output->emit_rr(N_MOVR, reg + 1, reg);
                        } else {
                            output->emit_rr(N_SUBR, reg + 1, reg);
                            output->emit_r(N_ADCR, reg);
                            output->emit_r(N_DECR, reg);
                        }
                    } else if (type == C_AND) {
                        output->emit_rr(N_ANDR, reg + 1, reg);
                    } else if (type == C_XOR) {
                        output->emit_rr(N_XORR, reg + 1, reg);
                    } else if (type == C_OR) {
                        output->emit_r(N_COMR, reg + 1);
                        output->emit_rr(N_ANDR, reg + 1, reg);
                        output->emit_r(N_COMR, reg + 1);
                        output->emit_rr(N_XORR, reg + 1, reg);
                    } else if (type == C_EQUAL) {
                        output->emit_rr(N_CMPR, reg + 1, reg);
						if (decision) {
                            output->emit_a(N_BNE, TEMP_PREFIX, decision);
							optimized = true;
						} else {
                            output->emit_nr(N_MVII, "", -1, reg);
                            output->emit_a(N_BEQ, "", 3);
                            output->emit_r(N_INCR, reg);
						}
                    } else if (type == C_NOTEQUAL) {
                        output->emit_rr(N_CMPR, reg + 1, reg);
						if (decision) {
                            output->emit_a(N_BEQ, TEMP_PREFIX, decision);
							optimized = true;
						} else {
                            output->emit_nr(N_MVII, "", -1, reg);
                            output->emit_a(N_BNE, "", 3);
                            output->emit_r(N_INCR, reg);
						}
                    } else if (type == C_LESS) {
                        output->emit_rr(N_CMPR, reg + 1, reg);
						if (decision) {
                            output->emit_a(reversed ? N_BLE : N_BGE, TEMP_PREFIX, decision);
							optimized = true;
						} else {
                            output->emit_nr(N_MVII, "", -1, reg);
                            output->emit_a(reversed ? N_BGT : N_BLT, "", 3);
                            output->emit_r(N_INCR, reg);
						}
                    } else if (type == C_LESSEQUAL) {
                        output->emit_rr(N_CMPR, reg + 1, reg);
						if (decision) {
                            output->emit_a(reversed ? N_BLT : N_BGT, TEMP_PREFIX, decision);
							optimized = true;
						} else {
                            output->emit_nr(N_MVII, "", -1, reg);
                            output->emit_a(reversed ? N_BGE : N_BLE, "", 3);
                            output->emit_r(N_INCR, reg);
						}
                    } else if (type == C_GREATER) {
                        output->emit_rr(N_CMPR, reg + 1, reg);
						if (decision) {
                            output->emit_a(reversed ? N_BGE : N_BLE, TEMP_PREFIX, decision);
							optimized = true;
						} else {
                            output->emit_nr(N_MVII, "", -1, reg);
                            output->emit_a(reversed ? N_BLT : N_BGT, "", 3);
                            output->emit_r(N_INCR, reg);
						}
                    } else if (type == C_GREATEREQUAL) {
                        output->emit_rr(N_CMPR, reg + 1, reg);
						if (decision) {
                            output->emit_a(reversed ? N_BGT : N_BLT, TEMP_PREFIX, decision);
							optimized = true;
						} else {
                            output->emit_nr(N_MVII, "", -1, reg);
                            output->emit_a(reversed ? N_BLE : N_BGE, "", 3);
                            output->emit_r(N_INCR, reg);
						}
                    } else if (type == C_MUL) {
                        if (jlp_used) {
                            output->emit_rl(N_MVO, reg, "", 0x9f86);
                            output->emit_rl(N_MVO, reg + 1, "", 0x9f87);
                            output->emit_lr(N_MVI, "", 0x9f8e, reg);
                        } else {
                            int label = next_local++;
                            int label2 = next_local++;
                        
                            output->emit_r(N_CLRR, 4);
                            output->emit(N_CLRC);
                            output->emit_s(N_RRC, reg, 1);
                            output->emit_a(N_BEQ, TEMP_PREFIX, label2);
                            output->emit_l(TEMP_PREFIX, label);
                            output->emit_a(N_BNC, "", 3);  // Two words of jump and one of ADDR
                            output->emit_rr(N_ADDR, reg + 1, 4);
                            output->emit_rr(N_ADDR, reg + 1, reg + 1);
                            output->emit_s(N_SARC, reg, 1);
                            output->emit_a(N_BNE, TEMP_PREFIX, label);
                            output->emit_l(TEMP_PREFIX, label2);
                            output->emit_a(N_BNC, "", 3);  // Two words of jump and one of ADDR
                            output->emit_rr(N_ADDR, reg + 1, 4);
                            output->emit_rr(N_MOVR, 4, reg);
                        }
                    } else if (type == C_DIV) {
                        if (jlp_used) {
                            if (reversed) {
                                output->emit_rl(N_MVO, reg + 1, "", 0x9f8a);
                                output->emit_rl(N_MVO, reg, "", 0x9f8b);
                            } else {
                                output->emit_rl(N_MVO, reg, "", 0x9f8a);
                                output->emit_rl(N_MVO, reg + 1, "", 0x9f8b);
                            }
                            output->emit_lr(N_MVI, "", 0x9f8e, reg);
                        } else {
                            int label = next_local++;
                            int label2 = next_local++;
                        
                            if (reversed) {
                                output->emit_rr(N_MOVR, reg + 1, 5);
                                output->emit_r(N_TSTR, reg);
                                output->emit_a(N_BEQ, TEMP_PREFIX, label);
                                output->emit_nr(N_MVII, "", -1, reg + 1);
                                output->emit_l(TEMP_PREFIX, label2);
                                output->emit_r(N_INCR, reg + 1);
                                output->emit_rr(N_SUBR, reg, 5);
                                output->emit_a(N_BC, TEMP_PREFIX, label2);
                                output->emit_l(TEMP_PREFIX, label);
                                output->emit_rr(N_MOVR, reg + 1, reg);
                            } else {
                                output->emit_rr(N_MOVR, reg, 5);
                                output->emit_r(N_TSTR, reg + 1);
                                output->emit_a(N_BEQ, TEMP_PREFIX, label);
                                output->emit_nr(N_MVII, "", -1, reg);
                                output->emit_l(TEMP_PREFIX, label2);
                                output->emit_r(N_INCR, reg);
                                output->emit_rr(N_SUBR, reg + 1, 5);
                                output->emit_a(N_BC, TEMP_PREFIX, label2);
                                output->emit_l(TEMP_PREFIX, label);
                            }
                        }
                    } else if (type == C_MOD) {
                        if (jlp_used) {
                            if (reversed) {
                                output->emit_rl(N_MVO, reg + 1, "", 0x9f8a);
                                output->emit_rl(N_MVO, reg, "", 0x9f8b);
                            } else {
                                output->emit_rl(N_MVO, reg, "", 0x9f8a);
                                output->emit_rl(N_MVO, reg + 1, "", 0x9f8b);
                            }
                            output->emit_lr(N_MVI, "", 0x9f8f, reg);
                        } else {
                            int label = next_local++;
                            int label2 = next_local++;
                        
                            if (reversed) {
                                output->emit_r(N_TSTR, reg);
                                output->emit_a(N_BEQ, TEMP_PREFIX, label);
                                output->emit_l(TEMP_PREFIX, label2);
                                output->emit_rr(N_SUBR, reg, reg + 1);
                                output->emit_a(N_BC, TEMP_PREFIX, label2);
                                output->emit_rr(N_ADDR, reg, reg + 1);
                                output->emit_l(TEMP_PREFIX, label);
                                output->emit_rr(N_MOVR, reg + 1, reg);
                            } else {
                                output->emit_r(N_TSTR, reg + 1);
                                output->emit_a(N_BEQ, TEMP_PREFIX, label);
                                output->emit_l(TEMP_PREFIX, label2);
                                output->emit_rr(N_SUBR, reg + 1, reg);
                                output->emit_a(N_BC, TEMP_PREFIX, label2);
                                output->emit_rr(N_ADDR, reg + 1, reg);
                                output->emit_l(TEMP_PREFIX, label);
                            }
                        }
                    } else if (type == C_ASSIGN) {
                        if (reversed) {
                            if (reg == 0) {
                                output->emit_rr(N_MOVR, reg, 4);
                                output->emit_rr(N_MVOA, reg + 1, 4);
                            } else {
                                output->emit_rr(N_MVOA, reg + 1, reg);
                            }
                        } else {
                            output->emit_rr(N_MVOA, reg, reg + 1);
                        }
                    }
                }
                break;
        }
    }
};

//
// Representation for a loop
//
struct loop {
    int type;
    class node *step;
    class node *final;
    string var;
    int label;
};

//
// Representation for a macro
//
class lexical_element {
    enum lexical_component lex;
    int value;
    string name;
    
public:
    lexical_element(enum lexical_component lex, int value, string name) {
        this->lex = lex;
        this->value = value;
        this->name = name;
    }
    
    enum lexical_component get_lex(void) {
        return this->lex;
    }
    
    int get_value(void) {
        return this->value;
    }
    
    string get_name(void) {
        return this->name;
    }
};

struct macro {
    int total_arguments;
    bool in_use;
    list <lexical_element *> definition;
};

//
// The compiler
//
class compiler {
private:
    ifstream input;
    ifstream included;
	ifstream included2;
    int saved_line_number;
    int active_include;
    int next_include;
    ifstream include[50];
    string line;
	int line_start;
    string assigned;
    int pstring[256];
    int offset;
    size_t line_pos;
    size_t line_size;
    map <string, int> arrays;
    map <string, int> labels;
    map <string, int> variables;
    map <string, int> constants;
    map <string, int> functions;
    map <string, int> read_write;
    map <string, macro *> macros;
    map <string, int>::iterator access;
    list <struct loop> loops;
    int line_number;
    int next_label;
    int next_var;
    
    enum lexical_component lex;
    int value;
    string name;
    list <lexical_element *> accumulated;
    
    int bitmap_value;
    int bitmap_byte;
    int frame_drive;    // Label for frame-driven game (ON FRAME GOSUB)
    int last_is_return; // Indicates if last statement processed was a RETURN
    int scroll_used;    // Indicates if scroll used
    int keypad_used;    // Indicates if keypad used
    int music_used;     // Indicates if music used
    int stack_used;     // Indicates if stack check used
    int numbers_used;   // Indicates if numbers used (PRINT)
    int voice_used;     // Indicates if Intellivoice used (VOICE)
    bool ecs_used;      // Indicates if ECS used (SOUND, CONT3, CONT4)
    
    //
    // Avoid spaces
    //
    void skip_spaces(void) {
        while (line_pos < line_size && isspace(line[line_pos]))
            line_pos++;
    }
    
    //
    // Sneak-peek to next character
    //
    int sneak_peek(void) {
        if (accumulated.size() > 0) {
            if (accumulated.front()->get_lex() == C_LPAREN)
                return '(';
            return 0;
        }
        skip_spaces();
        if (line_pos == line_size)
            return '\0';
        return line[line_pos];
    }
    
    //
    // Gets another lexical component
    // Output:
    //  lex = lexical component
    //  name = identifier
    //  value = value
    //  pstring = string for PRINT
    //
    void get_lex(void) {
        if (accumulated.size() > 0) {
            lex = accumulated.front()->get_lex();
            value = accumulated.front()->get_value();
            name = accumulated.front()->get_name();
            accumulated.pop_front();
#if defined(DEBUG_FN)
            std::cerr << "C " << lexical_names[lex] << "," << value << "," << name << "\n";
            std::cerr.flush();
#endif
            return;
        }
        skip_spaces();
        if (line_pos == line_size) {
            lex = C_END;
            return;
        }
		if (isalpha(line[line_pos]) || line[line_pos] == '#') {  // Name or label
            name = "";
			name += toupper(line[line_pos]);
			line_pos++;
            while (line_pos < line_size
             && (isalnum(line[line_pos]) || line[line_pos] == '_' || line[line_pos] == '#')) {
                name += toupper(line[line_pos]);
                line_pos++;
            }
            if (line_pos < line_size && line[line_pos] == ':' && line_start
            && name != "RETURN" && name != "CLS" && name != "WAIT"
            && name != "RESTORE" && name != "STACK_CHECK" && name != "WEND") {
                lex = C_LABEL;
                line_pos++;
            } else {
                lex = C_NAME;
            }
			line_start = 0;
            return;
        }
        if (isdigit(line[line_pos])) {  // Decimal number
            int fraction;
            
            value = 0;
            while (line_pos < line_size && isdigit(line[line_pos]))
                value = (value * 10) + line[line_pos++] - '0';
            if (value > 65535) {
                if (warnings)
                    std::cerr << "Warning: Number exceeds 16 bits in line " << line_number << "\n";
                err_code = 1;
            }
            if (line_pos < line_size && line[line_pos] == '.'
                && line_pos + 1 < line_size && isdigit(line[line_pos + 1])) {
                if (value > 255) {
                    if (warnings)
                        std::cerr << "Warning: Fixed number exceeds basic 8 bits in line " << line_number << "\n";
                    err_code = 1;
                }
                line_pos++;
                fraction = 0;
                if (line_pos < line_size && isdigit(line[line_pos]))
                    fraction += (line[line_pos++] - '0') * 100;
                if (line_pos < line_size && isdigit(line[line_pos]))
                    fraction += (line[line_pos++] - '0') * 10;
                if (line_pos < line_size && isdigit(line[line_pos]))
                    fraction += (line[line_pos++] - '0');
                while (line_pos < line_size && isdigit(line[line_pos]))
                    line_pos++;
                value += (int) (fraction * (256.0 / 1000.0) + 0.5) * 256;
            }
            lex = C_NUM;
			line_start = 0;
            return;
        }
        if (line[line_pos] == '$' && line_pos + 1 < line_size
         && isxdigit(line[line_pos + 1])) {  // Hexadecimal number
            value = 0;
            line_pos++;
            while (line_pos < line_size && isxdigit(line[line_pos])) {
                int temp;
                
                temp = toupper(line[line_pos]) - '0';
                if (temp > 9)
                    temp -= 7;
                value = (value << 4) | temp;
                line_pos++;
            }
            lex = C_NUM;
			line_start = 0;
            return;
        }
        if (line[line_pos] == '&' && line_pos + 1 < line_size
         && (line[line_pos + 1] == '0' || line[line_pos + 1] == '1')) {  // Binary number
            value = 0;
            line_pos++;
            while (line_pos < line_size && (line[line_pos] == '0' || line[line_pos] == '1')) {
                value = (value << 1) | (line[line_pos] & 1);
                line_pos++;
            }
            lex = C_NUM;
			line_start = 0;
            return;
        }
        if (line[line_pos] == '"') {  // String
            line_pos++;
            offset = 0;
            while (line_pos < line_size && line[line_pos] != '"') {
                if (offset == 255) {
                    emit_error("string too long");
                    do {
                        line_pos++;
                    } while (line_pos < line_size && line[line_pos] != '"') ;
                    break;
                }
                if (line[line_pos] == '\\') {
                    int c;
                    
                    line_pos++;
                    if (line_pos < line_size && line[line_pos] == '"') {
                        c = line[line_pos] - 32;
                        if (c < 0)
                            c = 0;
                        line_pos++;
                    } else {
                        c = 0;
                        while (line_pos < line_size && isdigit(line[line_pos])) {
                            c = c * 10 + (line[line_pos] - '0');
                            line_pos++;
                        }
                    }
                    pstring[offset++] = c;
                } else {
                    int c;
                    
                    c = line[line_pos] - 32;
                    if (c < 0)
                        c = 0;
                    line_pos++;
                    pstring[offset++] = c;
                }
            }
            if (line_pos < line_size && line[line_pos] == '"') {
                line_pos++;
            } else {
                emit_error("unfinished string");
            }
            lex = C_STRING;
			line_start = 0;
            return;
        }
   		line_start = 0;
        switch (line[line_pos]) {
            case '=':
                line_pos++;
                lex = C_EQUAL;
                break;
            case '+':
                line_pos++;
                if (line_pos < line_size && line[line_pos] == '.') {
                    lex = C_PLUSF;
                    line_pos++;
                } else {
                    lex = C_PLUS;
                }
                break;
            case '-':
                line_pos++;
                if (line_pos < line_size && line[line_pos] == '.') {
                    lex = C_MINUSF;
                    line_pos++;
                } else {
                    lex = C_MINUS;
                }
                break;
            case '(':
                line_pos++;
                lex = C_LPAREN;
                break;
            case ')':
                line_pos++;
                lex = C_RPAREN;
                break;
            case '<':
                line_pos++;
                lex = C_LESS;
                if (line[line_pos] == '=') {
                    line_pos++;
                    lex = C_LESSEQUAL;
                } else if (line[line_pos] == '>') {
                    line_pos++;
                    lex = C_NOTEQUAL;
                }
                break;
            case '>':
                line_pos++;
                lex = C_GREATER;
                if (line[line_pos] == '=') {
                    line_pos++;
                    lex = C_GREATEREQUAL;
                }
                break;
            case '*':
                line_pos++;
                lex = C_MUL;
                break;
            case '/':
                line_pos++;
                lex = C_DIV;
                break;
            case '%':
                line_pos++;
                lex = C_MOD;
                break;
            case ':':
                line_pos++;
                lex = C_COLON;
                break;
            case '.':
                line_pos++;
                lex = C_PERIOD;
                break;
            case ',':
                line_pos++;
                lex = C_COMMA;
                break;
            case '\'':
                line_pos = line_size;
                lex = C_END;
                break;
            default:
                line_pos++;
                lex = C_ERR;
                break;
        }
    }
    
    //
    // Evaluates an expression
    // Result in R0
    //
    enum lexical_component eval_expr(int reg, int decision)
    {
        class node *tree;
        enum lexical_component c;
        
        tree = eval_level0();
        tree->label();
		optimized = false;
        tree->generate(reg, decision);
        c = tree->node_type();
        delete tree;
        tree = NULL;
        return c;
    }
    
    //
    // Expression evaluation: Level 0 (OR)
    //
    class node *eval_level0(void)
    {
        class node *left;
        class node *right;
        
        left = eval_level1();
        while (1) {
            if (lex == C_NAME && name == "OR") {
                get_lex();
                right = eval_level1();
                left = new node(C_OR, 0, left, right);
            } else {
                break;
            }
        }
        return left;
    }
    
    //
    // Expression evaluation: Level 1 (XOR)
    //
    class node *eval_level1(void)
    {
        class node *left;
        class node *right;
        
        left = eval_level2();
        while (1) {
            if (lex == C_NAME && name == "XOR") {
                get_lex();
                right = eval_level2();
                left = new node(C_XOR, 0, left, right);
            } else {
                break;
            }
        }
        return left;
    }
    
    //
    // Expression evaluation: Level 2 (AND)
    //
    class node *eval_level2(void)
    {
        class node *left;
        class node *right;
        
        left = eval_level3();
        while (1) {
            if (lex == C_NAME && name == "AND") {
                get_lex();
                right = eval_level3();
                left = new node(C_AND, 0, left, right);
            } else {
                break;
            }
        }
        return left;
    }
    
    //
    // Expression evaluation: Level 3 (= <> < <= > >=)
    //
    class node *eval_level3(void)
    {
        class node *left;
        class node *right;
        
        left = eval_level4();
        while (1) {
            if (lex == C_EQUAL) {
                get_lex();
                right = eval_level4();
                left = new node(C_EQUAL, 0, left, right);
            } else if (lex == C_NOTEQUAL) {
                get_lex();
                right = eval_level4();
                left = new node(C_NOTEQUAL, 0, left, right);
            } else if (lex == C_LESS) {
                get_lex();
                right = eval_level4();
                left = new node(C_LESS, 0, left, right);
            } else if (lex == C_LESSEQUAL) {
                get_lex();
                right = eval_level4();
                left = new node(C_LESSEQUAL, 0, left, right);
            } else if (lex == C_GREATER) {
                get_lex();
                right = eval_level4();
                left = new node(C_GREATER, 0, left, right);
            } else if (lex == C_GREATEREQUAL) {
                get_lex();
                right = eval_level4();
                left = new node(C_GREATEREQUAL, 0, left, right);
            } else {
                break;
            }
        }
        return left;
    }
    
    //
    // Expression evaluation: Level 4 (+ -)
    //
    class node *eval_level4(void)
    {
        class node *left;
        class node *right;
        
        left = eval_level5();
        while (1) {
            if (lex == C_PLUS) {
                get_lex();
                right = eval_level5();
                left = new node(C_PLUS, 0, left, right);
            } else if (lex == C_MINUS) {
                get_lex();
                right = eval_level5();
                left = new node(C_MINUS, 0, left, right);
            } else if (lex == C_PLUSF) {
                get_lex();
                right = eval_level5();
                left = new node(C_PLUSF, 0, left, right);
            } else if (lex == C_MINUSF) {
                get_lex();
                right = eval_level5();
                left = new node(C_MINUSF, 0, left, right);
            } else {
                break;
            }
        }
        return left;
    }
    
    //
    // Expression evaluation: Level 5 (* / %)
    //
    class node *eval_level5(void)
    {
        class node *left;
        class node *right;
        
        left = eval_level6();
        while (1) {
            if (lex == C_MUL) {
                get_lex();
                right = eval_level6();
                left = new node(C_MUL, 0, left, right);
            } else if (lex == C_DIV) {
                get_lex();
                right = eval_level6();
                left = new node(C_DIV, 0, left, right);
            } else if (lex == C_MOD) {
                get_lex();
                right = eval_level6();
                left = new node(C_MOD, 0, left, right);
            } else {
                break;
            }
        }
        return left;
    }
    
    //
    // Expression evaluation: Level 6 (NOT)
    //
    class node *eval_level6(void)
    {
        class node *left;
        
        if (lex == C_MINUS) {
            get_lex();
            left = eval_level7();
            left = new node(C_NEG, 0, left, NULL);
        } else if (lex == C_NAME && name == "NOT") {
            get_lex();
            left = eval_level7();
            left = new node(C_NOT, 0, left, NULL);
        } else {
            left = eval_level7();
        }
        return left;
    }
    
    //
    // Expression evaluation: Level 7 (parenthesis, functions, variables and values)
    //
    class node *eval_level7(void)
    {
        if (lex == C_LPAREN) {
            class node *tree;
            
            get_lex();
            tree = eval_level0();
            if (lex != C_RPAREN)
                emit_error("missing right parenthesis");
            else
                get_lex();
            return tree;
        }
        if (lex == C_STRING) {
            int temp;
            
            if (offset == 0) {
                emit_error("empty string");
                temp = 0;
            } else {
                temp = pstring[0];
            }
            get_lex();
            return new node(C_NUM, temp, NULL, NULL);
        }
        if (lex == C_NAME) {
            int temp;
            
            if (name == "PEEK") {
                class node *tree;
                
                get_lex();
                if (lex != C_LPAREN)
                    emit_error("missing left parenthesis in PEEK");
                else
                    get_lex();
                tree = eval_level0();
                if (lex != C_RPAREN)
                    emit_error("missing right parenthesis in PEEK");
                else
                    get_lex();
                return new node(C_PEEK, 0, tree, NULL);
            } else if (name == "ABS") {
                class node *tree;
                
                get_lex();
                if (lex != C_LPAREN)
                    emit_error("missing left parenthesis in ABS");
                else
                    get_lex();
                tree = eval_level0();
                if (lex != C_RPAREN)
                    emit_error("missing right parenthesis in ABS");
                else
                    get_lex();
                return new node(C_ABS, 0, tree, NULL);
            } else if (name == "SGN") {
                class node *tree;
                
                get_lex();
                if (lex != C_LPAREN)
                    emit_error("missing left parenthesis in SGN");
                else
                    get_lex();
                tree = eval_level0();
                if (lex != C_RPAREN)
                    emit_error("missing right parenthesis in SGN");
                else
                    get_lex();
                return new node(C_SGN, 0, tree, NULL);
            } else if (name == "CONT" || name == "CONT1" || name == "CONT2") {
                class node *tree;
                int c;
                
                if (name == "CONT2") {
                    tree = new node(C_NUM, c = 0x01FE, NULL, NULL);
                    tree = new node(C_PEEK, 0, tree, NULL);
                    tree = new node(C_NOT, 0, tree, NULL);
                } else if (name == "CONT1") {
                    tree = new node(C_NUM, c = 0x01FF, NULL, NULL);
                    tree = new node(C_PEEK, 0, tree, NULL);
                    tree = new node(C_NOT, 0, tree, NULL);
                } else if (name == "CONT4") {
                    tree = new node(C_NUM, c = 0x00FE, NULL, NULL);
                    tree = new node(C_PEEK, 0, tree, NULL);
                    tree = new node(C_NOT, 0, tree, NULL);
                    ecs_used = true;
                } else if (name == "CONT3") {
                    tree = new node(C_NUM, c = 0x00FF, NULL, NULL);
                    tree = new node(C_PEEK, 0, tree, NULL);
                    tree = new node(C_NOT, 0, tree, NULL);
                    ecs_used = true;
                } else {
                    tree = new node(C_VAR, c = 13, NULL, NULL);
                }
                get_lex();
                if (lex == C_PERIOD) {
                    get_lex();
                    if (lex != C_NAME)
                        emit_error("missing name for CONT? syntax");
                    else if (name == "DOWN")
                        tree = new node(C_AND, 0, tree, new node(C_NUM, 0x01, NULL, NULL));
                    else if (name == "RIGHT")
                        tree = new node(C_AND, 0, tree, new node(C_NUM, 0x02, NULL, NULL));
                    else if (name == "UP")
                        tree = new node(C_AND, 0, tree, new node(C_NUM, 0x04, NULL, NULL));
                    else if (name == "LEFT")
                        tree = new node(C_AND, 0, tree, new node(C_NUM, 0x08, NULL, NULL));
                    else if (name == "BUTTON")
                        tree = new node(C_AND, 0, tree, new node(C_NUM, 0xe0, NULL, NULL));
                    else if (name == "B0")
                        tree = new node(C_EQUAL, 0,
                                        new node(C_AND, 0, tree, new node(C_NUM, 0xe0, NULL, NULL)),
                                        new node(C_NUM, 0xa0, NULL, NULL));
                    else if (name == "B1")
                        tree = new node(C_EQUAL, 0,
                                        new node(C_AND, 0, tree, new node(C_NUM, 0xe0, NULL, NULL)),
                                        new node(C_NUM, 0x60, NULL, NULL));
                    else if (name == "B2")
                        tree = new node(C_EQUAL, 0,
                                        new node(C_AND, 0, tree, new node(C_NUM, 0xe0, NULL, NULL)),
                                        new node(C_NUM, 0xc0, NULL, NULL));
                    else if (name == "KEY") {
                        if (c != 13 && c < 0x100) {
                            emit_error("KEY support not available (yet) for CONT3 and CONT4");
                        } else {
                            delete tree;
                            if (c == 13)
                                tree = new node(C_VAR, 14, NULL, NULL);
                            else
                                tree = new node(C_VAR, (c == 0x01ff) ? 10 : 11, NULL, NULL);
                            keypad_used = 1;
                        }
                    } else {
                        emit_error("wrong name for CONT? syntax");
                    }
                    get_lex();
                }
                return tree;
            } else if (name == "COL0") {
                get_lex();
                return new node(C_VAR, 0, NULL, NULL);
            } else if (name == "COL1") {
                get_lex();
                return new node(C_VAR, 1, NULL, NULL);
            } else if (name == "COL2") {
                get_lex();
                return new node(C_VAR, 2, NULL, NULL);
            } else if (name == "COL3") {
                get_lex();
                return new node(C_VAR, 3, NULL, NULL);
            } else if (name == "COL4") {
                get_lex();
                return new node(C_VAR, 4, NULL, NULL);
            } else if (name == "COL5") {
                get_lex();
                return new node(C_VAR, 5, NULL, NULL);
            } else if (name == "COL6") {
                get_lex();
                return new node(C_VAR, 6, NULL, NULL);
            } else if (name == "COL7") {
                get_lex();
                return new node(C_VAR, 7, NULL, NULL);
            } else if (name == "FRAME") {
                get_lex();
                return new node(C_VAR, 8, NULL, NULL);
            } else if (name == "RAND") {
                get_lex();
                if (lex == C_LPAREN) {  // RAND with range
                    class node *tree;
                    int c;
                    
                    get_lex();
                    tree = eval_level0();
                    if (lex != C_RPAREN)
                        emit_error("missing right parenthesis in RAND");
                    else
                        get_lex();
                    if (tree->node_type() == C_NUM) {
                        c = tree->node_value();
                        if (c == 2
                         || c == 4
                         || c == 8
                         || c == 16
                         || c == 32
                         || c == 64
                         || c == 128
                         || c == 256) {
                            return new node(C_MOD, 0, new node(C_VAR, 9, NULL, NULL), tree);
                        }
                    }
                    return new node(C_RAND, 0, tree, NULL);
                }
                return new node(C_VAR, 9, NULL, NULL);
            } else if (name == "RANDOM") {
                class node *tree;
                
                get_lex();
                if (lex != C_LPAREN)
                    emit_error("missing left parenthesis in RANDOM");
                else
                    get_lex();
                tree = eval_level0();
                if (lex != C_RPAREN)
                    emit_error("missing right parenthesis in RANDOM");
                else
                    get_lex();
                return new node(C_RANDOM, 0, tree, NULL);
            } else if (name == "NTSC") {
                get_lex();
                return new node(C_VAR, 12, NULL, NULL);
            } else if (name == "USR") {  // Call to function written in assembler
                class node *tree;
                class node *list;
                class node *last_list;
                int c;
                
                get_lex();
                if (lex != C_NAME)
                    emit_error("missing function name");
                if (functions[name] != 0) {
                    temp = functions[name];
                } else {
                    functions[name] = temp = next_label++;
                }
                get_lex();
                tree = NULL;
                list = NULL;
                last_list = NULL;
                c = 0;
                if (lex == C_LPAREN) {
                    get_lex();
                    while (1) {
                        tree = eval_level0();
                        tree = new node(C_COMMA, 0, tree, NULL);
                        if (list == NULL) {
                            list = tree;
                        } else {
                            last_list->set_right(tree);
                        }
                        last_list = tree;
                        c++;
                        if (lex != C_COMMA)
                            break;
                        get_lex();
                    }
                    if (lex == C_RPAREN)
                        get_lex();
                    else
                        emit_error("missing right parenthesis");
                }
                if (c > 4)
                    emit_error("more than 4 arguments for USR function");
                return new node(C_USR, temp, list, NULL);
            } else if (name == "VARPTR") {  // Access to variable/array/label address
                get_lex();
                if (lex != C_NAME) {
                    emit_error("missing variable name for VARPTR");
                    return new node(C_NUM, 0, NULL, NULL);
                }
                if (sneak_peek() == '(') {  // Indexed access
                    class node *tree;
                    
                    if (arrays[name] != 0) {
                        temp = arrays[name] >> 16;
                    } else if (labels[name] != 0) {
                        temp = labels[name];
                    } else {
                        labels[name] = temp = next_label++;
                    }
                    get_lex();
                    if (lex != C_LPAREN)
                        emit_error("missing left parenthesis in array access");
                    else
                        get_lex();
                    tree = eval_level0();
                    if (lex != C_RPAREN)
                        emit_error("missing right parenthesis in array access");
                    else
                        get_lex();
                    return new node(C_PLUS, 0,
                                    new node(C_NAME_RO, temp, NULL, NULL), tree);
                }
                if ((constants[name] & 0x10000) != 0) {
                    emit_error("constants doesn't have address for VARPTR");
                    get_lex();
                    return new node(C_NUM, 0, NULL, NULL);
                }
                if (variables[name] == 0)
                    variables[name] = next_var++;
                temp = variables[name];
                get_lex();
                return new node(C_NAME_R, temp, NULL, NULL);
            } else if (macros[name] != NULL) {  // Function (macro)
                if (replace_macro())
                    return new node(C_NUM, 0, NULL, NULL);
                return eval_level0();
            }
            if (sneak_peek() == '(') {  // Indexed access
                class node *tree;
                
                if (arrays[name] != 0) {
                    temp = arrays[name] >> 16;
                } else if (labels[name] != 0) {
                    temp = labels[name];
                } else {
                    labels[name] = temp = next_label++;
                }
                get_lex();
                if (lex != C_LPAREN)
                    emit_error("missing left parenthesis in array access");
                else
                    get_lex();
                tree = eval_level0();
                if (lex != C_RPAREN)
                    emit_error("missing right parenthesis in array access");
                else
                    get_lex();
                return new node(C_PEEK, 0,
                                new node(C_PLUS, 0,
                                         new node(C_NAME_RO, temp, NULL, NULL), tree), NULL);
            }
            if ((constants[name] & 0x10000) != 0) {
                temp = constants[name] & 0xffff;
                get_lex();
                return new node(C_NUM, temp, NULL, NULL);
            }
            read_write[name] = (read_write[name] | 1);
            if (variables[name] == 0)
                variables[name] = next_var++;
            temp = variables[name];
            get_lex();
            return new node(C_NAME, temp, NULL, NULL);
        }
        if (lex == C_NUM) {
            int temp;
            
            temp = value;
            get_lex();
            return new node(C_NUM, temp, NULL, NULL);
        }
        emit_error("bad syntax for expression");
        return new node(C_NUM, 0, NULL, NULL);
    }
    
    //
    // Generates an error message
    //
    void emit_error(string message)
    {
        std::cerr << "Error: " << message << " in line " << line_number << "\n";
        err_code = 1;
    }
    
    //
    // Replace a macro
    //
    int replace_macro(void)
    {
        string function;
        int total_arguments;
        int c;
        int level;
        list <lexical_element *>::iterator explorer;
        list <lexical_element *>::iterator explorer2;
        list <lexical_element *> *argument;
        
        function = name;
        get_lex();
        if (macros[function]->in_use) {
            emit_error("Recursion in FN name");
            return 1;
        }
        macros[function]->in_use = true;
        total_arguments = macros[function]->total_arguments;
        if (total_arguments > 0) {
            argument = new list <lexical_element *> [total_arguments]();
            if (lex != C_LPAREN) {
                emit_error("missing left parenthesis in call to FN");
                return 1;
            }
            get_lex();
            c = 0;
            level = 0;
            while (c < total_arguments) {
                while (1) {
                    if (level == 0 && (lex == C_RPAREN || lex == C_COMMA))
                        break;
                    if (lex == C_LPAREN)
                        level++;
                    if (lex == C_RPAREN)
                        level--;
                    argument[c].push_back(new lexical_element(lex, value, name));
                    get_lex();
                }
                if (lex == C_COMMA && c + 1 < total_arguments) {
                    get_lex();
                    c++;
                    continue;
                }
                if (lex == C_RPAREN && c + 1 == total_arguments) {
                    get_lex();
                    break;
                }
                emit_error("syntax error in call to FN");
                break;
            }
        }
        // Push macro into lexical analyzer
        explorer = macros[function]->definition.begin();
        while (explorer != macros[function]->definition.end()) {
            if ((*explorer)->get_lex() == C_ERR) {
                explorer2 = argument[(*explorer)->get_value()].begin();
                while (explorer2 != argument[(*explorer)->get_value()].end()) {
                    accumulated.push_back(new lexical_element((*explorer2)->get_lex(), (*explorer2)->get_value(), (*explorer2)->get_name()));
                    ++explorer2;
                }
            } else {
                accumulated.push_back(new lexical_element((*explorer)->get_lex(), (*explorer)->get_value(), (*explorer)->get_name()));
            }
            ++explorer;
        }
        accumulated.push_back(new lexical_element(lex, value, name));  // The actual one for later
        lex = accumulated.front()->get_lex();
        value = accumulated.front()->get_value();
        name = accumulated.front()->get_name();
        accumulated.pop_front();
        macros[function]->in_use = false;
        return 0;
    }
    
    //
    // Assignment processing
    //
    void compile_assignment(int is_read)
    {
        if (lex != C_NAME) {
            emit_error("name required for assignment");
            return;
        }
        if (sneak_peek() == '(') {
            class node *tree;
            class node *tree2;
            int temp;
            int bits;
            
            if (arrays[name] == 0) {
                emit_error("using array without previous DIM, autoassigning DIM(10)");
                arrays[name] = 10 | (next_label++ << 16);
            }
            temp = arrays[name] >> 16;
            if (name[0] == '#')
                bits = 1;
            else
                bits = 0;
            get_lex();
            if (lex != C_LPAREN)
                emit_error("missing left parenthesis in array access");
            else
                get_lex();
            tree = eval_level0();
            if (lex != C_RPAREN)
                emit_error("missing right parenthesis in array access");
            else
                get_lex();
            if (is_read) {
                tree2 = new node(C_READ, 0, NULL, NULL);
            } else {
                if (lex != C_EQUAL)
                    emit_error("required '=' for assignment");
                else
                    get_lex();
                tree2 = eval_level0();
            }
            tree = new node(C_ASSIGN, bits, tree2,
                            new node(C_PLUS, 0,
                                        new node(C_NAME_RO, temp, NULL, NULL), tree));
            tree->label();
            optimized = false;
            tree->generate(0, 0);
            delete tree;
            tree = NULL;
            return;
        }
        read_write[name] = (read_write[name] | 2);
        if (variables[name] == 0)
            variables[name] = next_var++;
        assigned = name;
        get_lex();
        if (is_read) {
            output->emit_lr(N_MVI, "_read", -1, 4);
            output->emit_rr(N_MVIA, 4, 0);
            output->emit_rl(N_MVO, 4, "_read", -1);
        } else {
            if (lex != C_EQUAL) {
                emit_error("required '=' for assignment");
                return;
            }
            get_lex();
            eval_expr(0, 0);
        }
        if (assigned[0] == '#')
            output->emit_rl(N_MVO, 0, VAR_PREFIX, variables[assigned]);
        else
            output->emit_rlo8(N_MVO, 0, VAR_PREFIX, variables[assigned], 0);
    }
    
    //
    // Process a BASIC statement
    //
    void compile_statement(void)
    {
        while (1) {
            if (lex == C_NAME) {
                last_is_return = 0;
                if (name == "ELSE")
                    break;
                if (name == "GOTO") {
                    get_lex();
                    if (lex != C_NAME) {
                        emit_error("bad syntax for GOTO");
                    } else {
                        if (labels[name] == 0) {
                            labels[name] = next_label;
                            next_label++;
                        }
                        output->emit_a(N_B, LABEL_PREFIX, labels[name]);
                        get_lex();
                    }
                } else if (name == "GOSUB") {
                    get_lex();
                    if (lex != C_NAME) {
                        emit_error("bad syntax for GOSUB");
                    } else {
                        if (labels[name] == 0) {
                            labels[name] = next_label;
                            next_label++;
                        }
                        output->emit_a(N_CALL, LABEL_PREFIX, labels[name]);
                        get_lex();
                    }
                } else if (name == "RETURN") {
                    get_lex();
                    output->emit(N_RETURN);
                    last_is_return = 1;
                } else if (name == "IF") {
                    int label;
                    int there_is_else;
                    int label2;
                    enum lexical_component type;
                    
                    get_lex();
                    label = next_local++;
                    type = eval_expr(0, label);
					if (!optimized) {
                        output->emit_r(N_TSTR, 0);
                        output->emit_a(N_BEQ, TEMP_PREFIX, label);
					}
                    if (lex == C_NAME && name == "GOTO") {
                        compile_statement();
                    } else if (lex != C_NAME || name != "THEN") {
                        emit_error("missing ELSE in IF");
                    } else {
                        get_lex();
                        compile_statement();
                    }
                    if (lex == C_NAME && name == "ELSE") {
                        there_is_else = 1;
                        get_lex();
                        label2 = next_local++;
                        output->emit_a(N_B, TEMP_PREFIX, label2);
                    } else {
                        there_is_else = 0;
                    }
                    output->emit_l3(TEMP_PREFIX, label);
                    if (there_is_else) {
                        compile_statement();
                        output->emit_l(TEMP_PREFIX, label2);
                    }
                    last_is_return = 0;  // Solves bug where last internal statement was RETURN
                } else if (name == "FOR") {  // FOR loop
                    int label1;
                    string loop;
                    class node *final = NULL;
                    class node *step = NULL;
					struct loop new_loop;
                    bool positive;
                    
                    get_lex();
                    compile_assignment(0);
                    loop = assigned;
                    read_write[assigned] = (read_write[assigned] | 1);  // Take note it's used
                    label1 = next_local++;
                    output->emit_l(TEMP_PREFIX, label1);
                    if (lex != C_NAME || name != "TO") {
                        emit_error("missing TO in FOR");
                    } else {
                        get_lex();
                        final = eval_level0();
                        positive = true;
                        if (lex == C_NAME && name == "STEP") {
                            get_lex();
                            if (lex == C_MINUS) {
                                get_lex();
                                step = eval_level0();
                                step = new node(C_MINUS, 0,
                                                new node(C_NAME, variables[loop], 0, 0), step);
                                positive = false;
                            } else {
                                step = eval_level0();
                                step = new node(C_PLUS, 0,
                                                new node(C_NAME, variables[loop], 0, 0), step);
                            }
                        } else {
                            step = new node(C_NUM, 1, NULL, NULL);
                            step = new node(C_PLUS, 0,
                                            new node(C_NAME, variables[loop], 0, 0), step);
                        }
                        final = new node(positive ? C_GREATER : C_LESS, 0, new node(C_NAME, variables[loop], 0, 0), final);
                    }
                    new_loop.type = 0;
					new_loop.step = step;
					new_loop.final = final;
					new_loop.var = loop;
					new_loop.label = label1;
                    loops.push_front(new_loop);
                } else if (name == "NEXT") {
                    get_lex();
                    if (loops.size() == 0) {
                        emit_error("NEXT without FOR");
                    } else {
                        class node *final = loops.front().final;
                        class node *step = loops.front().step;
                        int label1 = loops.front().label;
                        string loop = loops.front().var;
                        
                        if (loops.front().type != 0) {
                            emit_error("bad nested NEXT");
                            if (lex == C_NAME)
                                get_lex();
                        } else {
                            if (lex == C_NAME) {
                                if (loops.size() > 0 && name != loops.front().var)
                                    emit_error("bad nested NEXT");
                                get_lex();
                            }
                            if (step != NULL) {
                                step->label();
                                step->generate(0, 0);
                            }
                            // This gives us better optimization and solves a bug ;)
//                            if (loop[0] == '#')
                                output->emit_rl(N_MVO, 0, VAR_PREFIX, variables[loop]);
//                            else
//                                output->emit_rlo8(N_MVO, 0, VAR_PREFIX, variables[loop], 0);
                            if (final != NULL) {
                                final->label();
                                optimized = false;
                                final->generate(0, label1);
                                delete final;
                                final = NULL;
                            }
                            if (step != NULL) {
                                delete step;
                                step = NULL;
                            }
                            loops.pop_front();
                        }
                    }
                } else if (name == "WHILE") {  // WHILE loop
                    int label1;
                    int label2;
                    enum lexical_component type;
					struct loop new_loop;
                    
                    get_lex();
                    label1 = next_local++;
                    label2 = next_local++;
                    output->emit_l(TEMP_PREFIX, label1);
                    type = eval_expr(0, label2);
					if (!optimized) {
                        output->emit_r(N_TSTR, 0);
                        output->emit_a(N_BEQ, TEMP_PREFIX, label2);
					}
                    new_loop.type = label2;
					new_loop.step = NULL;
					new_loop.final = NULL;
					new_loop.var = "";
					new_loop.label = label1;
                    loops.push_front(new_loop);
                } else if (name == "WEND") {
                    get_lex();
                    if (loops.size() == 0) {
                        emit_error("WEND without WHILE");
                    } else {
                        int label1 = loops.front().label;
                        
                        if (loops.front().type == 0) {
                            emit_error("bad nested WEND");
                        } else {
                            output->emit_a(N_B, TEMP_PREFIX, label1);
                            output->emit_l(TEMP_PREFIX, loops.front().type);
                            loops.pop_front();
                        }
                    }
                } else if (name == "POKE") {
                    get_lex();
                    eval_expr(0, 0);
                    output->emit_r(N_PSHR, 0);
                    if (lex != C_COMMA)
                        emit_error("missing comma in POKE");
                    else
                        get_lex();
                    eval_expr(0, 0);
                    output->emit_r(N_PULR, 4);
                    output->emit_rr(N_MVOA, 0, 4);
                    
                } else if (name == "REM") {
                    line_pos = line_size;
                    get_lex();
                } else if (name == "CLS") {
                    get_lex();
                    output->emit_a(N_CALL, "CLRSCR", -1);
                    output->emit_nr(N_MVII, "", 0x200, 0);
                    output->emit_rl(N_MVO, 0, "_screen", -1);
                } else if (name == "WAIT") {
                    get_lex();
                    output->emit_a(N_CALL, "_wait", -1);
                } else if (name == "RESTORE") {
                    get_lex();
                    if (lex != C_NAME) {
                        emit_error("bad syntax for RESTORE");
                    } else {
                        if (labels[name] == 0) {
                            labels[name] = next_label;
                            next_label++;
                        }
                        output->emit_nr(N_MVII, LABEL_PREFIX, labels[name], 4);
                        output->emit_rl(N_MVO, 4, "_read", -1);
                        get_lex();
                    }
                } else if (name == "READ") {
                    get_lex();
                    while (1) {
                        compile_assignment(1);
                        if (lex != C_COMMA)
                            break;
                        get_lex();
                    }
                } else if (name == "DATA") {
                    get_lex();
                    while (1) {
                        class node *tree;
                        
                        tree = eval_level0();
                        if (tree->node_type() != C_NUM) {
                            emit_error("not a constant expression in DATA");
                            break;
                        }
                        output->emit_d(N_DECLE, tree->node_value());
                        delete tree;
                        tree = NULL;
                        if (lex != C_COMMA)
                            break;
                        get_lex();
                    }
                } else if (name == "DEFINE") {
                    int label;
                    
                    get_lex();
                    if (lex == C_NAME && name == "ALTERNATE") {
                        get_lex();
                        eval_expr(0, 0);
                        output->emit_rl(N_MVO, 0, "_gram2_target", -1);
                        if (lex != C_COMMA)
                            emit_error("missing comma for DEFINE");
                        else
                            get_lex();
                        eval_expr(0, 0);
                        output->emit_rl(N_MVO, 0, "_gram2_total", -1);
                        if (lex != C_COMMA)
                            emit_error("missing comma for DEFINE");
                        else
                            get_lex();
                        if (lex != C_NAME) {
                            emit_error("bad syntax for DEFINE");
                        } else {
                            if (name == "VARPTR") {
                                eval_expr(0, 0);
                            } else {
                                if (arrays[name] != 0) {
                                    label = arrays[name] >> 16;
                                } else if (labels[name] == 0) {
                                    label = labels[name] = next_label;
                                    next_label++;
                                } else {
                                    label = labels[name];
                                }
                                output->emit_nr(N_MVII, LABEL_PREFIX, label, 0);
                            }
                            output->emit_rl(N_MVO, 0, "_gram2_bitmap", -1);
                            get_lex();
                        }
                    } else {
                        eval_expr(0, 0);
                        output->emit_rl(N_MVO, 0, "_gram_target", -1);
                        if (lex != C_COMMA)
                            emit_error("missing comma for DEFINE");
                        else
                            get_lex();
                        eval_expr(0, 0);
                        output->emit_rl(N_MVO, 0, "_gram_total", -1);
                        if (lex != C_COMMA)
                            emit_error("missing comma for DEFINE");
                        else
                            get_lex();
                        if (lex != C_NAME) {
                            emit_error("bad syntax for DEFINE");
                        } else {
                            if (name == "VARPTR") {
                                eval_expr(0, 0);
                            } else {
                                if (arrays[name] != 0) {
                                    label = arrays[name] >> 16;
                                } else if (labels[name] == 0) {
                                    label = labels[name] = next_label;
                                    next_label++;
                                } else {
                                    label = labels[name];
                                }
                                output->emit_nr(N_MVII, LABEL_PREFIX, label, 0);
                            }
                            output->emit_rl(N_MVO, 0, "_gram_bitmap", -1);
                            get_lex();
                        }
                    }
                } else if (name == "SOUND") {
                    int channel;
                    
                    get_lex();
                    if (lex != C_NUM) {
                        emit_error("bad syntax for SOUND");
                        channel = 0;
                    } else {
                        channel = value;
                        if (channel < 0 || channel > 9)
                            emit_error("bad channel for SOUND");
                        get_lex();
                        if (channel >= 5)
                            ecs_used = true;
                    }
                    if (lex != C_COMMA) {
                        emit_error("bad syntax for SOUND");
                    } else {
                        get_lex();
                    }
                    if (lex != C_COMMA) {
                        eval_expr(0, 0);
                        if (channel == 0)
                            output->emit_rl(N_MVO, 0, "", 0x01f0);
                        if (channel == 1)
                            output->emit_rl(N_MVO, 0, "", 0x01f1);
                        if (channel == 2)
                            output->emit_rl(N_MVO, 0, "", 0x01f2);
                        if (channel == 3)
                            output->emit_rl(N_MVO, 0, "", 0x01f3);
                        if (channel == 4)
                            output->emit_rl(N_MVO, 0, "", 0x01f9);
                        if (channel == 5)
                            output->emit_rl(N_MVO, 0, "", 0x00f0);
                        if (channel == 6)
                            output->emit_rl(N_MVO, 0, "", 0x00f1);
                        if (channel == 7)
                            output->emit_rl(N_MVO, 0, "", 0x00f2);
                        if (channel == 8)
                            output->emit_rl(N_MVO, 0, "", 0x00f3);
                        if (channel == 9)
                            output->emit_rl(N_MVO, 0, "", 0x00f9);
                        if ((channel >= 0 && channel <= 3) || (channel >= 5 && channel <= 8))
                            output->emit_r(N_SWAP, 0);
                        if (channel == 0)
                            output->emit_rl(N_MVO, 0, "", 0x01f4);
                        if (channel == 1)
                            output->emit_rl(N_MVO, 0, "", 0x01f5);
                        if (channel == 2)
                            output->emit_rl(N_MVO, 0, "", 0x01f6);
                        if (channel == 3)
                            output->emit_rl(N_MVO, 0, "", 0x01f7);
                        if (channel == 5)
                            output->emit_rl(N_MVO, 0, "", 0x00f4);
                        if (channel == 6)
                            output->emit_rl(N_MVO, 0, "", 0x00f5);
                        if (channel == 7)
                            output->emit_rl(N_MVO, 0, "", 0x00f6);
                        if (channel == 8)
                            output->emit_rl(N_MVO, 0, "", 0x00f7);
                    }
                    if (lex == C_COMMA) {
                        get_lex();
                        eval_expr(0, 0);
                        if (channel == 0)
                            output->emit_rl(N_MVO, 0, "", 0x01fb);
                        if (channel == 1)
                            output->emit_rl(N_MVO, 0, "", 0x01fc);
                        if (channel == 2)
                            output->emit_rl(N_MVO, 0, "", 0x01fd);
                        if (channel == 3)
                            output->emit_rl(N_MVO, 0, "", 0x01fa);
                        if (channel == 4)
                            output->emit_rl(N_MVO, 0, "", 0x01f8);
                        if (channel == 5)
                            output->emit_rl(N_MVO, 0, "", 0x00fb);
                        if (channel == 6)
                            output->emit_rl(N_MVO, 0, "", 0x00fc);
                        if (channel == 7)
                            output->emit_rl(N_MVO, 0, "", 0x00fd);
                        if (channel == 8)
                            output->emit_rl(N_MVO, 0, "", 0x00fa);
                        if (channel == 9)
                            output->emit_rl(N_MVO, 0, "", 0x00f8);
                    }
                } else if (name == "SPRITE") {
                    int sprite;
                    
                    get_lex();
                    if (lex != C_NUM) {
                        emit_error("bad syntax for SPRITE");
                        sprite = 0;
                    } else {
                        sprite = value;
                        if (sprite < 0 || sprite > 7)
                            emit_error("bad number (0-7) for SPRITE");
                        get_lex();
                        if (lex != C_COMMA)
                            emit_error("bad syntax for SPRITE");
                        else
                            get_lex();
                    }
                    if (lex != C_COMMA) {
                        eval_expr(0, 0);
                        output->emit_rlo(N_MVO, 0, "_mobs", -1, 0 + sprite);
                    }
                    if (lex == C_COMMA) {
                        get_lex();
                        if (lex != C_COMMA) {
                            eval_expr(0, 0);
                            output->emit_rlo(N_MVO, 0, "_mobs", -1, 8 + sprite);
                        }
                        if (lex == C_COMMA) {
                            get_lex();
                            eval_expr(0, 0);
                            output->emit_rlo(N_MVO, 0, "_mobs", -1, 16 + sprite);
                        }
                    }
                } else if (name == "PRINT") {
                    int start;
                    
                    get_lex();
                    start = 1;
                    if (lex == C_NAME && name == "AT") {
                        class node *final;
                        
                        get_lex();
                        final = eval_level0();
                        final = new node(C_PLUS, 0, final, new node(C_NUM, 0x200, NULL, NULL));
                        final->label();
                        final->generate(0, 0);
                        output->emit_rl(N_MVO, 0, "_screen", -1);
                        start = 0;
                    }
                    if (lex == C_NAME && name == "COLOR") {
                        get_lex();
                        eval_expr(0, 0);
                        output->emit_rl(N_MVO, 0, "_color", -1);
                        start = 0;
                    }
                    while (1) {
                        if (!start) {
                            if (lex != C_COMMA)
                                break;
                            get_lex();
                        }
                        start = 0;
                        if (lex == C_STRING) {
                            int c;
							int p;
                            
                            output->emit_lr(N_MVI, "_screen", -1, 4);
							p = -1;
                            for (c = 0; c < offset; c++) {
								if (pstring[c] * 8 != p) {
                                    if (p != -1) {
                                        if ((p ^ (pstring[c] * 8)) == 0)
                                            output->emit(N_NOP);
                                        else
                                            output->emit_nr(N_XORI, "", (p ^ (pstring[c] * 8)), 0);
                                    } else {
                                        if (pstring[c] == 0) {
                                            output->emit_lr(N_MVI, "_color", -1, 0);
                                        } else {
                                            output->emit_nr(N_MVII, "", (pstring[c] * 8), 0);
                                            output->emit_lr(N_XOR, "_color", -1, 0);
                                        }
									}
                                    p = pstring[c] * 8;
								}
                                output->emit_rr(N_MVOA, 0, 4);
                            }
                            get_lex();
                        } else if (lex == C_LESS || lex == C_NOTEQUAL) {
                            int type = 0;
                            int size = 1;
                            
                            if (lex == C_NOTEQUAL) {
                                get_lex();
                            } else {
                                get_lex();
                                if (lex == C_PERIOD) {
                                    get_lex();
                                    type = 1;
                                    if (lex != C_NUM) {
                                        emit_error("missing size for number");
                                    } else {
                                        size = value;
                                        get_lex();
                                    }
                                } else if (lex == C_NUM) {
                                    type = 2;
                                    if (lex != C_NUM) {
                                        emit_error("missing size for number");
                                    } else {
                                        size = value;
                                        get_lex();
                                    }
                                }
                                if (lex == C_GREATER)
                                    get_lex();
                                else
                                    emit_error("missing > in PRINT for number");
                            }
                            eval_expr(0, 0);
                            if (type != 0)
                                output->emit_nr(N_MVII, "", size, 2);
                            output->emit_lr(N_MVI, "_color", -1, 3);
                            output->emit_lr(N_MVI, "_screen", -1, 4);
                            if (type == 0)
                                output->emit_a(N_CALL, "PRNUM16.l", -1);
                            else if (type == 1)
                                output->emit_a(N_CALL, "PRNUM16.b", -1);
                            else if (type == 2)
                                output->emit_a(N_CALL, "PRNUM16.z", -1);
                            numbers_used = 1;
                        } else {
                            eval_expr(0, 0);
                            output->emit_lr(N_MVI, "_screen", -1, 4);
                            output->emit_rr(N_MVOA, 0, 4);
                        }
                        output->emit_rl(N_MVO, 4, "_screen", -1);
                    }
                } else if (name == "BITMAP") {
                    get_lex();
                    if (lex != C_STRING || offset != 8) {
                        emit_error("syntax error in BITMAP");
                    } else {
                        int c;
                        
                        value = 0;
                        for (c = 0; c < 8; c++) {
                            if (pstring[c] != 0x10 && pstring[c] != 0x3f   // 0 and _
                             && pstring[c] != 0x00 && pstring[c] != 0x0e)  // space and .
                                value |= 0x80 >> c;
                        }
                        get_lex();
                        if (bitmap_byte == 0) {
                            bitmap_value = value;
                            bitmap_byte = 1;
                        } else {
                            bitmap_value |= value << 8;
                            bitmap_byte = 0;
                            output->emit_d(N_DECLE, bitmap_value);
                        }
                    }
                } else if (name == "SCROLL") {
                    get_lex();
                    scroll_used = 1;
                    if (lex != C_COMMA) {
                        eval_expr(0, 0);
                        output->emit_rl(N_MVO, 0, "_scroll_x", -1);
                    }
                    if (lex == C_COMMA) {
                        get_lex();
                        if (lex != C_COMMA) {
                            eval_expr(0, 0);
                            output->emit_rl(N_MVO, 0, "_scroll_y", -1);
                        }
                        if (lex == C_COMMA) {
                            get_lex();
                            eval_expr(0, 0);
                            output->emit_rl(N_MVO, 0, "_scroll_d", -1);
                        }
                    }
                } else if (name == "BORDER") {
                    get_lex();
                    if (lex != C_COMMA) {
                        eval_expr(0, 0);
                        output->emit_rl(N_MVO, 0, "_border_color", -1);
                    }
                    if (lex == C_COMMA) {
                        get_lex();
                        eval_expr(0, 0);
                        output->emit_rl(N_MVO, 0, "_border_mask", -1);
                    }
                } else if (name == "CONST") {
                    get_lex();
                    if (lex != C_NAME) {
                        emit_error("name required for constant assignment");
                        return;
                    }
                    if (constants[name] != 0)
                        emit_error("constant redefined");
                    assigned = name;
                    get_lex();
                    if (lex != C_EQUAL) {
                        emit_error("required '=' for constant assignment");
                    } else {
                        class node *tree;
                        
                        get_lex();
                        tree = eval_level0();
                        if (tree->node_type() != C_NUM) {
                            emit_error("not a constant expression in CONST");
                        } else {
                            constants[assigned] = (tree->node_value() & 0xffff) | 0x10000;
                        }
                        delete tree;
                        tree = NULL;
                    }
                } else if (name == "DIM") {
                    string array;
                    class node *tree = NULL;
                    int c;
                    
                    while (1) {
                        get_lex();
                        if (lex != C_NAME) {
                            emit_error("missing name in DIM");
                            break;
                        }
                        array = name;
                        get_lex();
                        if (lex != C_LPAREN) {
                            emit_error("missing left parenthesis in DIM");
                        } else {
                            get_lex();
                        }
                        tree = eval_level0();
                        if (tree->node_type() != C_NUM) {
                            emit_error("not a constant expression in DIM");
                            break;
                        }
                        c = tree->node_value();
                        if (c <= 0 || c > 65535) {
                            emit_error("invalid dimension in DIM");
                            c = 1;
                        }
                        if (arrays[array] != 0)
                            emit_error("already used name for DIM");
                        else
                            arrays[array] = c | (next_label++ << 16);
                        delete tree;
                        tree = NULL;
                        if (lex != C_RPAREN) {
                            emit_error("missing right parenthesis in DIM");
                        } else {
                            get_lex();
                        }
                        if (lex != C_COMMA)
                            break;
                    }
                } else if (name == "MODE") {    // Video mode selection
                    class node *tree = NULL;
                    class node *tree2 = NULL;
                    int mode;
                    
                    get_lex();
                    tree = eval_level0();
                    if (tree->node_type() != C_NUM) {
                        emit_error("not a constant expression in MODE");
                        break;
                    }
                    mode = tree->node_value();
                    if (mode != 0 && mode != 1) {
                        emit_error("invalid MODE");
                        break;
                    }
                    delete tree;
                    tree = NULL;
                    if (mode == 0) {  // Color Stack mode
                        if (lex != C_COMMA)
                            emit_error("missing comma in MODE");
                        else
                            get_lex();
                        tree = eval_level0();
                        if (lex != C_COMMA)
                            emit_error("missing comma in MODE");
                        else
                            get_lex();
                        tree2 = eval_level0();
                        tree2 = new node(C_MUL, 0, tree2, new node(C_NUM, 0x100, NULL, NULL));
                        tree = new node(C_PLUS, 0, tree, tree2);
                        if (lex != C_COMMA)
                            emit_error("missing comma in MODE");
                        else
                            get_lex();
                        tree2 = eval_level0();
                        tree2 = new node(C_MUL, 0, tree2, new node(C_NUM, 0x1000, NULL, NULL));
                        tree = new node(C_PLUS, 0, tree, tree2);
                        if (lex != C_COMMA)
                            emit_error("missing comma in MODE");
                        else
                            get_lex();
                        tree2 = eval_level0();
                        tree2 = new node(C_MUL, 0, tree2, new node(C_NUM, 0x10, NULL, NULL));
                        tree = new node(C_PLUS, 0, tree, tree2);
                        tree->label();
                        tree->generate(0, 0);
                        delete tree;
                        tree = NULL;
                        output->emit_rl(N_MVO, 0, "_color", -1);
                        output->emit_nr(N_MVII, "", 1, 0);
                        output->emit_rl(N_MVO, 0, "_mode_select", -1);
                    } else {    // Foreground/Background mode
                        output->emit_nr(N_MVII, "", 2, 0);
                        output->emit_rl(N_MVO, 0, "_mode_select", -1);
                    }
                } else if (name == "SCREEN") {  // Copy screen
                    int label;
                    
                    get_lex();
                    if (lex != C_NAME) {
                        emit_error("bad syntax for SCREEN");
                        break;
                    }
                    assigned = name;
                    if (arrays[name] != 0) {
                        label = arrays[name] >> 16;
                    } else if (labels[name] == 0) {
                        label = labels[name] = next_label;
                        next_label++;
                    } else {
                        label = labels[name];
                    }
                    get_lex();
                    if (lex == C_COMMA) {
                        get_lex();
                        eval_expr(0, 0);
                        output->emit_nr(N_ADDI, LABEL_PREFIX, label, 0);
                        output->emit_r(N_PSHR, 0);
                        if (lex != C_COMMA) {
                            emit_error("missing comma after second parameter in SCREEN");
                            break;
                        }
                        get_lex();
                        eval_expr(0, 0);
                        output->emit_nr(N_ADDI, "", 0x0200, 0);
                        output->emit_r(N_PSHR, 0);
                        if (lex != C_COMMA) {
                            emit_error("missing comma after third parameter in SCREEN");
                            break;
                        }
                        get_lex();
                        eval_expr(0, 0);
                        output->emit_r(N_PSHR, 0);
                        if (lex != C_COMMA) {
                            emit_error("missing comma after fourth parameter in SCREEN");
                            break;
                        }
                        get_lex();
                        eval_expr(0, 0);
                        output->emit_r(N_PULR, 1);
                        output->emit_r(N_PULR, 2);
                        output->emit_r(N_PULR, 3);
                        output->emit_a(N_CALL, "CPYBLK", -1);
                    } else {
                        output->emit_nr(N_MVII, LABEL_PREFIX, label, 3);
                        output->emit_nr(N_MVII, "", 0x200, 2);
                        output->emit_nr(N_MVII, "", 20, 1);
                        output->emit_nr(N_MVII, "", 12, 0);
                        output->emit_a(N_CALL, "CPYBLK", -1);
                    }
                } else if (name == "PLAY") {
                    int label;
                    
                    get_lex();
                    if (lex != C_NAME) {
                        emit_error("bad syntax for PLAY");
                        break;
                    }
                    music_used = 1;
                    if (name == "OFF") {
                        output->emit_r(N_CLRR, 0);
                        output->emit_a(N_CALL, "_play_music", -1);
                    } else if (name == "NONE") {
                        output->emit_r(N_CLRR, 3);
                        output->emit_rl(N_MVO, 3, "_music_mode", -1);
                    } else if (name == "SIMPLE") {
                        output->emit_nr(N_MVII, "", 1, 3);
                        output->emit_rl(N_MVO, 3, "_music_mode", -1);
                    } else if (name == "FULL") {
                        output->emit_nr(N_MVII, "", 2, 3);
                        output->emit_rl(N_MVO, 3, "_music_mode", -1);
                    } else {
                        if (arrays[name] != 0) {
                            label = arrays[name] >> 16;
                        } else if (labels[name] == 0) {
                            label = labels[name] = next_label;
                            next_label++;
                        } else {
                            label = labels[name];
                        }
                        output->emit_nr(N_MVII, LABEL_PREFIX, label, 0);
                        output->emit_a(N_CALL, "_play_music", -1);
                    }
                    get_lex();
                } else if (name == "MUSIC") {
                    int arg;
                    static int previous[4];
                    unsigned int notes;
                    int note;
                    int c;
                    
                    get_lex();
                    notes = 0;
                    arg = 0;
                    while (1) {
                        if (lex != C_NAME && lex != C_MINUS) {
                            emit_error("bad syntax for MUSIC");
                            break;
                        }
                        if (lex == C_MINUS) {
                            // Nothing to do
                        } else if (arg == 0 && name == "REPEAT") {
                            get_lex();
                            notes = 0xfd;
                            break;
                        } else if (arg == 0 && name == "STOP") {
                            get_lex();
                            notes = 0xfe;
                            break;
						} else if (arg == 3) {
							if (name[0] != 'M' || name[1] < '1' || name[1] > '3') {
								emit_error("bad syntax for drum in MUSIC");
								break;
							}
                            notes |= (name[1] - '0') << (arg * 8);
                        } else if (name == "S") {
                            notes |= 0x3f << (arg * 8);
						} else {
                            notes |= previous[arg] << (arg * 8);
                            c = 0;
                            switch (name[c++]) {
                                case 'C': note = 0; break;
                                case 'D': note = 2; break;
                                case 'E': note = 4; break;
                                case 'F': note = 5; break;
                                case 'G': note = 7; break;
                                case 'A': note = 9; break;
                                case 'B': note = 11; break;
                                default: emit_error("bad syntax for note in MUSIC"); break;
                            }
                            switch (name[c++]) {
                                case '2': note += 0 * 12; break;
                                case '3': note += 1 * 12; break;
                                case '4': note += 2 * 12; break;
                                case '5': note += 3 * 12; break;
                                case '6': note += 4 * 12; break;
                                case '7': if (note == 0) { note += 5 * 12; break; }
                                default: emit_error("bad syntax for note in MUSIC"); break;
                            }
                            note++;
                            if (name[c] == '#') {
                                note++;
                                c++;
                            }
                            if (name[c] == 'W') {
                                previous[arg] = 0x00;
                                notes &= ~(0xc0 << (arg * 8));
                                notes |= previous[arg] << (arg * 8);
                            } else if (name[c] == 'X') {
                                previous[arg] = 0x40;
                                notes &= ~(0xc0 << (arg * 8));
                                notes |= previous[arg] << (arg * 8);
                            } else if (name[c] == 'Y') {
                                previous[arg] = 0x80;
                                notes &= ~(0xc0 << (arg * 8));
                                notes |= previous[arg] << (arg * 8);
                            } else if (name[c] == 'Z') {
                                previous[arg] = 0xc0;
                                notes &= ~(0xc0 << (arg * 8));
                                notes |= previous[arg] << (arg * 8);
                            }
                            notes |= note << (arg * 8);
                        }
                        get_lex();
                        arg++;
                        if (lex != C_COMMA)
                            break;
                        if (arg == 4) {
                            emit_error("too many arguments for MUSIC");
                            break;
                        }
                        get_lex();
                    }
                    output->emit_d2(N_DECLE, (notes & 0xffff), (notes >> 16 & 0xffff));
                } else if (name == "ON") {
                    int label;
                    int table;
                    int c;
                    int max_value;
                    int gosub;
                    int options[256];
                    
                    get_lex();
                    if (lex == C_NAME && name == "FRAME") {  // Frame-driven games
                        get_lex();
                        if (lex != C_NAME || name != "GOSUB") {
                            emit_error("Bad syntax for ON FRAME GOSUB");
                        }
                        get_lex();
                        if (lex != C_NAME) {
                            emit_error("Missing label for ON FRAME GOSUB");
                        }
                        if (frame_drive >= 0) {
                            emit_error("More than one ON FRAME GOSUB");
                        }
                        if (labels[name] == 0) {
                            labels[name] = next_label;
                            next_label++;
                        }
                        frame_drive = labels[name];
                        get_lex();
                    } else {
                        eval_expr(0, 0);
                        gosub = 0;
                        if (lex != C_NAME || (name != "GOTO" && name != "GOSUB")) {
                            emit_error("required GOTO or GOSUB after ON");
                        } else if (name == "GOTO") {
                            get_lex();
                        } else if (name == "GOSUB") {
                            get_lex();
                            gosub = 1;
                        }
                        max_value = 0;
                        while (1) {
                            if (max_value == sizeof(options) / sizeof(int)) {
                                emit_error("too many options for ON statement");
                                max_value--;
                            }
                            if (lex == C_NAME) {
                                if (labels[name] == 0) {
                                    labels[name] = next_label;
                                    next_label++;
                                }
                                options[max_value++] = labels[name];
                                get_lex();
                            } else {
                                options[max_value++] = 0;
                            }
                            if (lex != C_COMMA)
                                break;
                            get_lex();
                        }
                        table = next_local++;
                        label = next_local++;
                        output->emit_nr(N_CMPI, "", max_value, 0);
                        output->emit_a(N_BC, TEMP_PREFIX, label);
                        if (gosub)
                            output->emit_nr(N_MVII, TEMP_PREFIX, label, 5);
                        output->emit_nr(N_ADDI, TEMP_PREFIX, table, 0);
                        output->emit_rr(N_MOVR, 0, 1);
                        output->emit_rr(N_MVIA, 1, 7);
                        
                        output->emit_l(TEMP_PREFIX, table);
                        for (c = 0; c < max_value; c++) {
                            if (options[c])
                                output->emit_dl(N_DECLE, LABEL_PREFIX, options[c]);
                            else
                                output->emit_dl(N_DECLE, TEMP_PREFIX, label);
                        }
                        output->emit_l(TEMP_PREFIX, label);
                    }
                } else if (name == "VOICE") {  // Intellivoice support
                    int label;
                    
                    get_lex();
                    if (lex == C_NAME && name == "PLAY") {
                        voice_used = 1;
                        get_lex();
                        if (lex == C_NAME && name == "WAIT") {
                            get_lex();
                            if (lex != C_NAME) {
                                emit_error("bad syntax for VOICE PLAY WAIT");
                                break;
                            }
                            if (arrays[name] != 0) {
                                label = arrays[name] >> 16;
                            } else if (labels[name] == 0) {
                                label = labels[name] = next_label;
                                next_label++;
                            } else {
                                label = labels[name];
                            }
                            output->emit_nr(N_MVII, LABEL_PREFIX, label, 0);
                            output->emit_a(N_CALL, "IV_PLAYW.1", -1);
                            get_lex();
                        } else {
                            if (lex != C_NAME) {
                                emit_error("bad syntax for VOICE PLAY");
                                break;
                            }
                            if (arrays[name] != 0) {
                                label = arrays[name] >> 16;
                            } else if (labels[name] == 0) {
                                label = labels[name] = next_label;
                                next_label++;
                            } else {
                                label = labels[name];
                            }
                            output->emit_nr(N_MVII, LABEL_PREFIX, label, 0);
                            output->emit_a(N_CALL, "IV_PLAY.1", -1);
                            get_lex();
                        }
                    } else if (lex == C_NAME && name == "INIT") {
                        voice_used = 1;
                        get_lex();
                        output->emit_a(N_CALL, "IV_INIT", -1);
                    } else if (lex == C_NAME && name == "WAIT") {
                        voice_used = 1;
                        get_lex();
                        output->emit_a(N_CALL, "IV_WAIT", -1);
                    } else if (lex == C_NAME && name == "NUMBER") {
                        voice_used = 1;
                        get_lex();
                        eval_expr(0, 0);
                        output->emit_a(N_CALL, "IV_SAYNUM16", -1);
                    } else {
                        while (1) {
                            if (lex == C_NUM) {
                                output->emit_d(N_DECLE, value);
                                get_lex();
                            } else {
                                if (lex != C_NAME) {
                                    emit_error("bad syntax for VOICE");
                                    break;
                                }
                                if (name == "PA5") {
                                    output->emit_d(N_DECLE, 1);
                                } else if (name == "PA4") {
                                    output->emit_d(N_DECLE, 2);
                                } else if (name == "PA3") {
                                    output->emit_d(N_DECLE, 3);
                                } else if (name == "PA2") {
                                    output->emit_d(N_DECLE, 4);
                                } else if (name == "PA1") {
                                    output->emit_d(N_DECLE, 5);
                                } else if (name == "MATTEL") {
                                    output->emit_d(N_DECLE, 6);
                                } else if (name == "ZERO") {
                                    output->emit_d(N_DECLE, 7);
                                } else if (name == "ONE") {
                                    output->emit_d(N_DECLE, 8);
                                } else if (name == "TWO") {
                                    output->emit_d(N_DECLE, 9);
                                } else if (name == "THREE") {
                                    output->emit_d(N_DECLE, 10);
                                } else if (name == "FOUR") {
                                    output->emit_d(N_DECLE, 11);
                                } else if (name == "FIVE") {
                                    output->emit_d(N_DECLE, 12);
                                } else if (name == "SIX") {
                                    output->emit_d(N_DECLE, 13);
                                } else if (name == "SEVEN") {
                                    output->emit_d(N_DECLE, 14);
                                } else if (name == "EIGHT") {
                                    output->emit_d(N_DECLE, 15);
                                } else if (name == "NINE") {
                                    output->emit_d(N_DECLE, 16);
                                } else if (name == "TEN") {
                                    output->emit_d(N_DECLE, 17);
                                } else if (name == "ELEVEN") {
                                    output->emit_d(N_DECLE, 18);
                                } else if (name == "TWELVE") {
                                    output->emit_d(N_DECLE, 19);
                                } else if (name == "THIRTEEN") {
                                    output->emit_d(N_DECLE, 20);
                                } else if (name == "FOURTEEN") {
                                    output->emit_d(N_DECLE, 21);
                                } else if (name == "FIFTEEN") {
                                    output->emit_d(N_DECLE, 22);
                                } else if (name == "SIXTEEN") {
                                    output->emit_d(N_DECLE, 23);
                                } else if (name == "SEVENTEEN") {
                                    output->emit_d(N_DECLE, 24);
                                } else if (name == "EIGHTEEN") {
                                    output->emit_d(N_DECLE, 25);
                                } else if (name == "NINETEEN") {
                                    output->emit_d(N_DECLE, 26);
                                } else if (name == "TWENTY") {
                                    output->emit_d(N_DECLE, 27);
                                } else if (name == "THIRTY") {
                                    output->emit_d(N_DECLE, 28);
                                } else if (name == "FOURTY") {
                                    output->emit_d(N_DECLE, 29);
                                } else if (name == "FIFTY") {
                                    output->emit_d(N_DECLE, 30);
                                } else if (name == "SIXTY") {
                                    output->emit_d(N_DECLE, 31);
                                } else if (name == "SEVENTY") {
                                    output->emit_d(N_DECLE, 32);
                                } else if (name == "EIGHTY") {
                                    output->emit_d(N_DECLE, 33);
                                } else if (name == "NINETY") {
                                    output->emit_d(N_DECLE, 34);
                                } else if (name == "HUNDRED") {
                                    output->emit_d(N_DECLE, 35);
                                } else if (name == "THOUSAND") {
                                    output->emit_d(N_DECLE, 36);
                                } else if (name == "TEEN") {
                                    output->emit_d(N_DECLE, 37);
                                } else if (name == "TY") {
                                    output->emit_d(N_DECLE, 38);
                                } else if (name == "PRESS") {
                                    output->emit_d(N_DECLE, 39);
                                } else if (name == "ENTER") {
                                    output->emit_d(N_DECLE, 40);
                                } else if (name == "OR") {
                                    output->emit_d(N_DECLE, 41);
                                } else if (name == "AND") {
                                    output->emit_d(N_DECLE, 42);
                                } else {
                                    name = "_" + name;
                                    output->emit_dl(N_DECLE, name, -1);
                                }
                            }
                            get_lex();
                            if (lex != C_COMMA)
                                break;
                            get_lex();
                        }
                    }
                } else if (name == "STACK_CHECK") {  // Stack overflow check
                    get_lex();
                    stack_used = 1;
                } else if (name == "ASM") {         // ASM statement for inserting assembly code
                    size_t c;
                    
                    c = line_pos;
                    while (c < line_size && isspace(line[c]))
                        c++;
                    while (c < line_size && !isspace(line[c]))
                        c++;
                    if (line[c - 1] == ':')
                        skip_spaces();
                    output->emit_literal(line.substr(line_pos, line_size - line_pos));
                    line_pos = line_size;
                    get_lex();
                } else if (name == "DEF") {     // Function definition (macro in IntyBASIC)
                    string function;
                    int total_arguments;
                    map <string, int> arguments;
                    
                    get_lex();
                    if (lex != C_NAME || name != "FN") {
                        emit_error("syntax error for DEF FN");
                    } else {
                        get_lex();
                        if (lex != C_NAME) {
                            emit_error("missing function name for DEF FN");
                        } else if (macros[name] != NULL) {
                            emit_error("DEF FN name already defined");
                        } else {
                            function = name;
                            get_lex();
                            total_arguments = 0;
                            if (lex == C_LPAREN) {
                                get_lex();
                                while (1) {
                                    if (lex != C_NAME) {
                                        emit_error("syntax error in argument list for DEF FN");
                                        break;
                                    }
                                    arguments[name] = ++total_arguments;
                                    get_lex();
                                    if (lex == C_COMMA) {
                                        get_lex();
                                    } else if (lex == C_RPAREN) {
                                        get_lex();
                                        break;
                                    } else {
                                        emit_error("syntax error in argument list for DEF FN");
                                        break;
                                    }
                                }
                            }
                            macros[function] = new struct macro;
                            macros[function]->total_arguments = total_arguments;
                            macros[function]->in_use = NULL;
                            if (lex != C_EQUAL) {
                                emit_error("missing = in DEF FN");
                            } else {
                                get_lex();
                                while (lex != C_END) {
                                    if (lex == C_ERR) {
                                        emit_error("bad syntax inside DEF FN replacement text");
                                        break;
                                    }
                                    if (lex == C_STRING) {
                                        emit_error("strings not accepted in DEF FN");
                                        break;
                                    }
                                    if (lex == C_NAME && arguments[name] != 0) {  // Checks for argument
                                        lex = C_ERR;
                                        value = arguments[name] - 1;
                                    }
                                    macros[function]->definition.push_back(new lexical_element(lex, value, name));
                                    get_lex();
                                }
                            }
                        }
                    }
                } else if (macros[name] != NULL) {  // Function (macro)
                    if (!replace_macro()) {
                        compile_statement();
                        return;
                    }
                } else {
                    compile_assignment(0);
                }
            } else {
                last_is_return = 0;
                emit_error("syntax error in statement");
            }
            if (lex != C_COLON)
                break;
            get_lex();
        }
    }
    
public:
    //
    // Starts compilation
    //
    int start(const char *input_file, const char *output_file, const char *library_path, int flags) {
        int used_space;
        int available_vars;
        int inside_proc;
        char path[4096];  // For Windows and Linux
        char *p;
        //char path[PATH_MAX];  // Only works in Mac OS X :/
        
		line_number = 0;
		next_label = 1;
		next_var = 1;
        scroll_used = 0;
        keypad_used = 0;
        music_used = 0;
        stack_used = 0;
        numbers_used = 0;
        voice_used = 0;
        ecs_used = false;
        jlp_used = ((flags & 1) != 0);
        cc3_used = ((flags & 2) != 0);
        warnings = ((flags & 4) == 0);
        fastmult_used = false;
        fastdiv_used = false;
        frame_drive = -1;
        active_include = 0;
        next_include = 0;
        err_code = 0;
        input.open(input_file);
        if (!input.is_open()) {
			std::cerr << "Unable to open input file: " << input_file << "\n";
            return 2;
        }
        output = new code;
        asm_output.open(output_file);
        if (!asm_output.is_open()) {
			std::cerr << "Unable to open output file: " << output_file << "\n";
            input.close();
            return 2;
        }
        asm_output << "\t; IntyBASIC compiler " << VERSION << "\n";
        if (jlp_used || voice_used) {
            asm_output << "\tIF DEFINED __FEATURE.CFGVAR\n";
            if (jlp_used)
                asm_output << "\t\tCFGVAR \"jlp\" = 1\n";
            if (voice_used)
                asm_output << "\t\tCFGVAR \"voice\" = 1\n";
            if (ecs_used)
                asm_output << "\t\tCFGVAR \"ecs\" = 1\n";
            asm_output << "\tENDI\n";
        }
        strcpy(path, library_path);
#ifdef _WIN32
        if (strlen(path) > 0 && path[strlen(path) - 1] != '\\')
            strcat(path, "\\");
#else
        if (strlen(path) > 0 && path[strlen(path) - 1] != '/')
            strcat(path, "/");
#endif
        strcat(path, "intybasic_prologue.asm");
        included.open(path);
        if (included.is_open()) {
            while (getline(included, line)) {
                if (line.find(";IntyBASIC MARK DON'T CHANGE") != string::npos) {  // Location to replace title
                    asm_output << "\tBYTE " << program_year << ",'" << program_title << "',0\n";
                } else {
                    asm_output << line << "\n";
                }
            }
            included.close();
		} else {
			std::cerr << "Error: unable to include: " << path << "\n";
            err_code = 2;
        }
        asm_output << "\t;FILE " << input_file << "\n";
        bitmap_byte = 0;
        inside_proc = 0;
        arrays["#MOBSHADOW"] = 24 | (next_label++ << 16);  // #MOBSHADOW array
        while (1) {
            int label_exists;
            
            if (active_include) {
                if (!getline(include[next_include], line)) {
                    include[next_include].close();
                    asm_output << "\t;ENDFILE\n";
                    next_include++;
                    active_include = 0;
                    line_number = saved_line_number;
                    asm_output << "\t;FILE " << input_file << "\n";
                }
            }
            if (!active_include) {
                if (!getline(input, line))
                    break;
            }
            line_number++;
			line_start = 1;
            line_pos = 0;
            line_size = line.length();
			asm_output << "\t;[" << line_number << "] " << line << "\n";
            asm_output << "\tSRCFILE \"" << (active_include ? path : input_file) << "\"," << line_number << "\n";
            get_lex();
            if (lex == C_LABEL) {
                if (labels.find(name) != labels.end()) {
//                    string temp = "already defined '" + name + "' label";
//                    emit_error(temp);
                } else {
                    labels[name] = next_label;
                }
                asm_output << "\t; " << name << "\n";
                asm_output << LABEL_PREFIX << labels[name] << ":";
                next_label++;
                label_exists = 1;
                get_lex();
                output->trash_registers();
            } else {
                label_exists = 0;
            }
            if (lex == C_NAME) {
                if (name == "PROCEDURE") {
                    if (inside_proc) {
                        if (warnings)
                            std::cerr << "Warning: starting PROCEDURE without ENDing previous PROCEDURE in line " << line_number << "\n";
                        err_code = 1;
                    }
                    // as1600 requires that label and PROC are on same line
                    get_lex();
                    asm_output << "\tPROC\n\tBEGIN\n";
                    inside_proc = 1;
                    last_is_return = 0;
                    output->trash_registers();
                } else if (name == "END") {
                    if (!inside_proc) {
                        if (warnings)
                            std::cerr << "Warning: END without PROCEDURE in line " << line_number << "\n";
                        err_code = 1;
                    }
                    get_lex();
                    if (!last_is_return)
                        asm_output << "\tRETURN\n";
                    asm_output << "\tENDP\n";
                    inside_proc = 0;
                    last_is_return = 0;
                    output->trash_registers();
                } else if (name == "INCLUDE") {
                    int quotes;
                    
                    if (next_include == 50) {  // No more than 50 INCLUDE
                        std::cerr << "Error: more than 50 INCLUDE used at line " << line_number << "\n";
                        err_code = 1;
                    } else if (active_include) {  // No nested INCLUDE
                        std::cerr << "Error: trying to use INCLUDE inside INCLUDE in line " << line_number << "\n";
                        err_code = 1;
                    } else {
                        while (line_pos < line_size && isspace(line[line_pos]))
                            line_pos++;
                        
                        // Separate filename, admit use of quotes
                        if (line_pos < line_size && line[line_pos] == '"') {
                            quotes = 1;
                            line_pos++;
                        } else {
                            quotes = 0;
                        }
                        p = &path[0];
                        while (p < &path[4095] && line_pos < line_size) {
                            if (quotes && line[line_pos] == '"')
                                break;
                            *p++ = line[line_pos++];
                        }
                        if (quotes) {
                            if (line_pos >= line_size || line[line_pos] != '"')
                                std::cerr << "Error: missing quotes in INCLUDE in line " << line_number << "\n";
                            else
                                line_pos++;
                        } else {
                            while (p > &path[0] && isspace(*(p - 1)))
                                p--;
                        }
                        *p = '\0';
                        
                        // Try to open in current directory and then try library path.
                        include[next_include].open(path);
                        if (!include[next_include].is_open() && strlen(library_path) + strlen(path) + 2 < 4095) {
                            char path2[4096];
                            
                            strcpy(path2, path);
                            strcpy(path, library_path);
#ifdef _WIN32
                            if (strlen(path) > 0 && path[strlen(path) - 1] != '\\')
                                strcat(path, "\\");
#else
                            if (strlen(path) > 0 && path[strlen(path) - 1] != '/')
                                strcat(path, "/");
#endif
                            strcat(path, path2);
                            // This needed because of bug in Visual C++ 2008 Express Edition
                            if (++next_include == 50) {
                                std::cerr << "Error: too many INCLUDE used at line " << line_number << "\n";
                                err_code = 1;
                            } else {
                                include[next_include].open(path);
                            }
                        }
                        if (!include[next_include].is_open()) {
                            std::cerr << "Error: INCLUDE not successful in line " << line_number << "\n";
                            err_code = 2;
                            next_include++;
                        } else {
                            saved_line_number = line_number;
                            line_number = 0;
                            active_include = 1;
                            asm_output << "\t;FILE " << path << "\n";
                        }
                    }
                    lex = C_END;
                } else {
                    if (label_exists == 1)
                        asm_output << "\t";
                    compile_statement();
                    output->dump();
                }
            }
            if (lex != C_END) {
                if (warnings)
                    std::cerr << "Warning: Invalid extra characters in line " << line_number << "\n";
                err_code = 1;
            }
            
        }
        asm_output << "\t;ENDFILE\n";
        asm_output << "\tSRCFILE \"\",0\n";
        if (scroll_used)
            asm_output << "intybasic_scroll:\tequ 1\t; Forces to include scroll library\n";
        if (keypad_used)
            asm_output << "intybasic_keypad:\tequ 1\t; Forces to include keypad library\n";
        if (music_used)
            asm_output << "intybasic_music:\tequ 1\t; Forces to include music library\n";
        if (stack_used)
            asm_output << "intybasic_stack:\tequ 1\t; Forces to include stack overflow checking\n";
        if (numbers_used)
            asm_output << "intybasic_numbers:\tequ 1\t; Forces to include numbers library\n";
        if (voice_used)
            asm_output << "intybasic_voice:\tequ 1\t; Forces to include voice library\n";
        if (ecs_used)
            asm_output << "intybasic_ecs:\tequ 1\t; Forces to include ECS startup\n";
        if (fastmult_used)
            asm_output << "intybasic_fastmult:\tequ 1\t; Forces to include fast multiplication\n";
        if (fastdiv_used)
            asm_output << "intybasic_fastdiv:\tequ 1\t; Forces to include fast division/remainder\n";
        strcpy(path, library_path);
#ifdef _WIN32
        if (strlen(path) > 0 && path[strlen(path) - 1] != '\\')
            strcat(path, "\\");
#else
        if (strlen(path) > 0 && path[strlen(path) - 1] != '/')
            strcat(path, "/");
#endif
        strcat(path, "intybasic_epilogue.asm");
		// For some reason using 'included' in Visual C++ 2008 causes following to not work ???
        included2.open(path);
        if (included2.is_open()) {
            while (getline(included2, line)) {
                if (line.find(";IntyBASIC MARK DON'T CHANGE") != string::npos) {  // Location to replace title
                    if (frame_drive >= 0)
                        asm_output << "\tCALL " << LABEL_PREFIX << frame_drive << "\n";
                } else {
                    asm_output << line << "\n";
                }
            }
            included2.close();
		} else {
			std::cerr << "Error: unable to include: " << path << "\n";
            err_code = 2;
		}
        
        // Warns of read but non-assigned variables
        for (access = read_write.begin(); access != read_write.end(); access++) {
            if (access->second == 1) {
                if (warnings)
                    std::cerr << "Warning: variable '" << access->first << "' read but never assigned\n";
            } else if (access->second == 2) {
                if (warnings)
                    std::cerr << "Warning: variable '" << access->first << "' assigned but never read\n";
            }
        }
        
        // Dumps 8-bits variables
        used_space = 0;
        for (access = variables.begin(); access != variables.end(); access++) {
			if (access->first[0] != '#') {
                int size;
                
                size = 1;
                asm_output << VAR_PREFIX << access->second << ":\tRMB "
                    << size << "\t; " << access->first << "\n";
                used_space += size;
            }
        }
        for (access = arrays.begin(); access != arrays.end(); access++) {
			if (access->first[0] != '#' && access->second != 0) {
                int size;
                int label;
                
                size = access->second & 0xffff;
                label = access->second >> 16;
                asm_output << LABEL_PREFIX << label << ":\tRMB "
                    << size << "\t; " << access->first << "\n";
                used_space += size;
            }
        }
        available_vars = 240 - 12;
        if (voice_used)
            available_vars -= 3;
        if (scroll_used)
            available_vars -= 3;
        if (keypad_used)
            available_vars -= 6;
        if (music_used)
            available_vars -= 26;
        if (used_space > available_vars) {
            std::cerr << "Error: Use of 8-bits variables exceeds available space (" << used_space << " vs " << available_vars << ")\n";
            err_code = 1;
        } else {
            std::cerr << used_space << " used 8-bit variables of " << available_vars << " available\n";
        }
        asm_output << "_SCRATCH:\tEQU $\n";
        
        // Arranges stack
        if (jlp_used || cc3_used)
            asm_output << "\nSYSTEM:\tORG $8040, $8040, \"-RWBN\"\n";
        else
            asm_output << "\nSYSTEM:\tORG $2F0, $2F0, \"-RWBN\"\n";
        asm_output << "STACK:\tRMB 24\n";

        // Dumps 16-bits variables
        used_space = 0;
        for (access = variables.begin(); access != variables.end(); access++) {
			if (access->first[0] == '#') {
                int size;
                
                size = 1;
                asm_output << VAR_PREFIX << access->second << ":\tRMB "
                    << size << "\t; " << access->first << "\n";
                used_space += size;
            }
        }
        for (access = arrays.begin(); access != arrays.end(); access++) {
			if (access->first[0] == '#' && access->second != 0) {
                int size;
                int label;
                
                label = access->second >> 16;
                if (access->first == "#MOBSHADOW") {
                    asm_output << LABEL_PREFIX << label << ":\tEQU _mobs\n";
                } else {
                    size = access->second & 0xffff;
                    asm_output << LABEL_PREFIX << label << ":\tRMB "
                        << size << "\t; " << access->first << "\n";
                    used_space += size;
                }
            }
        }
        if (jlp_used || cc3_used) {
            available_vars = 0x9f80 - 0x8040 - 24;
        } else {
            if (voice_used)
                available_vars = 0x319 - 0x2f0 - 24;
            else if (scroll_used)
                available_vars = 0x323 - 0x2f0 - 24;
            else
                available_vars = 0x337 - 0x2f0 - 24;
        }
        if (used_space > available_vars) {
            std::cerr << "Error: Use of 16-bits variables exceeds available space (" << used_space << " vs " << available_vars << ")\n";
            err_code = 1;
        } else {
            std::cerr << used_space << " used 16-bit variables of " << available_vars << " available\n";
        }
        
		asm_output << "_SYSTEM:\tEQU $\n";

        // Dumps functions reference
        for (access = functions.begin(); access != functions.end(); access++) {
            if (access->second != 0)
                asm_output << FUNC_PREFIX << access->second << ":\tEQU " << access->first << "\n";
        }

    
        asm_output.close();
        input.close();
        std::cerr << "Compilation finished\n\n";
        return err_code;
    }
};

//
// Main program
//
int main(int argc, const char * argv[])
{
    compiler basic;
    int base;
    int flags;
    time_t actual;
    struct tm *date;
    char *p1;
    const char *p2;
    
    std::cerr << "\nIntyBASIC compiler " << VERSION << "\n";
    std::cerr << "(c) 2014-2015 Oscar Toledo G. http://nanochess.org/\n\n";
    
    // Get year and default title for program
    // And yep, use old-style C functions :)
    actual = time(0);
    date = localtime(&actual);
    program_year = date->tm_year;
    strcpy(program_title, "IntyBASIC program");
    
    // Process command line arguments
    base = 1;
    flags = 0;
    while (1) {
        if (argc > base && strcmp(argv[base], "--jlp") == 0) {
            flags |= 1;
            base++;
        } else if (argc > base && strcmp(argv[base], "--cc3") == 0) {
            flags |= 2;
            base++;
        } else if (argc > base && strcmp(argv[base], "-w") == 0) {
            flags |= 4;
            base++;
        } else if (argc > base + 1 && strcmp(argv[base], "--title") == 0) {
            base++;
            p1 = program_title;
            p2 = argv[base];
            while (p1 < program_title + (sizeof(program_title) - 3) && *p2) {
                if (*p2 < 0x20) {
                    p2++;
                    continue;
                }
                if (*p2 == '\'')
                    *p1++ = '\\';
                *p1++ = *p2++;
            }
            *p1 = '\0';
            base++;
        } else {
            break;
        }
    }
    
    // Show usage info if user doesn't provide enough arguments
    if (argc != base + 2 && argc != base + 3) {
        std::cerr << "Usage:\n";
        std::cerr << "\n";
        std::cerr << "    intybasic [--jlp] [--cc3] [--title \"title\"] infile.bas outfile.asm [library_path]\n\n";
        std::cerr << "    --jlp       Enables use of 8K-words extra memory feature of JLP\n";
        std::cerr << "                and also usage of hardware acceleration for\n";
        std::cerr << "                multiplication and division.\n\n";
        std::cerr << "    --cc3       Enables use of 8K-words extra memory feature of\n";
        std::cerr << "                Cuttle Cart 3.\n\n";
        std::cerr << "    --title \"a\" Selects title of the compiled program.\n";
        std::cerr << "                By default this is \"IntyBASIC program\".\n";
        std::cerr << "                Only appears in emulators/multicarts.\n\n";
        std::cerr << "    -w          Disables warnings\n\n";
        std::cerr << "    The library path is where the intybasic_prologue.asm and\n";
        std::cerr << "    intybasic_epilogue.asm files are searched for inclusion.\n";
        std::cerr << "\n";
        std::cerr << "Many thanks to atari2600land, awhite2600, carlsson, catsfolly, ckblackm,\n";
        std::cerr << "CrazyBoss, Cybearg, DZ-Jay, First Spear, freewheel, GroovyBee, intvnut,\n";
        std::cerr << "Jess Ragan, Kiwi, RevEng, SpiceWare and Tarzilla.\n";
        std::cerr << "\n";
        return 0;
    }
    return basic.start(argv[base], argv[base + 1], (argc > base + 2) ? argv[base + 2] : "", flags);
}
