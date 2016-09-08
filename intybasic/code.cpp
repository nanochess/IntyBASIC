//
//  code.cpp
//  This module takes generated assembler code, keeps register state for...
//  ...subexpression elimination and does peephole optimization.
//
//  Created by Oscar Toledo on 07/01/16.
//  Copyright (c) 2016 Oscar Toledo. All rights reserved.
//

#include <string>
#include <list>

using namespace std;

#include "global.h"
#include "microcode.h"
#include "code.h"

// Startup of class code
code::code(void) {
    trash_registers();
}

// Subexpressions currently consists of addresses of the form: array + const + a
// Checks for subexpression available (currently only in R3)
bool code::subexpression_available(int base, int offset, int index) {
    int diff;
    
    if (!subexpression_valid)
        return false;
    if (subexpression_index != index)  // Return if not same index
        return false;
    diff = offset - subexpression_offset;
    subexpression_offset = offset;
    if (subexpression_base == base) {  // Same base array, adjust offset
        if (diff == 1)
            everything.push_back(new microcode(M_R, N_INCR, 3, 0, "", 0, 0));
        else if (diff == -1)
            everything.push_back(new microcode(M_R, N_DECR, 3, 0, "", 0, 0));
        else if (diff > 0)
            everything.push_back(new microcode(M_NR, N_ADDI, 3, 0, "", diff, 0));
        else if (diff < 0)
            everything.push_back(new microcode(M_NR, N_SUBI, 3, 0, "", -diff, 0));
        if (diff != 0)
            cycles = 0;
        return true;
    }
    everything.push_back(new microcode(M_NNR, N_ADDI, 3, subexpression_base, LABEL_PREFIX, base, diff));
    cycles = 0;
    subexpression_base = base;
    return true;
}

// Annotate for subexpression available (currently only in R3)
void code::annotate_subexpression(int base, int offset, int index) {
    if (subexpression_valid)
        return;
    subexpression_valid = true;
    subexpression_base = base;
    subexpression_offset = offset;
    subexpression_index = index;
}

// Trash registers
void code::trash_registers(void) {
    int c;
    
    for (c = 0; c < 8; c++) {
        register_content[c].valid = 0;
        register_memory[c].valid = 0;
    }
    subexpression_valid = false;
}

// Check if enough cycles for non-interruptable sequence of instructions
void code::check_for_cycles(int how_many, int limit) {
    if (cycles + how_many >= limit) {
        everything.push_back(new microcode(M_SINGLE, N_NOP, 0, 0, "", 0, 0));
        cycles = 0;
    }
    cycles += how_many;
}

// Push register an create subcontext
void code::push(int r) {
    // Copies 'everything' to 'subeverything' and leaves empty 'everything'
    while (everything.size() > 0) {
        subeverything.push_back(everything.front());
        everything.pop_front();
    }
    push_register = r;
    register_content[4].valid = 2;
}

// Pop register and restore subcontext trying to optimize out PUSH/PULR, always R4
void code::pop(void) {
    if (register_content[4].valid == 2) {
        if (push_register != 4) {
            // Idea for optimization: going back in code and changing register output, now that
            // would be something hard!
            subeverything.push_back(new microcode(M_RR, N_MOVR, push_register, 4, "", 0, 0));
        }
    } else {
        subeverything.push_back(new microcode(M_R, N_PSHR, push_register, 0, "", 0, 0));
    }
    // Paste 'subeverything' just before 'everything'
    while (subeverything.size() > 0) {
        everything.push_front(subeverything.back());
        subeverything.pop_back();
    }
    // Starting from here 'everything' has been restored and the new code has been added
    if (register_content[4].valid != 2) {
        everything.push_back(new microcode(M_R, N_PULR, 4, 0, "", 0, 0));
    }
    register_content[4].valid = 0;
}

// Emits an instruction with no operands
void code::emit(enum opcode type) {
    everything.push_back(new microcode(M_SINGLE, type, 0, 0, "", 0, 0));
    cycles = 0;
}

// Emits instruction with single register operand
void code::emit_r(enum opcode type, int r1) {
    
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
        if (type == N_PULR && c == N_PSHR && previous->get_r1() == r1) {
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
    if (type == N_CLRR && register_content[r1].valid == 1 && register_content[r1].prefix == "" && register_content[r1].value == 0 && register_content[r1].offset == 0) {
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
void code::emit_rr(enum opcode type, int r1, int r2) {
    
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
    if (type == N_MVIA && r1 == 4)
        register_content[r1].valid = 0;
    if (type == N_MVOA && r2 == 4)
        register_content[r2].valid = 0;
    if (type == N_MVIA && r2 == 7)  // Jump table
        trash_registers();
}

// Emits instruction with constant/address in left side
void code::emit_nr(enum opcode type, string prefix, int value, int r) {
    emit_nor(type, prefix, value, 0, r);
}

// Emits instruction with constant/address plus offset in left side
void code::emit_nor(enum opcode type, string prefix, int value, int offset, int r) {
    int c;
    int d;
    int diff;
    
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
    // Not so common but possible because of DEF FN
    if (type == N_ANDI && prefix == "" && everything.size() > 0) {
        class microcode *previous;
        
        previous = everything.back();
        if (previous->get_type() == N_ANDI && previous->get_r1() == r && previous->get_prefix() == "") {
            value &= previous->get_value();
            everything.pop_back();
            delete previous;
        }
    }
    if (type == N_XORI && prefix == "" && everything.size() > 0) {
        class microcode *previous;
        
        previous = everything.back();
        if (previous->get_type() == N_XORI && previous->get_r1() == r && previous->get_prefix() == "") {
            value ^= previous->get_value();
            everything.pop_back();
            delete previous;
        }
    }
    
    // Common pattern optimization: constant in register
    for (c = 0; c < 4; c++) {
        d = (r + c) % 4;
        if (type == N_MVII && register_content[d].valid == 1 && register_content[d].prefix == prefix && register_content[d].value == value) {
            diff = offset - register_content[d].offset;
            if (diff == 0) {
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
                    return;
                }
            } else if (diff == 1 && d == r) {
                everything.push_back(new microcode(M_R, N_INCR, r, 0, "", 0, 0));
                register_content[r].offset = offset;
                if (r == 3)
                    subexpression_valid = false;
                return;
            } else if (diff == -1 && d == r) {
                everything.push_back(new microcode(M_R, N_INCR, r, 0, "", 0, 0));
                register_content[r].offset = offset;
                if (r == 3)
                    subexpression_valid = false;
                return;
            }
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
    } else if (type == N_ADDI) {
        if (register_content[r].valid && register_content[r].prefix != "")
            register_content[r].offset = (register_content[r].offset + value) & 0xffff;
        register_memory[r].valid = 0;
        if (r == 3)
            subexpression_valid = false;
    } else if (type == N_SUBI) {
        if (register_content[r].valid && register_content[r].prefix != "")
            register_content[r].offset = (register_content[r].offset - value) & 0xffff;
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
void code::emit_lr(enum opcode type, string prefix, int value, int r) {
    emit_lor(type, prefix, value, 0, r);
}

// Emits instruction with label plus offset in left side
void code::emit_lor(enum opcode type, string prefix, int value, int offset, int r) {
    int c;
    int d;
    class microcode *previous;
    
    // Common optimization case: register just saved to memory and still available
    for (c = 0; c < 4; c++) {
        d = (r + c) % 4;
        if (register_memory[d].valid == 1           // Register is copy of memory
         && register_memory[d].prefix == prefix     // Same prefix
         && register_memory[d].value == value       // Same value
         && register_memory[d].offset == offset) {  // Same offset
            if (type == N_ADD) {                    // Change ADD from memory to ADD from register
                type = N_ADDR;
                everything.push_back(new microcode(M_RR, type, d, r, "", 0, 0));
                register_memory[r].valid = 0;
                if (r == 3)
                    subexpression_valid = false;
            } else if (type == N_AND) {             // Change AND from memory to AND from register
                type = N_ANDR;
                everything.push_back(new microcode(M_RR, type, d, r, "", 0, 0));
                register_memory[r].valid = 0;
                if (r == 3)
                    subexpression_valid = false;
            } else if (type == N_CMP) {             // Change CMP from memory to CMP from register
                type = N_CMPR;
                everything.push_back(new microcode(M_RR, type, d, r, "", 0, 0));
                // Note register is still valid
            } else if (type == N_SUB) {             // Change SUB from memory to SUB from register
                type = N_SUBR;
                everything.push_back(new microcode(M_RR, type, d, r, "", 0, 0));
                register_memory[r].valid = 0;
                if (r == 3)
                    subexpression_valid = false;
            } else if (type == N_XOR) {             // Change XOR from memory to XOR from register
                type = N_XORR;
                everything.push_back(new microcode(M_RR, type, d, r, "", 0, 0));
                register_memory[r].valid = 0;
                if (r == 3)
                    subexpression_valid = false;
            } else /*if (type == N_MVI)*/ {         // Change MVI to MOVR
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
    // Replace NEGR/ADD with SUB/NEGR for future optimization
    if (type == N_ADD && previous && previous->get_type() == N_NEGR && previous->get_r1() == r) {
        everything.pop_back();
        delete previous;
        type = N_SUB;
        everything.push_back(new microcode(M_LR, type, r, 0, prefix, value, offset));
        everything.push_back(new microcode(M_R, N_NEGR, r, 0, "", 0, 0));
    } else {
        everything.push_back(new microcode(M_LR, type, r, 0, prefix, value, offset));
    }
    if (type == N_CMP) {  // Comparisons doesn't affect registers state
        // register still valid
    } else if (type == N_ADD || type == N_AND || type == N_SUB || type == N_XOR) {
        // Target register ceases to be valid (content or memory alias)
        register_content[r].valid = 0;
        register_memory[r].valid = 0;
        if (r == 3)
            subexpression_valid = false;
    } else /*if (type == N_MVI)*/ {
        // Target register ceases to be valid as value
        register_content[r].valid = 0;
        if (prefix == "" && (value == 0x9f8e || value == 0x9f8f)) {  // JLP doesn't qualify for cache
            register_memory[r].valid = 0;
        } else {
            // Target register is now a copy of memory, so it's optimizable
            register_memory[r].valid = 1;
            register_memory[r].prefix = prefix;
            register_memory[r].value = value;
            register_memory[r].offset = offset;
        }
        if (r == 3)
            subexpression_valid = false;
    }
    cycles = 0;
}

// Emits instruction with label in right side
void code::emit_rl(enum opcode type, int r, string prefix, int value) {
    emit_rlo(type, r, prefix, value, 0);
}

// Emits instruction with label in right side (to 8 bits memory)
void code::emit_rlo8(enum opcode type, int r, string prefix, int value, int offset) {
    if (type == N_MVO)
        check_for_cycles(11, 23);
    everything.push_back(new microcode(M_RL, type, r, 0, prefix, value, offset));
    register_memory[r].valid = 0;  // Not valid because: cuts upper 8 bits
    subexpression_valid = false;  // Possibly wrote index variable
}

// Emits instruction with label plus offset in right side
void code::emit_rlo(enum opcode type, int r, string prefix, int value, int offset) {
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
void code::emit_a(enum opcode type, string prefix, int value) {
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
void code::emit_s(enum opcode type, int r, int s) {
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
void code::emit_m(enum opcode type, int r1, int r2, int v) {
    everything.push_back(new microcode(M_M, type, r1, r2, "", v, 0));
    register_content[r1].valid = 0;
    register_memory[r1].valid = 0;
    if (r1 == 3)
        subexpression_valid = false;
    register_content[r2].valid = 0;  // Not always
    cycles = 0;
}

// Emits label
void code::emit_l(string prefix, int value) {
    everything.push_back(new microcode(M_L, 0, 0, 0, prefix, value, 0));
    trash_registers();
    cycles = 0;
}

// Emits label guaranteed to be IF and used only one time
void code::emit_l3(string prefix, int value) {
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
void code::emit_d(enum opcode type, int d) {
    everything.push_back(new microcode(M_D, type, d, 0, "", 0, 0));
    cycles = 0;
}

// Emits data (2 words)
void code::emit_d2(enum opcode type, int d1, int d2) {
    everything.push_back(new microcode(M_D2, type, d1, d2, "", 0, 0));
    cycles = 0;
}

// Emits data (label)
void code::emit_dl(enum opcode type, string prefix, int value) {
    everything.push_back(new microcode(M_DL, type, 0, 0, prefix, value, 0));
    cycles = 0;
}

// Emits data (label)
void code::emit_dlo(enum opcode type, string prefix, int value, int offset) {
    everything.push_back(new microcode(M_DL, type, 0, 0, prefix, value, offset));
    cycles = 0;
}

// Emits literal assembler (user provided)
void code::emit_literal(string text) {
    everything.push_back(new microcode(M_LITERAL, 0, 0, 0, text, 0, 0));
    trash_registers();
    cycles = 0;
}

// Dump microcode as assembler
void code::dump(void) {
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
