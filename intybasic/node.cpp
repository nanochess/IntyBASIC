//
//  node.cpp
//  This module creates trees for IntyBASIC expressions, label nodes, and generates code.
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
#include "node.h"

//
// Builds an expression node
//
node::node(enum lexical_component type, int value, class node *left, class node *right) {
    this->type = type;
    this->value = value;
    this->regs = 0;
    this->left = left;
    this->right = right;
    
    // Optimizes trees of addition/substraction operators
    if (type == C_MINUS && right->type == C_NUM && left->type == C_PLUS && left->right->type == C_NUM) {
        right->value = left->right->value - right->value;
        this->left = left->left;
        left->left = NULL;
        delete left;
        left = this->left;
        this->type = C_PLUS;
    }
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
    // Optimize common array access case array(c+1)
    if (type == C_PLUS && left->type == C_NAME_RO && right->type == C_PLUS) {
        class node *temp;
        
        // Pass constant to left side (in order to be added to address in one instruction)
        if (right->right->type == C_NUM) {      // array(c+1)
            temp = right->left;
            right->left = left;
            this->left = right;
            this->right = temp;
        } else if (right->left->type == C_NUM) {    // array(1+c)
            temp = right->right;
            right->right = left;
            this->left = right;
            this->right = temp;
        }
    }
    // Optimize common array access case array(c-1)
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
    // Optimize simple signed 8-bits expression
    if (type == C_ASSIGN && value == 0 && left->type == C_EXTEND) {
        this->left = left->left;
    }
    if (type == C_ASSIGN && value == 0 && (left->type == C_PLUS || left->type == C_MINUS || left->type == C_MUL || left->type == C_AND || left->type == C_OR || left->type == C_XOR)) {
        if (left->left->type == C_EXTEND)
            left->left = left->left->left;
        if (left->right->type == C_EXTEND)
            left->right = left->right->left;
    }
    // Optimizes constant expressions
    if (type == C_PLUS) {
        if (left->type == C_NUM && right->type == C_NUM) {  // const + const --> const
            this->type = C_NUM;
            this->value = left->value + right->value;
            delete this->left;
            this->left = NULL;
            delete this->right;
            this->right = NULL;
        } else if (left->type == C_NUM) {  // const + xyz --> xyz + const
            this->left = right;
            this->right = left;
        }
    }
    if (type == C_MINUS) {
        if (left->type == C_NUM && right->type == C_NUM) {  // const - const --> const
            this->type = C_NUM;
            this->value = left->value - right->value;
            delete this->left;
            this->left = NULL;
            delete this->right;
            this->right = NULL;
        }
    }
    if (type == C_MUL) {
        if (left->type == C_NUM && right->type == C_NUM) {  // const * const --> const
            this->type = C_NUM;
            this->value = left->value * right->value;
            delete this->left;
            this->left = NULL;
            delete this->right;
            this->right = NULL;
        } else if (left->type == C_NUM) {  // const * xyz --> xyz * const
            this->left = right;
            this->right = left;
        }
    }
    if (type == C_DIV && left->type == C_NUM && right->type == C_NUM && right->value != 0) {
        this->type = C_NUM;
        this->value = left->value / right->value;
        delete this->left;
        this->left = NULL;
        delete this->right;
        this->right = NULL;
    }
    if (type == C_MOD && left->type == C_NUM && right->type == C_NUM && right->value != 0) {
        this->type = C_NUM;
        this->value = left->value % right->value;
        delete this->left;
        this->left = NULL;
        delete this->right;
        this->right = NULL;
    }
    if (type == C_AND) {
        if (left->type == C_NUM && right->type == C_NUM) {
            this->type = C_NUM;
            this->value = left->value & right->value;
            delete this->left;
            this->left = NULL;
            delete this->right;
            this->right = NULL;
        } else if (left->type == C_NUM) {
            this->left = right;
            this->right = left;
        }
    }
    if (type == C_OR) {
        if (left->type == C_NUM && right->type == C_NUM) {
            this->type = C_NUM;
            this->value = left->value | right->value;
            delete this->left;
            this->left = NULL;
            delete this->right;
            this->right = NULL;
        } else if (left->type == C_NUM) {
            this->left = right;
            this->right = left;
        }
    }
    if (type == C_XOR) {
        if (left->type == C_NUM && right->type == C_NUM) {
            this->type = C_NUM;
            this->value = left->value ^ right->value;
            delete this->left;
            this->left = NULL;
            delete this->right;
            this->right = NULL;
        } else if (left->type == C_NUM) {
            this->left = right;
            this->right = left;
        }
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
        this->value = left->value < 0 ? -left->value : left->value;
        delete this->left;
        this->left = NULL;
    }
    if (type == C_SGN && left->type == C_NUM) {
        this->type = C_NUM;
        this->value = left->value < 0 ? -1 : (left->value ? 1 : 0);
        delete this->left;
        this->left = NULL;
    }
}

//
// Destructor for node
//
node::~node() {
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
enum lexical_component node::node_type(void) {
    return type;
}

//
// Get node value
//
int node::node_value(void) {
    return value;
}

//
// Get node left side
//
class node *node::node_left(void) {
    return left;
}

//
// Get node right side
//
class node *node::node_right(void) {
    return right;
}

//
// Set right node
//
void node::set_right(class node *right) {
    this->right = right;
}

//
// Check for valid array with or without offset
//
bool node::valid_array(void) {
    //
    //         C_PLUS          a(x)
    //        /      \
    //   C_NAME_RO   C_NAME
    //
    //         C_PLUS          a(x+5)
    //        /      \
    //     C_PLUS   C_NAME
    //    /      \
    // C_NAME_RO  C_NUM
    //
    //
    if (type != C_PLUS || right->type != C_NAME)
        return false;
    if (left->type == C_NAME_RO)
        return true;
    if (left->type == C_PLUS && left->left->type == C_NAME_RO && left->right->type == C_NUM)
        return true;
    return false;
}

//
// Annotate index for subexpression
//
void node::annotate_index_for_subexpression(void)
{
    
    //         C_PLUS          a(x)
    //        /      \
    //   C_NAME_RO   C_NAME
    //
    if (left->type == C_NAME_RO) {  // Without offset
        if (!output->subexpression_available(left->value, 0, right->value)) {
            this->generate(3, 0);
            output->annotate_subexpression(left->value, 0, right->value);
        }
        
    //         C_PLUS          a(x+5)
    //        /      \
    //     C_PLUS   C_NAME
    //    /      \
    // C_NAME_RO  C_NUM
    //
    //
    } else {  // With offset into array
        if (!output->subexpression_available(left->left->value, left->right->value, right->value)) {
            this->generate(3, 0);
            output->annotate_subexpression(left->left->value, left->right->value, right->value);
        }
    }
}

//
// Analyze each node and calculate number of registers used
//
void node::label(void) {
    
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
        if (type == C_MUL && left->type == C_NAME && right->type == C_NAME && !jlp_used)
            regs = 10;
        else if (type == C_DIV && left->type == C_NAME && right->type == C_NAME && !jlp_used)
            regs = 10;
        else if (type == C_MOD && left->type == C_NAME && right->type == C_NAME && !jlp_used)
            regs = 10;
        else
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
        if (left->regs > right->regs)
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
void node::generate(int reg, int decision) {
    if (reg >= 4) {
        std::cerr << "Error: Too complex expression\n";
        err_code = 1;
        return;
    }
    switch (type) {
        default:        // Undefined node, never should happen
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
        case C_NAME_RO: // Access to label or array address
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
        case C_EXTEND:  // Extend 8-bits sign (assumes value in range 0x00-0xff)
            left->generate(reg, 0);
            output->emit_nr(N_XORI, "", 0x80, reg);
            output->emit_nr(N_SUBI, "", 0x80, reg);
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
                int label = next_local++;

                output->emit_lr(N_MVI, "_cnt1_key", -1, reg);
                output->emit_nr(N_CMPI, "", 12, reg);
                output->emit_a(N_BNE, TEMP_PREFIX, label);   // two words of jump and two words of MVI
                output->emit_lr(N_MVI, "_cnt2_key", -1, reg);
                output->emit_l(TEMP_PREFIX, label);
                output->trash_partial(reg);
            }
            if (value == 15)
                output->emit_lr(N_MVI, "_screen", -1, reg);
            if (value == 16)    // FLASH.FIRST
                output->emit_lr(N_MVI, "", 0x8023, reg);
            if (value == 17)    // FLASH.LAST
                output->emit_lr(N_MVI, "", 0x8024, reg);
            if (value == 18) {  // MUSIC.PLAYING
                int label = next_local++;

                output->emit_lr(N_MVI, "_music_p", -1, reg);    // 2
                output->emit_r(N_TSTR, reg);            // 1, if pointer is zero...
                output->emit_a(N_BEQ, TEMP_PREFIX, label);  // 2, ...not playing, jump with reg=zero
                output->emit_rr(N_MOVR, reg, 4);        // 1, prepare to read
                output->emit_rr(N_MVIA, 4, reg);        // 1, read first word...
                output->emit_nr(N_SUBI, "", 254, reg);  // 2, ...if it isn't 254...
                output->emit_a(N_BNE, TEMP_PREFIX, label);  // 2, ...jump with reg=non-zero (playing)
                output->emit_rr(N_MVIA, 4, reg);        // 1, MUSIC JUMP label, will be 0 if MUSIC STOP
                output->emit_l(TEMP_PREFIX, label);
                output->trash_partial(reg);
                music_used = true;
            }
            if (value == 19) {  // VOICE.AVAILABLE
                output->emit_lr(N_MVI, "", 0x0080, reg);
                output->emit_r(N_INCR, reg);
            }
            if (value == 20) {  // VOICE.PLAYING
                int label = next_local++;
                
                output->emit_lr(N_MVI, "IV.QT", -1, reg);   // 2
                output->emit_nr(N_ANDI, "", 7, reg);
                output->emit_lr(N_SUB, "IV.QH", -1, reg);   // 2
                output->emit_a(N_BNE, TEMP_PREFIX, label);          // 2
                output->emit_lr(N_MVI, "IV.FPTR", -1, reg);   // 2
                output->emit_r(N_TSTR, reg);            // 1
                output->emit_a(N_BNE, TEMP_PREFIX, label);          // 2
                output->emit_lr(N_MVI, "", 0x0081, reg);    // 2
                output->emit_r(N_COMR, reg);            // 1
                output->emit_lr(N_AND, "", 0x0080, reg);    // 2
                output->emit_r(N_COMR, reg);            // 1
                output->emit_nr(N_ANDI, "", 0x8000, reg);    // 2
                output->emit_l(TEMP_PREFIX, label);
                output->trash_partial(reg);
            }
            break;
        case C_PEEK:    // PEEK()
            if (left->type == C_NUM) {  // Peek directly from memory location
                output->emit_lr(N_MVI, "", left->value, reg);
            } else if (left->type == C_PLUS && left->left->type == C_NAME_RO && left->right->type == C_NUM) {  // Constant index into array
                output->emit_lor(N_MVI, LABEL_PREFIX, left->left->value, left->right->value, reg);
            } else if (left->valid_array()) {  // Simple index into array
                left->annotate_index_for_subexpression();
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
        {
            int label = next_local++;

            if (left->type == C_EXTEND) {
                left->left->generate(reg, 0);
                output->emit_r(N_RSWD, reg);
            } else {
                left->generate(reg, 0);
                output->emit_r(N_TSTR, reg);
            }
            output->emit_a(N_BPL, TEMP_PREFIX, label);  // two words of jump and one word of NEGR
            output->emit_r(N_NEGR, reg);
            output->emit_l(TEMP_PREFIX, label);
            output->trash_partial(reg);
            break;
        }
        case C_SGN:    // SGN()
        {
            int label = next_local++;

            if (left->type == C_EXTEND) {
                left->left->generate(reg, 0);
                output->emit_s(N_SWAP, reg, 2); // Trick, duplicate byte in register
            } else {
                left->generate(reg, 0);
                output->emit_r(N_TSTR, reg);
            }
            output->emit_a(N_BEQ, TEMP_PREFIX, label);  // 2 two words of jump and two words of MVI
            output->emit_nr(N_MVII, "", 1, reg);    // 2
            output->emit_a(N_BPL, TEMP_PREFIX, label);   // 2 two words of jump and one word of NEGR
            output->emit_r(N_NEGR, reg);    // 1
            output->emit_l(TEMP_PREFIX, label);
            output->trash_partial(reg);
            break;
        }
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
            } else if (type == C_PLUS && left->type == C_NAME_RO && right->type == C_NUM) {
                // Get address of array with constant index
                output->emit_nor(N_MVII, LABEL_PREFIX, left->value, right->value, reg);
            } else if (type == C_MINUS && left->type == C_PLUS && left->left->type == C_NAME_RO && right->type == C_PLUS && right->left->type == C_NAME_RO) {
                // Optimize VARPTR array1(x) - VARPTR array2(y)
                output->emit_nnr(N_MVII, right->left->value, left->left->value, left->right->value - right->right->value);
            } else if (type == C_ASSIGN && right->valid_array()) {
                // Optimization for assignation to array with simple index
                left->generate(0, 0);
                if (value == 0) // 8-bits
                    output->emit_256(0);
                right->annotate_index_for_subexpression();
                output->emit_rr(N_MVOA, 0, 3);
            } else if (right->type == C_NUM && type != C_ASSIGN) {
                // Optimize right side when it's constant
                int val = right->value & 0xffff;
                
                if (left->type == C_EXTEND && (type == C_EQUAL || type == C_NOTEQUAL)) {
                    left->left->generate(reg, 0);
                    if (val >= 0x0080 && val <= 0xff7f)
                        std::cerr << "Warning: equality comparison of 8-bit signed variable with value " << val << " out of range";
                    val &= 0xff;
                } else {
                    left->generate(reg, 0);
                }
                if (type == C_PLUS) {
                    if (val == 1)
                        output->emit_r(N_INCR, reg);
                    else if (val == 0xffff)
                        output->emit_r(N_DECR, reg);
                    else if (val != 0)
                        output->emit_nr(N_ADDI, "", val, reg);
                } else if (type == C_MINUS) {
                    if (val == 1)
                        output->emit_r(N_DECR, reg);
                    else if (val == 0xffff)
                        output->emit_r(N_INCR, reg);
                    else if (val != 0)
                        output->emit_nr(N_SUBI, "", val, reg);
                } else if (type == C_PLUSF) {
                    if (val != 0) {
                        output->emit_nr(N_ADDI, "", val, reg);
                        output->emit_r(N_ADCR, reg);
                    }
                } else if (type == C_MINUSF) {
                    if (val != 0) {
                        output->emit_nr(N_SUBI, "", val, reg);
                        output->emit_r(N_ADCR, reg);
                        output->emit_r(N_DECR, reg);
                    }
                } else if (type == C_AND) {
                    if (val != 0xffff)
                        output->emit_nr(N_ANDI, "", val, reg);
                } else if (type == C_XOR) {
                    if (val != 0x0000)
                        output->emit_nr(N_XORI, "", val, reg);
                } else if (type == C_OR) {
                    if (val != 0x0000) {
                        output->emit_nr(N_ANDI, "", ~val, reg);
                        output->emit_nr(N_XORI, "", val, reg);
                    }
                } else if (type == C_EQUAL) {
                    if (val)
                        output->emit_nr(N_CMPI, "", val, reg);
                    else if (left->type != C_AND && left->type != C_OR && left->type != C_XOR
                             && left->type != C_PLUS && left->type != C_MINUS)
                        output->emit_r(N_TSTR, reg);
                    if (decision) {
                        output->emit_a(N_BNE, TEMP_PREFIX, decision);
                        optimized = true;
                    } else {
                        int label = next_local++;
                        
                        output->emit_nr(N_MVII, "", -1, reg);
                        output->emit_a(N_BEQ, TEMP_PREFIX, label);   // two words of jump and one word of INCR
                        output->emit_r(N_INCR, reg);
                        output->emit_l(TEMP_PREFIX, label);
                        output->trash_partial(reg);
                    }
                } else if (type == C_NOTEQUAL) {
                    if (val)
                        output->emit_nr(N_CMPI, "", val, reg);
                    else if (left->type != C_AND && left->type != C_OR && left->type != C_XOR
                             && left->type != C_PLUS && left->type != C_MINUS)
                        output->emit_r(N_TSTR, reg);
                    if (decision) {
                        output->emit_a(N_BEQ, TEMP_PREFIX, decision);
                        optimized = true;
                    } else {
                        int label = next_local++;

                        output->emit_nr(N_MVII, "", -1, reg);
                        output->emit_a(N_BNE, TEMP_PREFIX, label);   // two words of jump and one word of INCR
                        output->emit_r(N_INCR, reg);
                        output->emit_l(TEMP_PREFIX, label);
                        output->trash_partial(reg);
                    }
                } else if (type == C_LESS) {
                    output->emit_nr(N_CMPI, "", val, reg);
                    if (decision) {
                        output->emit_a(value ? N_BC : N_BGE, TEMP_PREFIX, decision);
                        optimized = true;
                    } else {
                        int label = next_local++;

                        output->emit_nr(N_MVII, "", -1, reg);
                        output->emit_a(value ? N_BNC : N_BLT, TEMP_PREFIX, label);   // two words of jump and one word of INCR
                        output->emit_r(N_INCR, reg);
                        output->emit_l(TEMP_PREFIX, label);
                        output->trash_partial(reg);
                    }
                } else if (type == C_LESSEQUAL) {
                    output->emit_nr(N_CMPI, "", val, reg);
                    if (decision) {
                        if (value) {    // Unsigned
                            output->emit_a(N_BEQ, "", 4);
                            output->emit_a(N_BC, TEMP_PREFIX, decision);
                        } else {
                            output->emit_a(N_BGT, TEMP_PREFIX, decision);
                        }
                        optimized = true;
                    } else {
                        int label = next_local++;

                        output->emit_nr(N_MVII, "", -1, reg);
                        if (value) {    // Unsigned
                            output->emit_a(N_BEQ, TEMP_PREFIX, label);   // 2+2+1
                            output->emit_a(N_BNC, TEMP_PREFIX, label);   // two words of jump and one word of INCR
                        } else {
                            output->emit_a(N_BLE, TEMP_PREFIX, label);   // two words of jump and one word of INCR
                        }
                        output->emit_r(N_INCR, reg);
                        output->emit_l(TEMP_PREFIX, label);
                        output->trash_partial(reg);
                    }
                } else if (type == C_GREATER) {
                    output->emit_nr(N_CMPI, "", val, reg);
                    if (decision) {
                        if (value) {    // Unsigned
                            output->emit_a(N_BEQ, TEMP_PREFIX, decision);
                            output->emit_a(N_BNC, TEMP_PREFIX, decision);
                        } else {
                            output->emit_a(N_BLE, TEMP_PREFIX, decision);
                        }
                        optimized = true;
                    } else {
                        int label = next_local++;

                        output->emit_nr(N_MVII, "", -1, reg);
                        if (value) {    // Unsigned
                            output->emit_a(N_BEQ, TEMP_PREFIX, label);   // 2+2
                            output->emit_a(N_BC, TEMP_PREFIX, label);   // two words of jump and one word of INCR
                        } else {
                            output->emit_a(N_BGT, TEMP_PREFIX, label);   // two words of jump and one word of INCR
                        }
                        output->emit_r(N_INCR, reg);
                        output->emit_l(TEMP_PREFIX, label);
                        output->trash_partial(reg);
                    }
                } else if (type == C_GREATEREQUAL) {
                    output->emit_nr(N_CMPI, "", val, reg);
                    if (decision) {
                        output->emit_a(value ? N_BNC : N_BLT, TEMP_PREFIX, decision);
                        optimized = true;
                    } else {
                        int label = next_local++;

                        output->emit_nr(N_MVII, "", -1, reg);
                        output->emit_a(value ? N_BC : N_BGE, TEMP_PREFIX, label);   // two words of jump and one word of INCR
                        output->emit_r(N_INCR, reg);
                        output->emit_l(TEMP_PREFIX, label);
                        output->trash_partial(reg);
                    }
                } else if (type == C_MUL) {
                    if (val == 0) {
                        output->emit_r(N_CLRR, reg);
                    } if (val == 1) {
                        // Nothing to do
                    } if (val == 2) {
                        output->emit_s(N_SLL, reg, 1);
                    } else if (val == 4) {
                        output->emit_s(N_SLL, reg, 2);
                    } else if (val == 8) {
                        output->emit_s(N_SLL, reg, 2);
                        output->emit_rr(N_ADDR, reg, reg);
                    } else if (val == 16) {
                        output->emit_s(N_SLL, reg, 2);
                        output->emit_s(N_SLL, reg, 2);
                    } else if (val == 32) {
                        output->emit_s(N_SLL, reg, 2);
                        output->emit_s(N_SLL, reg, 2);
                        output->emit_rr(N_ADDR, reg, reg);
                    } else if (val == 64) {
                        output->emit_s(N_SLL, reg, 2);
                        output->emit_s(N_SLL, reg, 2);
                        output->emit_s(N_SLL, reg, 2);
                    } else if (val == 128) {
                        output->emit_s(N_SLL, reg, 2);
                        output->emit_s(N_SLL, reg, 2);
                        output->emit_s(N_SLL, reg, 2);
                        output->emit_rr(N_ADDR, reg, reg);
                    } else if (val == 256) {
                        output->emit_r(N_SWAP, reg);
                        output->emit_nr(N_ANDI, "", 0xff00, reg);
                    } else if (val == 512) {
                        output->emit_s(N_SLL, reg, 1);
                        output->emit_r(N_SWAP, reg);
                        output->emit_nr(N_ANDI, "", 0xfe00, reg);
                    } else if (val == 1024) {
                        output->emit_s(N_SLL, reg, 2);
                        output->emit_r(N_SWAP, reg);
                        output->emit_nr(N_ANDI, "", 0xfc00, reg);
                    } else if (val == 2048) {
                        output->emit_rr(N_ADDR, reg, reg);
                        output->emit_s(N_SLL, reg, 2);
                        output->emit_r(N_SWAP, reg);
                        output->emit_nr(N_ANDI, "", 0xf800, reg);
                    } else if (val == 4096) {
                        output->emit_s(N_SLL, reg, 2);
                        output->emit_s(N_SLL, reg, 2);
                        output->emit_r(N_SWAP, reg);
                        output->emit_nr(N_ANDI, "", 0xf000, reg);
                    } else {
                        // Speed of multiplication by constant macro for each number
                        const int speed[] = {
                             6,  0,  6, 18,  8, 20, 24, 26,
                            14, 26, 26, 34, 26, 32, 32, 28,
                            16, 28, 32, 38, 28, 34, 38, 38,
                            32, 38, 38, 44, 34, 40, 34, 34,
                            22, 34, 34, 40, 34, 40, 44, 40,
                            34, 40, 40, 46, 40, 46, 44, 40,
                            34, 40, 44, 50, 40, 46, 50, 46,
                            40, 46, 46, 52, 36, 42, 40, 36,
                            24, 36, 40, 46, 36, 42, 46, 46,
                            40, 46, 46, 52, 46, 52, 46, 42,
                            36, 42, 99, 99, 99, 99, 99, 99,
                            99, 99, 99, 99, 99, 99, 99, 99,
                            99, 99, 99, 99, 99, 99, 99, 99,
                            99, 99, 99, 99, 99, 99, 99, 99,
                            99, 99, 99, 99, 99, 99, 99, 99,
                            99, 99, 99, 99, 99, 99, 99, 99,
                        };
//                        if (val < 128 && speed[val] == 99)
//                            cerr << "count " << val << "\n";
                        if (jlp_used && (val >= 128 || speed[val] >= 40)) {
                            output->emit_nr(N_MVII, "", val, 4);  // 8 cycles
                            output->emit_rl(N_MVO, reg, "", 0x9f86);  // 11 cycles
                            output->emit_rl(N_MVO, 4, "", 0x9f87);  // 11 cycles
                            output->emit_lr(N_MVI, "", 0x9f8e, reg);  // 10 cycles
                        } else if (val <= 127) {  // DZ-Jay's macro
                            output->emit_m(N_MULT, reg, 4, val);
                        } else {
                            int label = next_local++;
                            int label2 = next_local++;
                            int label3 = next_local++;
                            int label4 = next_local++;
                            
                            output->emit_nr(N_MVII, "", val, 5);
                            output->emit_r(N_CLRR, 4);
                            output->emit(N_CLRC);
                            output->emit_s(N_RRC, reg, 1);
                            output->emit_a(N_BEQ, TEMP_PREFIX, label2);
                            output->emit_l(TEMP_PREFIX, label);
                            output->emit_a(N_BNC, TEMP_PREFIX, label3);  // Two words of jump and one of ADDR
                            output->emit_rr(N_ADDR, 5, 4);
                            output->emit_l(TEMP_PREFIX, label3);
                            output->emit_rr(N_ADDR, 5, 5);
                            output->emit_s(N_SARC, reg, 1);
                            output->emit_a(N_BNE, TEMP_PREFIX, label);
                            output->emit_l(TEMP_PREFIX, label2);
                            output->emit_a(N_BNC, TEMP_PREFIX, label4);  // Two words of jump and one of ADDR
                            output->emit_rr(N_ADDR, 5, 4);
                            output->emit_l(TEMP_PREFIX, label4);
                            output->emit_rr(N_MOVR, 4, reg);
                            output->trash_partial(reg);
                        }
                    }
                } else if (type == C_DIV) {
                    if (val == 0) {
                        std::cerr << "Error: Division by zero in expression\n";
                        err_code = 1;
                    } else if (val == 1) {
                        // Nada que hacer
                    } else if (val == 2) {
                        output->emit_s(N_SLR, reg, 1);
                    } else if (val == 4) {
                        output->emit_s(N_SLR, reg, 2);
                    } else if (val == 8) {
                        output->emit_s(N_SLR, reg, 2);
                        output->emit_s(N_SLR, reg, 1);
                    } else if (val == 16) {
                        output->emit_s(N_SLR, reg, 2);
                        output->emit_s(N_SLR, reg, 2);
                    } else if (val == 32) {
                        output->emit_s(N_SLR, reg, 2);
                        output->emit_s(N_SLR, reg, 2);
                        output->emit_s(N_SLR, reg, 1);
                    } else if (val == 64) {
                        output->emit_s(N_SLR, reg, 2);
                        output->emit_s(N_SLR, reg, 2);
                        output->emit_s(N_SLR, reg, 2);
                    } else if (val == 128) {
                        output->emit_r(N_SWAP, reg);
                        output->emit_rr(N_ADDR, reg, reg);
                        output->emit_r(N_ADCR, reg);
                        output->emit_nr(N_ANDI, "", 0x01ff, reg);
                    } else if (val == 256) {
                        output->emit_r(N_SWAP, reg);
                        output->emit_nr(N_ANDI, "", 0x00ff, reg);
                    } else if (val == 4096) {
                        output->emit_r(N_SWAP, reg);
                        output->emit_nr(N_ANDI, "", 0x00f0, reg);
                        output->emit_s(N_SLR, reg, 2);
                        output->emit_s(N_SLR, reg, 2);
                    } else {
                        if (jlp_used) {
                            output->emit_nr(N_MVII, "", val, 4);
                            output->emit_rl(N_MVO, reg, "", 0x9f8a);
                            output->emit_rl(N_MVO, 4, "", 0x9f8b);
                            output->emit_lr(N_MVI, "", 0x9f8e, reg);
                        } else {
                            int label = next_local++;
                            
                            output->emit_nr(N_MVII, "", -1, 4);
                            output->emit_l(TEMP_PREFIX, label);
                            output->emit_r(N_INCR, 4);
                            output->emit_nr(N_SUBI, "", val, reg);
                            output->emit_a(N_BC, TEMP_PREFIX, label);
                            output->emit_rr(N_MOVR, 4, reg);
                        }
                    }
                } else if (type == C_MOD) {
                    if (val == 0) {
                        std::cerr << "Error: Modulus by zero in expression\n";
                        err_code = 1;
                    } else if (val == 1) {
                        output->emit_r(N_CLRR, reg);
                    } else if (val == 2
                               || val == 4
                               || val == 8
                               || val == 16
                               || val == 32
                               || val == 64
                               || val == 128
                               || val == 256
                               || val == 512
                               || val == 1024
                               || val == 2048
                               || val == 4096
                               || val == 8192
                               || val == 16384
                               || val == 32768) {
                        output->emit_nr(N_ANDI, "", val - 1, reg);
                    } else {
                        if (jlp_used) {
                            output->emit_nr(N_MVII, "", val, 4);
                            output->emit_rl(N_MVO, reg, "", 0x9f8a);
                            output->emit_rl(N_MVO, 4, "", 0x9f8b);
                            output->emit_lr(N_MVI, "", 0x9f8f, reg);
                        } else {
                            int label = next_local++;
                            
                            output->emit_l(TEMP_PREFIX, label);
                            output->emit_nr(N_SUBI, "", val, reg);
                            output->emit_a(N_BC, TEMP_PREFIX, label);
                            output->emit_nr(N_ADDI, "", val, reg);
                        }
                    }
                }
                
                // Optimize right side when it's variable
            } else if ((right->type == C_NAME || (right->type == C_PEEK && right->left->type == C_PLUS && right->left->right->type == C_NUM && right->left->left->type == C_NAME_RO)) && type != C_ASSIGN) {
                string prefix;
                int offset;
                int variable;
                
                if (right->type == C_PEEK) {
                    prefix = LABEL_PREFIX;
                    variable = right->left->left->value;
                    offset = right->left->right->value;
                } else {
                    prefix = VAR_PREFIX;
                    variable = right->value;
                    offset = 0;
                }
                left->generate(reg, 0);
                if (type == C_PLUS) {
                    output->emit_lor(N_ADD, prefix, variable, offset, reg);
                } else if (type == C_MINUS) {
                    output->emit_lor(N_SUB, prefix, variable, offset, reg);
                } else if (type == C_PLUSF) {
                    output->emit_lor(N_ADD, prefix, variable, offset, reg);
                    output->emit_r(N_ADCR, reg);
                } else if (type == C_MINUSF) {
                    output->emit_lor(N_SUB, prefix, variable, offset, reg);
                    output->emit_r(N_ADCR, reg);
                    output->emit_r(N_DECR, reg);
                } else if (type == C_AND) {
                    output->emit_lor(N_AND, prefix, variable, offset, reg);
                } else if (type == C_XOR) {
                    output->emit_lor(N_XOR, prefix, variable, offset, reg);
                } else if (type == C_OR) {
                    output->emit_lor(N_MVI, prefix, variable, offset, 4);
                    output->emit_r(N_COMR, 4);
                    output->emit_rr(N_ANDR, 4, reg);
                    output->emit_lor(N_XOR, prefix, variable, offset, reg);
                } else if (type == C_EQUAL) {
                    output->emit_lor(N_CMP, prefix, variable, offset, reg);
                    if (decision) {
                        output->emit_a(N_BNE, TEMP_PREFIX, decision);
                        optimized = true;
                    } else {
                        int label = next_local++;
                        
                        output->emit_nr(N_MVII, "", -1, reg);
                        output->emit_a(N_BEQ, TEMP_PREFIX, label);   // two words of jump and one word of INCR
                        output->emit_r(N_INCR, reg);
                        output->emit_l(TEMP_PREFIX, label);
                        output->trash_partial(reg);
                    }
                } else if (type == C_NOTEQUAL) {
                    output->emit_lor(N_CMP, prefix, variable, offset, reg);
                    if (decision) {
                        output->emit_a(N_BEQ, TEMP_PREFIX, decision);
                        optimized = true;
                    } else {
                        int label = next_local++;

                        output->emit_nr(N_MVII, "", -1, reg);
                        output->emit_a(N_BNE, TEMP_PREFIX, label);   // two words of jump and one word of INCR
                        output->emit_r(N_INCR, reg);
                        output->emit_l(TEMP_PREFIX, label);
                        output->trash_partial(reg);
                    }
                } else if (type == C_LESS) {
                    output->emit_lor(N_CMP, prefix, variable, offset, reg);
                    if (decision) {
                        output->emit_a(value ? N_BC : N_BGE, TEMP_PREFIX, decision);
                        optimized = true;
                    } else {
                        int label = next_local++;

                        output->emit_nr(N_MVII, "", -1, reg);
                        output->emit_a(value ? N_BNC : N_BLT, TEMP_PREFIX, label);   // two words of jump and one word of INCR
                        output->emit_r(N_INCR, reg);
                        output->emit_l(TEMP_PREFIX, label);
                        output->trash_partial(reg);
                    }
                } else if (type == C_LESSEQUAL) {
                    output->emit_lor(N_CMP, prefix, variable, offset, reg);
                    if (decision) {
                        if (value) {    // Unsigned
                            output->emit_a(N_BEQ, "", 4);
                            output->emit_a(N_BC, TEMP_PREFIX, decision);
                        } else {
                            output->emit_a(N_BGT, TEMP_PREFIX, decision);
                        }
                        optimized = true;
                    } else {
                        int label = next_local++;

                        output->emit_nr(N_MVII, "", -1, reg);
                        if (value) {    // Unsigned
                            output->emit_a(N_BEQ, TEMP_PREFIX, label);   // 2+2+1
                            output->emit_a(N_BNC, TEMP_PREFIX, label);   // two words of jump and one word of INCR
                        } else {
                            output->emit_a(N_BLE, TEMP_PREFIX, label);   // two words of jump and one word of INCR
                        }
                        output->emit_r(N_INCR, reg);
                        output->emit_l(TEMP_PREFIX, label);
                        output->trash_partial(reg);
                    }
                } else if (type == C_GREATER) {
                    output->emit_lor(N_CMP, prefix, variable, offset, reg);
                    if (decision) {
                        if (value) {    // Unsigned
                            output->emit_a(N_BEQ, TEMP_PREFIX, decision);
                            output->emit_a(N_BNC, TEMP_PREFIX, decision);
                        } else {
                            output->emit_a(N_BLE, TEMP_PREFIX, decision);
                        }
                        optimized = true;
                    } else {
                        int label = next_local++;

                        output->emit_nr(N_MVII, "", -1, reg);
                        if (value) {    // Unsigned
                            output->emit_a(N_BEQ, TEMP_PREFIX, label);   // 2+2
                            output->emit_a(N_BC, TEMP_PREFIX, label);   // two words of jump and one word of INCR
                        } else {
                            output->emit_a(N_BGT, TEMP_PREFIX, label);   // two words of jump and one word of INCR
                        }
                        output->emit_r(N_INCR, reg);
                        output->emit_l(TEMP_PREFIX, label);
                        output->trash_partial(reg);
                    }
                } else if (type == C_GREATEREQUAL) {
                    output->emit_lor(N_CMP, prefix, variable, offset, reg);
                    if (decision) {
                        output->emit_a(value ? N_BNC : N_BLT, TEMP_PREFIX, decision);
                        optimized = true;
                    } else {
                        int label = next_local++;

                        output->emit_nr(N_MVII, "", -1, reg);
                        output->emit_a(value ? N_BC : N_BGE, TEMP_PREFIX, label);   // two words of jump and one word of INCR
                        output->emit_r(N_INCR, reg);
                        output->emit_l(TEMP_PREFIX, label);
                        output->trash_partial(reg);
                    }
                } else if (type == C_MUL) {
                    if (jlp_used) {
                        output->emit_lor(N_MVI, prefix, variable, offset, 4);
                        output->emit_rl(N_MVO, reg, "", 0x9f86);
                        output->emit_rl(N_MVO, 4, "", 0x9f87);
                        output->emit_lr(N_MVI, "", 0x9f8e, reg);
                    } else {
                        int label = next_local++;
                        int label2 = next_local++;
                        int label3 = next_local++;
                        int label4 = next_local++;
                        
                        output->emit_lor(N_MVI, prefix, variable, offset, 5);
                        output->emit_r(N_CLRR, 4);
                        output->emit(N_CLRC);
                        output->emit_s(N_RRC, reg, 1);
                        output->emit_a(N_BEQ, TEMP_PREFIX, label2);
                        output->emit_l(TEMP_PREFIX, label);
                        output->emit_a(N_BNC, TEMP_PREFIX, label3);  // Two words of jump and one of ADDR
                        output->emit_rr(N_ADDR, 5, 4);
                        output->emit_l(TEMP_PREFIX, label3);
                        output->emit_rr(N_ADDR, 5, 5);
                        output->emit_s(N_SARC, reg, 1);
                        output->emit_a(N_BNE, TEMP_PREFIX, label);
                        output->emit_l(TEMP_PREFIX, label2);
                        output->emit_a(N_BNC, TEMP_PREFIX, label4);  // Two words of jump and one of ADDR
                        output->emit_rr(N_ADDR, 5, 4);
                        output->emit_l(TEMP_PREFIX, label4);
                        output->emit_rr(N_MOVR, 4, reg);
                        output->trash_partial(reg);
                    }
                } else if (type == C_DIV) {
                    if (jlp_used) {
                        output->emit_lor(N_MVI, prefix, variable, offset, 4);
                        output->emit_rl(N_MVO, reg, "", 0x9f8a);
                        output->emit_rl(N_MVO, 4, "", 0x9f8b);
                        output->emit_lr(N_MVI, "", 0x9f8e, reg);
                    } else {
                        int label = next_local++;
                        int label2 = next_local++;
                        
                        output->emit_lor(N_MVI, prefix, variable, offset, 4);
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
                        output->emit_lor(N_MVI, prefix, variable, offset, 4);
                        output->emit_rl(N_MVO, reg, "", 0x9f8a);
                        output->emit_rl(N_MVO, 4, "", 0x9f8b);
                        output->emit_lr(N_MVI, "", 0x9f8f, reg);
                    } else {
                        int label = next_local++;
                        int label2 = next_local++;
                        
                        output->emit_lor(N_MVI, prefix, variable, offset, 4);
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
                if (value == 0) {
                    output->emit_256(reg);
                    output->emit_rlo8(N_MVO, reg, LABEL_PREFIX, right->left->value, right->right->value);
                } else {
                    output->emit_rlo(N_MVO, reg, LABEL_PREFIX, right->left->value, right->right->value);
                }
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
                        int label = next_local++;
                        
                        output->emit_nr(N_MVII, "", -1, reg);
                        output->emit_a(N_BEQ, TEMP_PREFIX, label);
                        output->emit_r(N_INCR, reg);
                        output->emit_l(TEMP_PREFIX, label);
                        output->trash_partial(reg);
                    }
                } else if (type == C_NOTEQUAL) {
                    output->emit_rr(N_CMPR, reg + 1, reg);
                    if (decision) {
                        output->emit_a(N_BEQ, TEMP_PREFIX, decision);
                        optimized = true;
                    } else {
                        int label = next_local++;

                        output->emit_nr(N_MVII, "", -1, reg);
                        output->emit_a(N_BNE, TEMP_PREFIX, label);
                        output->emit_r(N_INCR, reg);
                        output->emit_l(TEMP_PREFIX, label);
                        output->trash_partial(reg);
                    }
                } else if (type == C_LESS) {
                    output->emit_rr(N_CMPR, reg + 1, reg);
                    if (decision) {
                        if (value) {    // Unsigned comparison
                            if (reversed) {
                                output->emit_a(N_BEQ, TEMP_PREFIX, decision);
                                output->emit_a(N_BNC, TEMP_PREFIX, decision);
                            } else {
                                output->emit_a(N_BC, TEMP_PREFIX, decision);
                            }
                        } else {
                            output->emit_a(reversed ? N_BLE : N_BGE, TEMP_PREFIX, decision);
                        }
                        optimized = true;
                    } else {
                        int label = next_local++;

                        output->emit_nr(N_MVII, "", -1, reg);
                        if (value) {    // Unsigned comparison
                            if (reversed) {
                                output->emit_a(N_BEQ, TEMP_PREFIX, label);
                                output->emit_a(N_BC, TEMP_PREFIX, label);
                            } else {
                                output->emit_a(N_BNC, TEMP_PREFIX, label);
                            }
                        } else {
                            output->emit_a(reversed ? N_BGT : N_BLT, TEMP_PREFIX, label);
                        }
                        output->emit_r(N_INCR, reg);
                        output->emit_l(TEMP_PREFIX, label);
                        output->trash_partial(reg);
                    }
                } else if (type == C_LESSEQUAL) {
                    output->emit_rr(N_CMPR, reg + 1, reg);
                    if (decision) {
                        if (value) {    // Unsigned cpmparison
                            if (reversed) {
                                output->emit_a(N_BNC, TEMP_PREFIX, decision);
                            } else {
                                output->emit_a(N_BEQ, "", 4);
                                output->emit_a(N_BC, TEMP_PREFIX, decision);
                            }
                        } else {
                            output->emit_a(reversed ? N_BLT : N_BGT, TEMP_PREFIX, decision);
                        }
                        optimized = true;
                    } else {
                        int label = next_local++;

                        output->emit_nr(N_MVII, "", -1, reg);
                        if (value) {    // Unsigned comparison
                            if (reversed) {
                                output->emit_a(N_BC, TEMP_PREFIX, label);
                            } else {
                                output->emit_a(N_BEQ, TEMP_PREFIX, label);
                                output->emit_a(N_BNC, TEMP_PREFIX, label);
                            }
                        } else {
                            output->emit_a(reversed ? N_BGE : N_BLE, TEMP_PREFIX, label);
                        }
                        output->emit_r(N_INCR, reg);
                        output->emit_l(TEMP_PREFIX, label);
                        output->trash_partial(reg);
                    }
                } else if (type == C_GREATER) {
                    output->emit_rr(N_CMPR, reg + 1, reg);
                    if (decision) {
                        if (value) {    // Unsigned
                            if (reversed) {
                                output->emit_a(N_BC, TEMP_PREFIX, decision);
                            } else {
                                output->emit_a(N_BEQ, TEMP_PREFIX, decision);
                                output->emit_a(N_BNC, TEMP_PREFIX, decision);
                            }
                        } else {
                            output->emit_a(reversed ? N_BGE : N_BLE, TEMP_PREFIX, decision);
                        }
                        optimized = true;
                    } else {
                        int label = next_local++;

                        output->emit_nr(N_MVII, "", -1, reg);
                        if (value) {    // Unsigned
                            if (reversed) {
                                output->emit_a(N_BNC, TEMP_PREFIX, label);
                            } else {
                                output->emit_a(N_BEQ, TEMP_PREFIX, label);
                                output->emit_a(N_BC, TEMP_PREFIX, label);
                            }
                        } else {
                            output->emit_a(reversed ? N_BLT : N_BGT, TEMP_PREFIX, label);
                        }
                        output->emit_r(N_INCR, reg);
                        output->emit_l(TEMP_PREFIX, label);
                        output->trash_partial(reg);
                    }
                } else if (type == C_GREATEREQUAL) {
                    output->emit_rr(N_CMPR, reg + 1, reg);
                    if (decision) {
                        if (value) {    // Unsigned
                            if (reversed) {
                                output->emit_a(N_BEQ, "", 4);
                                output->emit_a(N_BC, TEMP_PREFIX, decision);
                            } else {
                                output->emit_a(N_BNC, TEMP_PREFIX, decision);
                            }
                        } else {
                            output->emit_a(reversed ? N_BGT : N_BLT, TEMP_PREFIX, decision);
                        }
                        optimized = true;
                    } else {
                        int label = next_local++;

                        output->emit_nr(N_MVII, "", -1, reg);
                        if (value) {    // Unsigned
                            if (reversed) {
                                output->emit_a(N_BEQ, TEMP_PREFIX, label);
                                output->emit_a(N_BNC, TEMP_PREFIX, label);
                            } else {
                                output->emit_a(N_BC, TEMP_PREFIX, label);
                            }
                        } else {
                            output->emit_a(reversed ? N_BLE : N_BGE, TEMP_PREFIX, label);
                        }
                        output->emit_r(N_INCR, reg);
                        output->emit_l(TEMP_PREFIX, label);
                        output->trash_partial(reg);
                    }
                } else if (type == C_MUL) {
                    if (jlp_used) {
                        output->emit_rl(N_MVO, reg, "", 0x9f86);
                        output->emit_rl(N_MVO, reg + 1, "", 0x9f87);
                        output->emit_lr(N_MVI, "", 0x9f8e, reg);
                    } else {
                        int label = next_local++;
                        int label2 = next_local++;
                        int label3 = next_local++;
                        int label4 = next_local++;
                        
                        output->emit_r(N_CLRR, 4);
                        output->emit(N_CLRC);
                        output->emit_s(N_RRC, reg, 1);
                        output->emit_a(N_BEQ, TEMP_PREFIX, label2);
                        output->emit_l(TEMP_PREFIX, label);
                        output->emit_a(N_BNC, TEMP_PREFIX, label3);  // Two words of jump and one of ADDR
                        output->emit_rr(N_ADDR, reg + 1, 4);
                        output->emit_l(TEMP_PREFIX, label3);
                        output->emit_rr(N_ADDR, reg + 1, reg + 1);
                        output->emit_s(N_SARC, reg, 1);
                        output->emit_a(N_BNE, TEMP_PREFIX, label);
                        output->emit_l(TEMP_PREFIX, label2);
                        output->emit_a(N_BNC, TEMP_PREFIX, label4);  // Two words of jump and one of ADDR
                        output->emit_rr(N_ADDR, reg + 1, 4);
                        output->emit_l(TEMP_PREFIX, label4);
                        output->emit_rr(N_MOVR, 4, reg);
                        output->trash_partial(reg);
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

