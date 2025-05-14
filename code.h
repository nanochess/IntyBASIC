//
//  code.h
//  intybasic
//
//  Created by Oscar Toledo on 07/01/16.
//  Copyright (c) 2016 Oscar Toledo. All rights reserved.
//

#ifndef _intybasic_code_
#define _intybasic_code_

//
// Intermediate code generator with peephole optimization.
//
class code {
    
    list <class microcode *> everything;
    
    int cycles;
    
    // Relates which registers contain which constants or addresses.
    struct {
        int valid;          // This content is valid.
        string prefix;      // Refers to this prefix label... (empty string for value)
        int value;          // ...contains this label number or value...
        int offset;         // ...plus this offset.
    } register_content[8];
    
    int flags_valid;        // Which flags are valid currently.
    int flags_register;     // Which register evaluation is in flags.
    
    bool subexpression_valid;   // Subexpression in r3 is valid...
    int subexpression_base;
    int subexpression_offset;
    int subexpression_index;
    
    // Relates which registers are in which memory locations
    struct {
        int valid;          // This content is valid
        string prefix;
        int value;
        int offset;
    } register_memory[8];
    
    // Envelope for code trying to avoid PUSH/PULR
    list <class microcode *> subeverything;  // This sounds so deep
    int push_register;
    
public:
    code(void);
    bool subexpression_available(int base, int offset, int index);
    void annotate_subexpression(int base, int offset, int index);
    void trash_registers(void);
    void trash_partial(int r);
    void check_for_cycles(int how_many, int limit);
    void push(int r);
    void pop(void);
    void emit_256(int r1);
    void emit(enum opcode type);
    void emit_r(enum opcode type, int r1);
    void emit_rr(enum opcode type, int r1, int r2);
    void emit_nr(enum opcode type, string prefix, int value, int r);
    void emit_nor(enum opcode type, string prefix, int value, int offset, int r);
    void emit_lr(enum opcode type, string prefix, int value, int r);
    void emit_lor(enum opcode type, string prefix, int value, int offset, int r);
    void emit_rl(enum opcode type, int r, string prefix, int value);
    void emit_rlo8(enum opcode type, int r, string prefix, int value, int offset);
    void emit_rlo(enum opcode type, int r, string prefix, int value, int offset);
    void emit_a(enum opcode type, string prefix, int value);
    void emit_s(enum opcode type, int r, int s);
    void emit_m(enum opcode type, int r1, int r2, int v);
    void emit_l(string prefix, int value);
    void emit_l3(string prefix, int value);
    void emit_d(enum opcode type, int d);
    void emit_d2(enum opcode type, int d1, int d2);
    void emit_dl(enum opcode type, string prefix, int value);
    void emit_dlo(enum opcode type, string prefix, int value, int offset);
    void emit_literal(string text);
    void emit_nnr(enum opcode type, int r, int array1, int array2, int diff);
    void dump(void);
};

#endif /* defined(_intybasic_code_) */
