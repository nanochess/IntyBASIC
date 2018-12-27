//
//  global.h
//  intybasic
//
//  Created by Oscar Toledo on 07/01/16.
//  Copyright (c) 2016 Oscar Toledo. All rights reserved.
//

#ifndef intybasic_global_h
#define intybasic_global_h

#include <iostream>
#include <fstream>
#include <string>
#include <map>

using namespace std;

extern const string LABEL_PREFIX;   // Prefix for BASIC labels
extern const string TEMP_PREFIX;    // Prefix for temporal labels
extern const string VAR_PREFIX;     // Prefix for BASIC variables
extern const string FUNC_PREFIX;    // Prefix for USR functions

extern map <int, string> name_mangling;    // Map from label number to name
extern map <int, string> name_mangling_var;    // Map from label number to name

extern void mangle(string);
extern void mangle_label(string, int);

extern class code *output;

extern int next_local;

extern ofstream asm_output;         // IntyBASIC.cpp

extern bool optimized;              // Indicates if expression for IF statement jump was optimized
extern bool jlp_used;               // Indicates if JLP is used
extern bool fastmult_used;          // Indicates if fast multiplication is used
extern bool fastdiv_used;           // Indicates if fast division/remainder is used
extern bool music_used;             // Indicates if music used
extern int err_code;

// Lexical components mixed with expression tree node types
enum lexical_component {C_END, C_NAME, C_NAME_R, C_NAME_RO,
    C_STRING, C_LABEL, C_NUM,
    C_OR, C_XOR, C_AND, C_NOT, C_NEG, C_PEEK, C_ABS, C_SGN,
    C_READ, C_VAR, C_USR, C_RAND, C_RANDOM, C_EXTEND,
    C_ASSIGN,
    C_EQUAL, C_NOTEQUAL, C_LESS, C_LESSEQUAL, C_GREATER, C_GREATEREQUAL,
    C_PLUS, C_MINUS, C_PLUSF, C_MINUSF, C_MUL, C_DIV, C_MOD,
    C_LPAREN, C_RPAREN, C_COLON, C_PERIOD, C_COMMA,
    C_ERR};

#endif
