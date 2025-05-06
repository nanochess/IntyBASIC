//
//  main.cpp
//  intybasic
//
//  Created by Oscar Toledo on 13/01/14.
//  Copyright (c) 2014 Oscar Toledo. All rights reserved.
//
//  Revision notes are now in Git.
//
//

//  TODO:
//  * Debug.

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
#include <cstdlib>
#include <cstring>
#include <ctime>

using namespace std;

#include "global.h"     // Global definitions
#include "microcode.h"  // Class microcode
#include "code.h"       // Class code
#include "node.h"       // Class node

const string VERSION = "v1.5.1 May/06/2025";      // Compiler version

const string LABEL_PREFIX = "Q";    // Prefix for BASIC labels
const string TEMP_PREFIX = "T";     // Prefix for temporal labels
const string VAR_PREFIX = "V";      // Prefix for BASIC variables
const string FUNC_PREFIX = "F";     // Prefix for USR functions

const string NAME_MANGLING_ASSEMBLER = "";
const string NAME_MANGLING_VAR = "var_";
const string NAME_MANGLING_ARRAY = "array_";
const string NAME_MANGLING_LABEL = "label_";
const string NAME_MANGLING_CONST = "const_";

const int CALLED_BY_GOTO = 0x04;
const int CALLED_BY_GOSUB = 0x08;
const int IT_IS_PROCEDURE = 0x10;

class code *output;

int next_local = 1;

ofstream asm_output;

bool optimized;      // Indicates if expression for IF statement jump was optimized.
bool jlp_used;       // Indicates if JLP is used.
int jlp_flash_size;  // Indicates JLP flash size.
bool cc3_used;       // Indicates if CC3 is used.
bool fastmult_used;  // Indicates if fast multiplication is used.
bool fastdiv_used;   // Indicates if fast division/remainder is used.
bool music_used;     // Indicates if music used.
bool music_ecs_used; // Indicates if ECS music used.
bool warnings;       // Indicates if warnings are generated.
bool col_used;       // Indicates if collisions are used.
int intybasic_map;   // ROM map used.

int program_year;
char program_title[256];

int err_code;

bool option_explicit;   // Force declaration of variables
bool option_warnings;   // Enable/disable warnings

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
// Representation for a loop
//
struct loop {
    enum {NESTED_FOR, NESTED_WHILE, NESTED_IF, NESTED_DO, NESTED_DO_LOOP, NESTED_SELECT} type;
    class node *step;
    class node *final;
    string var;
    int label_loop;     // Main label, in C this would be destination for 'continue'
    int label_exit;     // Exit label, in C this would be destination for 'break'
};

//
// Dictionary of arrays and variables
//
map <int, string> name_mangling;    // Map from label number to name
map <int, string> name_mangling_var;    // Map from label number to name

//
// Mangle string for compatibility with as1600
//
void mangle(string name)
{
    int c;
    
    for (c = 0; c < (int) name.length(); c++) {
        if (name[c] == '#')
            asm_output << '&';
        else
            asm_output << name[c];
    }
}

//
// Retranslate label number into original name
//
void mangle_label(string prefix, int value)
{
    if (prefix == VAR_PREFIX && name_mangling_var[value] != "") {
        mangle(name_mangling_var[value]);
        return;
    }
    if ((prefix == LABEL_PREFIX || prefix == FUNC_PREFIX) && name_mangling[value] != "") {
        mangle(name_mangling[value]);
        return;
    }
    asm_output << prefix << value;
}

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
    int active_include;
    int saved_line_number;
    string line;
    int line_start;
    string assigned;
    size_t line_pos;
    size_t line_size;
    map <string, int> arrays;       // Arrays (DIM)
    map <string, int> labels;       // Labels (reference name -> label number)
    map <string, int> variables;    // Variables (reference name -> label number)
    map <string, int> constants;    // Constants (reference name -> value)
    map <string, int> functions;    // Functions (reference USR name -> intermediate label)

    // Note how following two could be mixed with 'variables' and 'labels' using struct instead of int
    map <string, int> read_write;   // Variables (reference name -> read/write)
    map <string, int> label_used;   // Labels (reference name -> used/defined)
    map <string, int> signedness;   // Variables/array (reference name -> signed/unsigned)
    
    map <string, macro *> macros;
    map <string, int>::iterator access;
    map <int, list <int> >::iterator access2;
    list <struct loop> loops;
    list <struct loop>::iterator loop_explorer;

    string global_label;            // Current global label
    int inside_proc;                // Current procedure label (main is zero)
    map <int, int> label_proc;      // Procedure of each label
    map <int, list <int> > proc_gotos;  // Procedures and list of GOTO's label inside each one (main is zero)
    
    int line_number;
    int next_label;
    int next_var;
    
    char path[4096];  // For Windows and Linux
    //char path[PATH_MAX];  // Only works in Mac OS X :/

    enum lexical_component lex;
    int value;
    string name;
    list <lexical_element *> accumulated;
    
    int bitmap_value;
    int bitmap_byte;
    int bitmap_state;
    int frame_drive;    // Label for frame-driven game (ON FRAME GOSUB)
    int last_is_return; // Indicates if last statement processed was a RETURN
    bool scroll_used;   // Indicates if scroll used
    bool keypad_used;   // Indicates if keypad used
    bool stack_used;    // Indicates if stack check used
    bool numbers_used;  // Indicates if numbers used (PRINT)
    bool voice_used;    // Indicates if Intellivoice used (VOICE)
    bool ecs_used;      // Indicates if ECS used (SOUND, CONT3, CONT4)
    bool flash_used;    // Indicates if JLP Flash is used (FLASH)
    bool playvol_used;  // Indicates if PLAY VOLUME is used.
    
    //
    // Avoid spaces
    //
    int skip_spaces(void) {
        int something = 0;
        
        if (line_pos == 0)
            something = 1;
        while (line_pos < line_size && isspace(line[line_pos])) {
            line_pos++;
            something = 1;
        }
        return something;
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
        return toupper(line[line_pos]);
    }
    
    //
    // Gets another lexical component
    // Output:
    //  lex = lexical component
    //  name = identifier or string
    //  value = value
    //
    void get_lex(void) {
        int spaces;
        
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
        spaces = skip_spaces();
        if (line_pos == line_size) {
            lex = C_END;
            return;
        }
        if (isalpha(line[line_pos]) ||
            line[line_pos] == '#' ||
            (spaces && line[line_pos] == '.') ||
            (line_pos > 0 && line[line_pos - 1] == ',' && line[line_pos] == '.') ||
            (line_pos > 0 && line[line_pos - 1] == ':' && line[line_pos] == '.')) {  // Name, label or local label
            if (line[line_pos] == '.') {
                name = global_label;
                value = 1;
            } else {
                name = "";
                value = 0;
            }
            name += toupper(line[line_pos]);
            line_pos++;
            while (line_pos < line_size
             && (isalnum(line[line_pos]) || line[line_pos] == '_' || line[line_pos] == '#')) {
                name += toupper(line[line_pos]);
                line_pos++;
            }
            if (line_pos < line_size && line[line_pos] == ':' && line_start
            && name != "RETURN" && name != "CLS" && name != "WAIT"
            && name != "RESTORE" && name != "STACK_CHECK" && name != "WEND"
            && name != "DO" && name != "NEXT") {
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
            int something;
            
            value = 0;
            while (line_pos < line_size && isdigit(line[line_pos]))
                value = (value * 10) + line[line_pos++] - '0';
            if (value > 65535) {
                emit_warning("number exceeds 16 bits");
            }
            name = "";
            if (line_pos < line_size && (line[line_pos] == 'u' || line[line_pos] == 'U')) {
                line_pos++;
                name = "u";
            } else if (line_pos < line_size && line[line_pos] == '.'
                && line_pos + 1 < line_size && isdigit(line[line_pos + 1])) {
                if (value > 255) {
                    emit_warning("fixed number exceeds basic 8 bits");
                }
                line_pos++;
                fraction = 0;
                something = 0;
                if (line_pos < line_size && isdigit(line[line_pos])) {
                    something += line[line_pos] - '0';
                    fraction += (line[line_pos++] - '0') * 100;
                }
                if (line_pos < line_size && isdigit(line[line_pos])) {
                    something += line[line_pos] - '0';
                    fraction += (line[line_pos++] - '0') * 10;
                }
                if (line_pos < line_size && isdigit(line[line_pos])) {
                    something += line[line_pos] - '0';
                    fraction += (line[line_pos++] - '0');
                }
                while (line_pos < line_size && isdigit(line[line_pos])) {
                    something += line[line_pos] - '0';
                    line_pos++;
                }
                if (something != 0 && fraction == 0) {
                    emit_warning("fraction of fixed number is zero (hasn't enough precision)ß");
                }
                value += (int) (fraction * (256.0 / 1000.0) + 0.5) * 256;
                name = ".";
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
            name = "";
            if (line_pos < line_size && (line[line_pos] == 'u' || line[line_pos] == 'U')) {
                line_pos++;
                name = "u";
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
            name = "";
            if (line_pos < line_size && (line[line_pos] == 'u' || line[line_pos] == 'U')) {
                line_pos++;
                name = "u";
            }
            lex = C_NUM;
            line_start = 0;
            return;
        }
        if (line[line_pos] == '"') {  // String
            line_pos++;
            name = "";
            while (line_pos < line_size && line[line_pos] != '"') {
                int c;
                int digits;
                
                if (line[line_pos] == '\\') {
                    line_pos++;
                    if (line_pos < line_size && (line[line_pos] == '"' || line[line_pos] == '\\')) {
                        c = line[line_pos++] - 32;
                        if (c < 0)
                            c = 0;
                        if (c > 95)
                            c = 95;
                    } else {
                        c = 0;
                        digits = 0;
                        while (line_pos < line_size && isdigit(line[line_pos])) {
                            c = c * 10 + (line[line_pos] - '0');
                            line_pos++;
                            if (++digits == 3)
                                break;
                        }
                        if (c > 382)
                            c = 382;
                        if (c >= 127) {
                            name += 127;
                            c -= 127;
                        }
                    }
                } else {
                    c = line[line_pos++] - 32;
                    if (c < 0)
                        c = 0;
                    if (c > 95)
                        c = 95;
                }
                name += c;
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
    // Mix signedness
    //
    int mix_signedness(int type1, int type2)
    {
        return type1 | type2;   // Unsigned wins
    }
    
    //
    // Evaluates an expression
    // Result in R0
    //
    enum lexical_component eval_expr(int reg, int decision)
    {
        class node *tree;
        enum lexical_component c;
        int type;
        
        tree = eval_level0(&type);
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
    class node *eval_level0(int *type)
    {
        class node *left;
        class node *right;
        int type2;
        int small_ops;
        
        small_ops = 0;
        left = eval_level1(type, &small_ops);
        while (1) {
            if (lex == C_NAME && name == "OR") {
                get_lex();
                if (small_ops)
                    emit_warning("Small operators used at left side of OR (parenthesis required?)");
                small_ops = 0;
                right = eval_level1(&type2, &small_ops);
                if (small_ops)
                    emit_warning("Small operators used at right side of OR (parenthesis required?)");
                left = new node(C_OR, 0, left, right);
                *type = mix_signedness(*type, type2);
            } else {
                break;
            }
        }
        return left;
    }
    
    //
    // Expression evaluation: Level 1 (XOR)
    //
    class node *eval_level1(int *type, int *small_ops)
    {
        class node *left;
        class node *right;
        int type2;
        
        left = eval_level2(type, small_ops);
        while (1) {
            if (lex == C_NAME && name == "XOR") {
                get_lex();
                if (*small_ops)
                    emit_warning("Small operators used at left side of XOR (parenthesis required?");
                *small_ops = 0;
                right = eval_level2(&type2, small_ops);
                if (*small_ops)
                    emit_warning("Small operators used at right side of XOR (parenthesis required?");
                left = new node(C_XOR, 0, left, right);
                *type = mix_signedness(*type, type2);
            } else {
                break;
            }
        }
        return left;
    }
    
    //
    // Expression evaluation: Level 2 (AND)
    //
    class node *eval_level2(int *type, int *small_ops)
    {
        class node *left;
        class node *right;
        int type2;
        
        left = eval_level3(type, small_ops);
        while (1) {
            if (lex == C_NAME && name == "AND") {
                get_lex();
                if (*small_ops)
                    emit_warning("Small operators used at left side of AND (parenthesis required?");
                *small_ops = 0;
                right = eval_level3(&type2, small_ops);
                if (*small_ops)
                    emit_warning("Small operators used at right side of AND (parenthesis required?");
                left = new node(C_AND, 0, left, right);
                *type = mix_signedness(*type, type2);
            } else {
                break;
            }
        }
        return left;
    }
    
    //
    // Expression evaluation: Level 3 (= <> < <= > >=)
    //
    class node *eval_level3(int *type, int *small_ops)
    {
        class node *left;
        class node *right;
        int type2;
        
        left = eval_level4(type, small_ops);
        while (1) {
            if (lex == C_EQUAL) {
                get_lex();
                right = eval_level4(&type2, small_ops);
                left = new node(C_EQUAL, 0, left, right);
                *type = 0;
            } else if (lex == C_NOTEQUAL) {
                get_lex();
                right = eval_level4(&type2, small_ops);
                left = new node(C_NOTEQUAL, 0, left, right);
                *type = 0;
            } else if (lex == C_LESS) {
                get_lex();
                right = eval_level4(&type2, small_ops);
                left = new node(C_LESS, *type | type2, left, right);
                *type = 0;
            } else if (lex == C_LESSEQUAL) {
                get_lex();
                right = eval_level4(&type2, small_ops);
                left = new node(C_LESSEQUAL, *type | type2, left, right);
                *type = 0;
            } else if (lex == C_GREATER) {
                get_lex();
                right = eval_level4(&type2, small_ops);
                left = new node(C_GREATER, *type | type2, left, right);
                *type = 0;
            } else if (lex == C_GREATEREQUAL) {
                get_lex();
                right = eval_level4(&type2, small_ops);
                left = new node(C_GREATEREQUAL, *type | type2, left, right);
                *type = 0;
            } else {
                break;
            }
            *small_ops = 0;
        }
        return left;
    }
    
    //
    // Expression evaluation: Level 4 (+ -)
    //
    class node *eval_level4(int *type, int *small_ops)
    {
        class node *left;
        class node *right;
        int type2;
        
        left = eval_level5(type, small_ops);
        while (1) {
            if (lex == C_PLUS) {
                get_lex();
                *small_ops = 1;
                right = eval_level5(&type2, small_ops);
                left = new node(C_PLUS, 0, left, right);
                *type = mix_signedness(*type, type2);
            } else if (lex == C_MINUS) {
                get_lex();
                *small_ops = 1;
                right = eval_level5(&type2, small_ops);
                left = new node(C_MINUS, 0, left, right);
                *type = mix_signedness(*type, type2);
            } else if (lex == C_PLUSF) {
                get_lex();
                *small_ops = 1;
                right = eval_level5(&type2, small_ops);
                left = new node(C_PLUSF, 0, left, right);
                *type = mix_signedness(*type, type2);
            } else if (lex == C_MINUSF) {
                get_lex();
                *small_ops = 1;
                right = eval_level5(&type2, small_ops);
                left = new node(C_MINUSF, 0, left, right);
                *type = mix_signedness(*type, type2);
            } else {
                break;
            }
        }
        return left;
    }
    
    //
    // Expression evaluation: Level 5 (* / %)
    //
    class node *eval_level5(int *type, int *small_ops)
    {
        class node *left;
        class node *right;
        int type2;
        
        left = eval_level6(type);
        while (1) {
            if (lex == C_MUL) {
                get_lex();
                *small_ops = 1;
                right = eval_level6(&type2);
                left = new node(C_MUL, 0, left, right);
                *type = mix_signedness(*type, type2);
            } else if (lex == C_DIV) {
                get_lex();
                *small_ops = 1;
                right = eval_level6(&type2);
                left = new node(C_DIV, 0, left, right);
                *type = mix_signedness(*type, type2);
            } else if (lex == C_MOD) {
                get_lex();
                *small_ops = 1;
                right = eval_level6(&type2);
                left = new node(C_MOD, 0, left, right);
                *type = mix_signedness(*type, type2);
            } else {
                break;
            }
        }
        return left;
    }
    
    //
    // Expression evaluation: Level 6 (NOT)
    //
    class node *eval_level6(int *type)
    {
        class node *left;
        
        if (lex == C_MINUS) {   // Unary -
            get_lex();
            if (lex == C_NUM && name == ".") {  // Ok, fractional value, special case
                value = ((value & 0xff00) >> 8) | ((value & 0x00ff) << 8);
                value = -value;
                value = ((value & 0xff00) >> 8) | ((value & 0x00ff) << 8);
                left = new node(C_NUM, value, NULL, NULL);
                get_lex();
            } else {
                left = eval_level6(type);
                left = new node(C_NEG, 0, left, NULL);
            }
        } else if (lex == C_PLUS) { // Unary +
            get_lex();
            left = eval_level6(type);
        } else if (lex == C_NAME && name == "NOT") {
            get_lex();
            left = eval_level6(type);
            left = new node(C_NOT, 0, left, NULL);
        } else if (lex == C_NAME && name == "SIGNED") {
            get_lex();
            left = eval_level6(type);
            *type = 0;
        } else if (lex == C_NAME && name == "UNSIGNED") {
            get_lex();
            left = eval_level6(type);
            *type = 1;
        } else {
            left = eval_level7(type);
        }
        return left;
    }
    
    //
    // Expression evaluation: Level 7 (parenthesis, functions, variables and values)
    //
    class node *eval_level7(int *type)
    {
        int type2;
        
        *type = 0;  /* Signed by default */
        if (lex == C_LPAREN) {
            class node *tree;
            
            get_lex();
            tree = eval_level0(type);
            if (lex != C_RPAREN)
                emit_error("missing right parenthesis");
            else
                get_lex();
            return tree;
        }
        if (lex == C_STRING) {
            int temp;
            
            if (name.length() == 0) {
                emit_error("empty string");
                temp = 0;
            } else {
                temp = name[0];
            }
            get_lex();
            return new node(C_NUM, temp, NULL, NULL);
        }
        if (lex == C_NAME) {
            class node *tree;
            int temp;
            int bits;
            int sign;
            
            if (name == "PEEK") {
                get_lex();
                if (lex != C_LPAREN)
                    emit_error("missing left parenthesis in PEEK");
                else
                    get_lex();
                tree = eval_level0(&type2);
                if (lex != C_RPAREN)
                    emit_error("missing right parenthesis in PEEK");
                else
                    get_lex();
                return new node(C_PEEK, 0, tree, NULL);
            } else if (name == "ABS") {
                get_lex();
                if (lex != C_LPAREN)
                    emit_error("missing left parenthesis in ABS");
                else
                    get_lex();
                tree = eval_level0(&type2);
                if (lex != C_RPAREN)
                    emit_error("missing right parenthesis in ABS");
                else
                    get_lex();
                return new node(C_ABS, 0, tree, NULL);
            } else if (name == "SGN") {
                get_lex();
                if (lex != C_LPAREN)
                    emit_error("missing left parenthesis in SGN");
                else
                    get_lex();
                tree = eval_level0(&type2);
                if (lex != C_RPAREN)
                    emit_error("missing right parenthesis in SGN");
                else
                    get_lex();
                return new node(C_SGN, 0, tree, NULL);
            } else if (name == "CONT" || name == "CONT1" || name == "CONT2" || name == "CONT3" || name == "CONT4") {
                int c;
                
                if (name == "CONT2") {
                    tree = new node(C_NUM, c = 0x01FE, NULL, NULL);
                    tree = new node(C_PEEK, 0, tree, NULL);
                    tree = new node(C_XOR, 0, tree, new node(C_NUM, 0xff, NULL, NULL));
                } else if (name == "CONT1") {
                    tree = new node(C_NUM, c = 0x01FF, NULL, NULL);
                    tree = new node(C_PEEK, 0, tree, NULL);
                    tree = new node(C_XOR, 0, tree, new node(C_NUM, 0xff, NULL, NULL));
                } else if (name == "CONT4") {
                    tree = new node(C_NUM, c = 0x00FE, NULL, NULL);
                    tree = new node(C_PEEK, 0, tree, NULL);
                    tree = new node(C_XOR, 0, tree, new node(C_NUM, 0xff, NULL, NULL));
                    ecs_used = true;
                } else if (name == "CONT3") {
                    tree = new node(C_NUM, c = 0x00FF, NULL, NULL);
                    tree = new node(C_PEEK, 0, tree, NULL);
                    tree = new node(C_XOR, 0, tree, new node(C_NUM, 0xff, NULL, NULL));
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
                            keypad_used = true;
                        }
                    } else {
                        emit_error("wrong name for CONT? syntax");
                    }
                    get_lex();
                }
                return tree;
            } else if (name == "COL0") {
                get_lex();
                col_used = true;
                return new node(C_VAR, 0, NULL, NULL);
            } else if (name == "COL1") {
                get_lex();
                col_used = true;
                return new node(C_VAR, 1, NULL, NULL);
            } else if (name == "COL2") {
                get_lex();
                col_used = true;
                return new node(C_VAR, 2, NULL, NULL);
            } else if (name == "COL3") {
                get_lex();
                col_used = true;
                return new node(C_VAR, 3, NULL, NULL);
            } else if (name == "COL4") {
                get_lex();
                col_used = true;
                return new node(C_VAR, 4, NULL, NULL);
            } else if (name == "COL5") {
                get_lex();
                col_used = true;
                return new node(C_VAR, 5, NULL, NULL);
            } else if (name == "COL6") {
                get_lex();
                col_used = true;
                return new node(C_VAR, 6, NULL, NULL);
            } else if (name == "COL7") {
                get_lex();
                col_used = true;
                return new node(C_VAR, 7, NULL, NULL);
            } else if (name == "FRAME") {
                get_lex();
                return new node(C_VAR, 8, NULL, NULL);
            } else if (name == "RAND") {
                get_lex();
                if (lex == C_LPAREN) {  // RAND with range
                    int c;
                    
                    get_lex();
                    tree = eval_level0(&type2);
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
                get_lex();
                if (lex != C_LPAREN)
                    emit_error("missing left parenthesis in RANDOM");
                else
                    get_lex();
                tree = eval_level0(&type2);
                if (lex != C_RPAREN)
                    emit_error("missing right parenthesis in RANDOM");
                else
                    get_lex();
                return new node(C_RANDOM, 0, tree, NULL);
            } else if (name == "NTSC") {
                get_lex();
                return new node(C_VAR, 12, NULL, NULL);
            } else if (name == "USR") {  // Call to function written in assembler
                get_lex();
                tree = process_usr(0);
                return tree;
            } else if (name == "VARPTR") {  // Access to variable/array/label address
                get_lex();
                *type = 1;  // Unsigned
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
                        label_used[name] |= 1;
                    } else {
                        labels[name] = temp = next_label++;
                        label_used[name] |= 1;
                        name_mangling[temp] = NAME_MANGLING_LABEL + name;
                    }
                    get_lex();
                    if (lex != C_LPAREN)
                        emit_error("missing left parenthesis in array access");
                    else
                        get_lex();
                    tree = eval_level0(&type2);
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
                if (variables[name] == 0) {
                    check_for_explicit(name);
                    variables[name] = next_var;
                    name_mangling_var[next_var] = NAME_MANGLING_VAR + name;
                    next_var++;
                }
                temp = variables[name];
                get_lex();
                return new node(C_NAME_R, temp, NULL, NULL);
            } else if (name == "LEN") { // Access to string length
                int c;
                
                get_lex();
                if (lex != C_LPAREN)
                    emit_error("missing left parenthesis in LEN");
                else
                    get_lex();
                if (lex != C_STRING) {
                    c = 0;
                    emit_error("missing string inside LEN");
                } else {
                    c = (int) name.length();
                    get_lex();
                }
                if (lex != C_RPAREN)
                    emit_error("missing right parenthesis in LEN");
                else
                    get_lex();
                return new node(C_NUM, c, NULL, NULL);
            } else if (name == "POS") { // Access to current screen position
                get_lex();
                if (lex != C_LPAREN)
                    emit_error("missing left parenthesis in POS");
                else
                    get_lex();
                tree = eval_level0(&type2);
                delete tree;
                if (lex != C_RPAREN)
                    emit_error("missing right parenthesis in POS");
                else
                    get_lex();
                return new node(C_MINUS, 0, new node(C_VAR, 15, NULL, NULL), new node(C_NUM, 512, NULL, NULL));
            } else if (name == "FLASH") {  // Access to JLP Flash parameters
                get_lex();
                if (!jlp_used)
                    emit_error("Using FLASH expression without using --jlp option");
                flash_used = true;
                if (lex != C_PERIOD)
                    emit_error("missing period in FLASH");
                else
                    get_lex();
                if (lex == C_NAME && name == "FIRST") {
                    get_lex();
                    return new node(C_VAR, 16, NULL, NULL);
                }
                if (lex == C_NAME && name == "LAST") {
                    get_lex();
                    return new node(C_VAR, 17, NULL, NULL);
                }
                emit_error("syntax error in FLASH");
                return new node(C_NUM, 0, NULL, NULL);
            } else if (name == "MUSIC") {  // Music status
                get_lex();
                if (lex != C_PERIOD)
                    emit_error("missing period in MUSIC");
                else
                    get_lex();
                if (lex == C_NAME && name == "PLAYING") {
                    get_lex();
                    return new node(C_VAR, 18, NULL, NULL);
                }
                emit_error("syntax error in MUSIC");
                return new node(C_NUM, 0, NULL, NULL);
            } else if (name == "VOICE") {  // Voice status
                get_lex();
                if (lex != C_PERIOD)
                    emit_error("missing period in VOICE");
                else
                    get_lex();
                if (lex == C_NAME && name == "AVAILABLE") {
                    get_lex();
                    return new node(C_VAR, 19, NULL, NULL);
                }
                if (lex == C_NAME && name == "PLAYING") {
                    get_lex();
                    return new node(C_VAR, 20, NULL, NULL);
                }
                emit_error("syntax error in VOICE");
                return new node(C_NUM, 0, NULL, NULL);
            } else if (name == "ECS") {  // ECS
                get_lex();
                if (lex != C_PERIOD)
                    emit_error("missing period in ECS");
                else
                    get_lex();
                if (lex == C_NAME && name == "AVAILABLE") {
                    get_lex();
                    return new node(C_VAR, 21, NULL, NULL);
                }
                emit_error("syntax error in ECS");
                return new node(C_NUM, 0, NULL, NULL);
            } else if (macros[name] != NULL) {  // Function (macro)
                if (replace_macro())
                    return new node(C_NUM, 0, NULL, NULL);
                return eval_level0(type);
            }
            
            // Take note for sign extension
            if (name[0] == '#')
                bits = 1;
            else
                bits = 0;
            sign = signedness[name];
            if (bits == 1 && sign == 2)  // UNSIGNED
                *type = 1;
            
            if (sneak_peek() == '(') {  // Indexed access
                if (arrays[name] != 0) {
                    temp = arrays[name] >> 16;
                } else if (labels[name] != 0) {
                    temp = labels[name];
                    label_used[name] |= 1;
                } else {
                    labels[name] = temp = next_label++;
                    label_used[name] |= 1;
                    name_mangling[temp] = NAME_MANGLING_LABEL + name;
                }
                get_lex();
                if (lex != C_LPAREN)
                    emit_error("missing left parenthesis in array access");
                else
                    get_lex();
                tree = eval_level0(&type2);
                if (lex != C_RPAREN)
                    emit_error("missing right parenthesis in array access");
                else
                    get_lex();
                tree = new node(C_PEEK, 0,
                                new node(C_PLUS, 0,
                                         new node(C_NAME_RO, temp, NULL, NULL), tree), NULL);
                if (bits == 0 && sign == 1)
                    tree = new node(C_EXTEND, 0, tree, NULL);
                return tree;
            }
            if ((constants[name] & 0x10000) != 0) {
                if ((constants[name] & 0x20000) != 0)
                    *type = 1;
                temp = constants[name] & 0xffff;
                get_lex();
                return new node(C_NUM, temp, NULL, NULL);
            }
            read_write[name] = (read_write[name] | 1);
            if (variables[name] == 0) {
                check_for_explicit(name);
                variables[name] = next_var;
                name_mangling_var[next_var] = NAME_MANGLING_VAR + name;
                next_var++;
            }
            temp = variables[name];
            get_lex();
            tree = new node(C_NAME, temp, NULL, NULL);
            if (bits == 0 && sign == 1)
                tree = new node(C_EXTEND, 0, tree, NULL);
            return tree;
        }
        if (lex == C_NUM) {
            int temp;
            
            temp = value;
            if (name == "u")
                *type = 1;
            get_lex();
            return new node(C_NUM, temp, NULL, NULL);
        }
        emit_error("bad syntax for expression");
        return new node(C_NUM, 0, NULL, NULL);
    }
    
    //
    // Process call to assembly language
    //
    class node *process_usr(int is_call)
    {
        class node *list;
        class node *last_list;
        int c;
        int temp;
        class node *tree;
        int type2;
        
        if (lex != C_NAME) {
            if (is_call)
                emit_error("missing function name in CALL");
            else
                emit_error("missing function name in USR");
        }
        if (functions[name] != 0) {
            temp = functions[name];
        } else {
            functions[name] = temp = next_label++;
            name_mangling[temp] = NAME_MANGLING_ASSEMBLER + name;
        }
        get_lex();
        tree = NULL;
        list = NULL;
        last_list = NULL;
        c = 0;
        if (lex == C_LPAREN) {
            get_lex();
            while (1) {
                tree = eval_level0(&type2);
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
        tree = new node(C_USR, temp, list, NULL);
        return tree;
    }
    
    //
    // Generates an error message
    //
    void emit_error(string message)
    {
        std::cerr << "Error: " << message << " in line " << line_number;
        if (active_include)
            std::cerr << ", file \"" << path << "\"";
        std::cerr << "\n";
        err_code = 1;
    }
    
    //
    // Generates a warning message
    //
    void emit_warning(string message)
    {
        if (!warnings)
            return;
        if (!option_warnings)
            return;
        std::cerr << "Warning: " << message << " in line " << line_number;
        if (active_include)
            std::cerr << ", file \"" << path << "\"";
        std::cerr << "\n";
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
        list <lexical_element *>::reverse_iterator explorer;
        list <lexical_element *>::reverse_iterator explorer2;
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
                    if (lex == C_END)   // Avoid possibility of being stuck
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
        } else {
            argument = NULL;
        }
        accumulated.push_front(new lexical_element(lex, value, name));  // The actual one for later
        // Push macro into lexical analyzer
        explorer = macros[function]->definition.rbegin();
        while (explorer != macros[function]->definition.rend()) {
            if ((*explorer)->get_lex() == C_ERR) {
                explorer2 = argument[(*explorer)->get_value()].rbegin();
                while (explorer2 != argument[(*explorer)->get_value()].rend()) {
                    accumulated.push_front(new lexical_element((*explorer2)->get_lex(), (*explorer2)->get_value(), (*explorer2)->get_name()));
                    ++explorer2;
                }
            } else {
                accumulated.push_front(new lexical_element((*explorer)->get_lex(), (*explorer)->get_value(), (*explorer)->get_name()));
            }
            ++explorer;
        }
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
    int compile_assignment(int is_read)
    {
        class node *tree2;
        int bits;
        int type;
        int c;
        
        if (lex != C_NAME) {
            emit_error("name required for assignment");
            return 65536;
        }
        if (name[0] == '#')
            bits = 1;
        else
            bits = 0;
        if (sneak_peek() == '(') {
            class node *tree;
            int temp;
            
            if (arrays[name] == 0) {
                emit_error("using array without previous DIM, autoassigning DIM(10)");
                arrays[name] = temp = 10 | (next_label++ << 16);
                name_mangling[temp >> 16] = NAME_MANGLING_ARRAY + name;
            }
            temp = arrays[name] >> 16;
            get_lex();
            if (lex != C_LPAREN)
                emit_error("missing left parenthesis in array access");
            else
                get_lex();
            tree = eval_level0(&type);
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
                tree2 = eval_level0(&type);
            }
            if (bits == 0 && tree2->node_type() == C_NUM) {
                c = tree2->node_value() & 0xffff;
                
                // Allows -128 to 255, some people were confused by the fact of a = -1
                if (c >= 0x0100 && c <= 0xff7f) {
                    char message[256];
                    
                    sprintf(message, "value %d doesn't fit in 8-bits array", c);
                    emit_warning(message);
                }
            }
            c = 65536;
            if (tree2->node_type() == C_NUM)
                c = tree2->node_value() & 0xffff;
            tree = new node(C_ASSIGN, bits, tree2,
                            new node(C_PLUS, 0,
                                        new node(C_NAME_RO, temp, NULL, NULL), tree));
            tree->label();
            optimized = false;
            tree->generate(0, 0);
            delete tree;
            tree = NULL;
            return c;
        }
        if (name != "COL0" && name != "COL1" && name != "COL2" && name != "COL3" && name != "COL4" && name != "COL5" && name != "COL6" && name != "COL7" && name != "FRAME" && name != "RAND" && name != "NTSC" && name != "CONT" && name != "CONT1" && name != "CONT2" && name != "CONT3" && name != "CONT4") {
            read_write[name] = (read_write[name] | 2);
        } else {
            string message;
            
            message = "assignment to internal variable '" + name + "' doesn't have effect, use another name";
            emit_warning(message);
        }
        if (variables[name] == 0) {
            check_for_explicit(name);
            variables[name] = next_var;
            name_mangling_var[next_var] = NAME_MANGLING_VAR + name;
            next_var++;
            if ((constants[name] & 0x10000) != 0) {
                string message;
                
                message = "variable '" + name + "' assigned but name already used for constant. Constant has priority";
                emit_warning(message);
            }
        }
        assigned = name;
        get_lex();
        if (is_read) {
            output->emit_lr(N_MVI, "_read", -1, 4);
            output->emit_rr(N_MVIA, 4, 0);
            output->emit_rl(N_MVO, 4, "_read", -1);
            c = 65536;
        } else {
            if (lex != C_EQUAL) {
                emit_error("required '=' for assignment");
                return 65536;
            }
            get_lex();
            tree2 = eval_level0(&type);
            if (bits == 0) {
                if (tree2->node_type() == C_NUM) {
                    int c = tree2->node_value() & 0xffff;
                    
                    // Allows -128 to 255, some people were confused by the fact of a = -1
                    if (c >= 0x0100 && c <= 0xff7f) {
                        char message[256];
                        
                        sprintf(message, "assignment of value %d doesn't fit in 8-bits variable", c);
                        emit_warning(message);
                    }
                }
                if (tree2->node_type() == C_EXTEND) {
                    tree2 = tree2->node_left();
                } else if ((tree2->node_type() == C_PLUS || tree2->node_type() == C_MINUS || tree2->node_type() == C_AND || tree2->node_type() == C_OR || tree2->node_type() == C_XOR)) {
                    tree2 = new node(tree2->node_type(), 0,
                                     tree2->node_left()->node_type() == C_EXTEND ? tree2->node_left()->node_left() : tree2->node_left(),
                                     tree2->node_right()->node_type() == C_EXTEND ? tree2->node_right()->node_left() : tree2->node_right());
                }
            }
            c = 65536;
            if (tree2->node_type() == C_NUM)
                c = tree2->node_value() & 0xffff;
            tree2->label();
            optimized = false;
            tree2->generate(0, 0);
            delete tree2;
            tree2 = NULL;
        }
        if (bits == 1) {
            output->emit_rl(N_MVO, 0, VAR_PREFIX, variables[assigned]);
        } else {
            output->emit_256(0);
            output->emit_rlo8(N_MVO, 0, VAR_PREFIX, variables[assigned], 0);
        }
        return c;
    }

    //
    // Check if explicit declaration is needed
    //
    void check_for_explicit(string name) {
        string message;
        
        if (!option_explicit)
            return;
        message = "variable '" + name + "' not defined previously";
        emit_error(message);
    }

    //
    // Process a BASIC statement
    //
    void compile_statement(bool check_for_else)
    {
        while (1) {
            if (lex == C_NAME) {
                last_is_return = 0;
                if (name == "GOTO") {
                    get_lex();
                    if (lex != C_NAME) {
                        emit_error("bad syntax for GOTO");
                    } else {
                        if (labels[name] == 0) {
                            labels[name] = next_label;
                            name_mangling[next_label] = NAME_MANGLING_LABEL + name;
                            next_label++;
                        }
                        label_used[name] |= 1;
                        label_used[name] |= CALLED_BY_GOTO;
                        output->emit_a(N_B, LABEL_PREFIX, labels[name]);
                        proc_gotos[inside_proc].push_back(labels[name]);
                        get_lex();
                    }
                } else if (name == "GOSUB") {
                    get_lex();
                    if (lex != C_NAME) {
                        emit_error("bad syntax for GOSUB");
                    } else {
                        if (labels[name] == 0) {
                            labels[name] = next_label;
                            name_mangling[next_label] = NAME_MANGLING_LABEL + name;
                            next_label++;
                        }
                        label_used[name] |= 1;
                        label_used[name] |= CALLED_BY_GOSUB;
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
                    struct loop new_loop;
                    int block;
                    
                    get_lex();
                    label = next_local++;
                    type = eval_expr(0, label);
                    if (!optimized) {
                        output->emit_r(N_TSTR, 0);
                        output->emit_a(N_BEQ, TEMP_PREFIX, label);
                    }
                    if (lex == C_NAME && name == "GOTO") {
                        compile_statement(false);
                        block = 0;
                    } else if (lex != C_NAME || name != "THEN") {
                        emit_error("missing THEN in IF");
                        block = 0;
                    } else {
                        get_lex();
                        if (lex == C_END) {
                            block = 1;
                            new_loop.type = loop::NESTED_IF;
                            new_loop.step = NULL;
                            new_loop.final = NULL;
                            new_loop.var = "";
                            new_loop.label_loop = label;
                            new_loop.label_exit = 0;
                            loops.push_front(new_loop);
                        } else {
                            compile_statement(true);
                            block = 0;
                        }
                    }
                    if (block) {
                        last_is_return = 0;  // Solves bug where last internal statement was RETURN
                        break;
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
                        compile_statement(true);
                        output->emit_l(TEMP_PREFIX, label2);
                    }
                    last_is_return = 0;  // Solves bug where last internal statement was RETURN
                } else if (name == "ELSEIF") {
                    enum lexical_component type;
                    
                    get_lex();
                    if (loops.size() == 0) {
                        emit_error("ELSEIF without IF");
                    } else if (loops.front().type != loop::NESTED_IF || loops.front().label_loop == 0) {
                        emit_error("bad nested ELSEIF");
                    } else {
                        if (loops.front().var != "1") {
                            loops.front().label_exit = next_local++;
                            loops.front().var = "1";
                        }
                        output->emit_a(N_B, TEMP_PREFIX, loops.front().label_exit);
                        output->emit_l(TEMP_PREFIX, loops.front().label_loop);
                        loops.front().label_loop = next_local++;
                        type = eval_expr(0, loops.front().label_loop);
                        if (!optimized) {
                            output->emit_r(N_TSTR, 0);
                            output->emit_a(N_BEQ, TEMP_PREFIX, loops.front().label_loop);
                        }
                        if (lex == C_NAME && name == "GOTO") {
                            compile_statement(false);
                        } else if (lex != C_NAME || name != "THEN") {
                            emit_error("missing THEN in ELSEIF");
                        } else {
                            get_lex();
                        }
                    }
                    if (lex == C_END)
                        break;
                    continue;
                } else if (name == "ELSE") {
                    if (check_for_else)
                        break;
                    get_lex();
                    if (loops.size() == 0) {
                        emit_error("ELSE without IF");
                    } else if (loops.front().type != loop::NESTED_IF) {
                        emit_error("bad nested ELSE");
                    } else if (loops.front().label_loop == 0) {
                        emit_error("more than one ELSE");
                    } else {
                        if (loops.front().var != "1") {
                            loops.front().label_exit = next_local++;
                            loops.front().var = "1";
                        }
                        output->emit_a(N_B, TEMP_PREFIX, loops.front().label_exit);
                        output->emit_l(TEMP_PREFIX, loops.front().label_loop);
                        loops.front().label_loop = 0;
                    }
                    if (lex == C_END)
                        break;
                    continue;
                } else if (name == "END") {
                    get_lex();
                    if (lex == C_NAME && name == "IF") {
                        get_lex();
                        if (loops.size() == 0 || loops.front().type != loop::NESTED_IF) {
                            emit_error("Bad nested END IF");
                        } else {
                            if (loops.front().var == "1")
                                output->emit_l(TEMP_PREFIX, loops.front().label_exit);
                            if (loops.front().label_loop != 0)
                                output->emit_l(TEMP_PREFIX, loops.front().label_loop);
                            loops.pop_front();
                        }
                    } else if (lex == C_NAME && name == "SELECT") {
                        get_lex();
                        if (loops.size() == 0 || loops.front().type != loop::NESTED_SELECT) {
                            emit_error("Bad nested END SELECT");
                        } else {
                            if (loops.front().label_loop != 0)
                                output->emit_l(TEMP_PREFIX, loops.front().label_loop);
                            output->emit_l(TEMP_PREFIX, loops.front().label_exit);
                            loops.pop_front();
                        }
                    } else {
                        emit_error("wrong END");
                    }
                } else if (name == "FOR") {  
                    int label_loop;
                    string loop;
                    class node *final = NULL;
                    class node *step = NULL;
                    struct loop new_loop;
                    bool positive;
                    int type;
                    int start_value;
                    int end_value;
                    int step_value;
                    
                    get_lex();
                    start_value = compile_assignment(0);
                    loop = assigned;
                    read_write[assigned] = (read_write[assigned] | 1);  // Take note it's used
                    label_loop = next_local++;
                    output->emit_l(TEMP_PREFIX, label_loop);
                    if (lex != C_NAME || name != "TO") {
                        emit_error("missing TO in FOR");
                    } else {
                        get_lex();
                        final = eval_level0(&type);
                        if (assigned[0] != '#' && final->node_type() == C_NUM && (final->node_value() & 0xffff) > 255) {
                            emit_warning("TO value is larger than 8-bits size of variable");
                        }
                        if (final->node_type() == C_NUM)
                            end_value = final->node_value() & 0xffff;
                        else
                            end_value = 65536;
                        positive = true;
                        if (lex == C_NAME && name == "STEP") {
                            get_lex();
                            if (lex == C_MINUS) {
                                get_lex();
                                step = eval_level0(&type);
                                if (step->node_type() == C_NUM)
                                    step_value = -step->node_value() & 0xffff;
                                else
                                    step_value = 65536;
                                step = new node(C_MINUS, 0,
                                                new node(C_NAME, variables[loop], 0, 0), step);
                                positive = false;
                            } else {
                                step = eval_level0(&type);
                                if (step->node_type() == C_NUM)
                                    step_value = step->node_value() & 0xffff;
                                else
                                    step_value = 65536;
                                step = new node(C_PLUS, 0,
                                                new node(C_NAME, variables[loop], 0, 0), step);
                            }
                        } else {
                            step_value = 1;
                            step = new node(C_NUM, 1, NULL, NULL);
                            step = new node(C_PLUS, 0,
                                            new node(C_NAME, variables[loop], 0, 0), step);
                        }
                        final = new node(positive ? C_GREATER : C_LESS, (loop[0] == '#' && signedness[loop] == 2) ? 1 : 0, new node(C_NAME, variables[loop], 0, 0), final);
                        if (start_value != 65536 && end_value != 65536 && step_value != 65536) {
                            if (start_value >= 32768)
                                start_value -= 65536;
                            if (end_value >= 32768)
                                end_value -= 65536;
                            if (step_value >= 32768)
                                step_value -= 65536;
                            if (start_value < end_value && step_value < 0)
                                emit_warning("TO greater than start value with negative STEP");
                            else if (start_value > end_value && step_value > 0)
                                emit_warning("TO less than start value with positive STEP");
                            else if (start_value != end_value && step_value == 0)
                                emit_warning("Infinite loop because STEP is zero");
                        }
                    }
                    new_loop.type = loop::NESTED_FOR;
                    new_loop.step = step;
                    new_loop.final = final;
                    new_loop.var = loop;
                    new_loop.label_loop = label_loop;
                    new_loop.label_exit = 0;
                    loops.push_front(new_loop);
                } else if (name == "NEXT") {
                    get_lex();
                    if (loops.size() == 0) {
                        emit_error("NEXT without FOR");
                    } else {
                        class node *final = loops.front().final;
                        class node *step = loops.front().step;
                        int label_loop = loops.front().label_loop;
                        int label_exit = loops.front().label_exit;
                        string loop = loops.front().var;
                        
                        if (loops.front().type != loop::NESTED_FOR) {
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
                                final->generate(0, label_loop);
                                delete final;
                                final = NULL;
                            }
                            if (step != NULL) {
                                delete step;
                                step = NULL;
                            }
                            if (label_exit != 0)
                                output->emit_l(TEMP_PREFIX, label_exit);
                            loops.pop_front();
                        }
                    }
                } else if (name == "WHILE") {  // WHILE loop
                    int label_loop;
                    int label_exit;
                    enum lexical_component type;
                    struct loop new_loop;
                    
                    get_lex();
                    label_loop = next_local++;
                    label_exit = next_local++;
                    output->emit_l(TEMP_PREFIX, label_loop);
                    type = eval_expr(0, label_exit);
                    if (!optimized) {
                        output->emit_r(N_TSTR, 0);
                        output->emit_a(N_BEQ, TEMP_PREFIX, label_exit);
                    }
                    new_loop.type = loop::NESTED_WHILE;
                    new_loop.step = NULL;
                    new_loop.final = NULL;
                    new_loop.var = "";
                    new_loop.label_loop = label_loop;
                    new_loop.label_exit = label_exit;
                    loops.push_front(new_loop);
                } else if (name == "WEND") {
                    get_lex();
                    if (loops.size() == 0) {
                        emit_error("WEND without WHILE");
                    } else if (loops.front().type != loop::NESTED_WHILE) {
                        emit_error("bad nested WEND");
                    } else {
                        output->emit_a(N_B, TEMP_PREFIX, loops.front().label_loop);
                        output->emit_l(TEMP_PREFIX, loops.front().label_exit);
                        loops.pop_front();
                    }
                } else if (name == "DO") {
                    int label_loop;
                    int label_exit;
                    enum lexical_component type;
                    struct loop new_loop;
                    
                    get_lex();
                    label_loop = next_local++;
                    label_exit = next_local++;
                    output->emit_l(TEMP_PREFIX, label_loop);
                    if (lex == C_NAME && name == "WHILE") {
                        get_lex();
                        type = eval_expr(0, label_exit);
                        if (!optimized) {
                            output->emit_r(N_TSTR, 0);
                            output->emit_a(N_BEQ, TEMP_PREFIX, label_exit);
                        }
                        new_loop.var = "1"; // Uses exit label
                        new_loop.type = loop::NESTED_DO;  // Condition at top
                    } else if (lex == C_NAME && name == "UNTIL") {
                        int label_temp = next_local++;
                        
                        get_lex();
                        type = eval_expr(0, label_temp);
                        if (!optimized) {
                            output->emit_r(N_TSTR, 0);
                            output->emit_a(N_BEQ, TEMP_PREFIX, label_temp);
                        }
                        // Let optimizer to solve this =P
                        output->emit_a(N_B, TEMP_PREFIX, label_exit);
                        output->emit_l(TEMP_PREFIX, label_temp);
                        new_loop.var = "1"; // Uses exit label
                        new_loop.type = loop::NESTED_DO;  // Condition at top
                    } else {
                        new_loop.var = ""; // Doesn't use exit label (yet)
                        new_loop.type = loop::NESTED_DO_LOOP;  // Condition at bottom
                    }
                    new_loop.step = NULL;
                    new_loop.final = NULL;
                    new_loop.label_loop = label_loop;
                    new_loop.label_exit = label_exit;
                    loops.push_front(new_loop);
                } else if (name == "LOOP") {
                    get_lex();
                    if (loops.size() == 0) {
                        emit_error("LOOP without DO");
                    } else if (loops.front().type == loop::NESTED_DO) {
                        output->emit_a(N_B, TEMP_PREFIX, loops.front().label_loop);
                        output->emit_l(TEMP_PREFIX, loops.front().label_exit);
                        loops.pop_front();
                    } else if (loops.front().type == loop::NESTED_DO_LOOP) {
                        enum lexical_component type;
                        
                        if (lex == C_NAME && name == "WHILE") {
                            int label_temp = next_local++;
                            
                            get_lex();
                            type = eval_expr(0, label_temp);
                            if (!optimized) {
                                output->emit_r(N_TSTR, 0);
                                output->emit_a(N_BEQ, TEMP_PREFIX, label_temp);
                            }
                            // Let optimizer to solve this =P
                            output->emit_a(N_B, TEMP_PREFIX, loops.front().label_loop);
                            output->emit_l(TEMP_PREFIX, label_temp);
                        } else if (lex == C_NAME && name == "UNTIL") {
                            get_lex();
                            type = eval_expr(0, loops.front().label_loop);
                            if (!optimized) {
                                output->emit_r(N_TSTR, 0);
                                output->emit_a(N_BEQ, TEMP_PREFIX, loops.front().label_loop);
                            }
                        } else {
                            emit_error("LOOP without condition");
                        }
                        if (loops.front().var == "1")  // Uses exit label?
                            output->emit_l(TEMP_PREFIX, loops.front().label_exit);
                        loops.pop_front();
                    } else {
                        emit_error("bad nested LOOP");
                    }
                } else if (name == "SELECT") {
                    int label_loop;
                    int label_exit;
                    enum lexical_component type;
                    struct loop new_loop;
                    
                    get_lex();
                    label_exit = next_local++;
                    if (lex == C_NAME && name == "CASE") {
                        get_lex();
                        type = eval_expr(0, 0);
                    } else {
                        emit_error("missing CASE after SELECT");
                    }
                    new_loop.type = loop::NESTED_SELECT;
                    new_loop.step = NULL;
                    new_loop.final = NULL;
                    new_loop.var = "";
                    new_loop.label_loop = 0;
                    new_loop.label_exit = label_exit;
                    loops.push_front(new_loop);
                } else if (name == "CASE") {
                    get_lex();
                    if (loops.size() == 0 || loops.front().type != loop::NESTED_SELECT) {
                        emit_error("CASE without SELECT CASE");
                    } else {
                        if (loops.front().label_loop != 0) {
                            output->emit_a(N_B, TEMP_PREFIX, loops.front().label_exit);
                            output->emit_l(TEMP_PREFIX, loops.front().label_loop);
                        }
                        if (lex == C_NAME && name == "ELSE") {
                            get_lex();
                            if (loops.front().label_loop == 0) {
                                emit_error("More than one CASE ELSE");
                            } else {
                                loops.front().label_loop = 0;
                            }
                        } else {
                            class node *tree;
                            int type;
                            int min;
                            int max;
                            
                            tree = eval_level0(&type);
                            if (tree->node_type() != C_NUM) {
                                emit_error("not a constant expression in CASE");
                                min = 0;
                            } else {
                                min = tree->node_value();
                            }
                            delete tree;
                            tree = NULL;
                            if (lex == C_NAME && name == "TO") {
                                get_lex();
                                tree = eval_level0(&type);
                                if (tree->node_type() != C_NUM) {
                                    emit_error("not a constant expression in CASE TO");
                                    max = min;
                                } else {
                                    max = tree->node_value();
                                }
                                delete tree;
                                tree = NULL;
                            } else {
                                max = min;
                            }
                            if (min > max) {
                                emit_error("Maximum range of CASE is lesser than minimum");
                            }
                            loops.front().label_loop = next_local++;
                            if (min == max) {
                                output->emit_nr(N_CMPI, "", min, 0);
                                output->emit_a(N_BNE, TEMP_PREFIX, loops.front().label_loop);
                            } else {
                                output->emit_nr(N_CMPI, "", min, 0);
                                output->emit_a(N_BNC, TEMP_PREFIX, loops.front().label_loop);
                                output->emit_nr(N_CMPI, "", max + 1, 0);
                                output->emit_a(N_BC, TEMP_PREFIX, loops.front().label_loop);
                            }
                        }
                    }
                } else if (name == "EXIT") {
                    get_lex();
                    
                    // Avoid IF blocks
                    loop_explorer = loops.begin();
                    while (loop_explorer != loops.end()) {
                        if (loop_explorer->type != loop::NESTED_IF)
                            break;
                        ++loop_explorer;
                    }
                    if (loops.size() == 0 || loop_explorer == loops.end()) {
                        emit_error("nowhere to EXIT");
                    } else {
                        if (lex != C_NAME) {
                            emit_error("missing type of EXIT, WHILE/FOR/DO");
                        } else if (name == "FOR") {
                            get_lex();
                            if (loops.size() == 0 || loop_explorer->type != loop::NESTED_FOR) {
                                emit_error("EXIT FOR without FOR");
                            } else {
                                if (loop_explorer->label_exit == 0)
                                    loop_explorer->label_exit = next_local++;
                                output->emit_a(N_B, TEMP_PREFIX, loop_explorer->label_exit);
                            }
                        } else if (name == "WHILE") {
                            get_lex();
                            if (loops.size() == 0 || loop_explorer->type != loop::NESTED_WHILE) {
                                emit_error("EXIT WHILE without WHILE");
                            } else {
                                output->emit_a(N_B, TEMP_PREFIX, loop_explorer->label_exit);
                            }
                        } else if (name == "DO") {
                            get_lex();
                            if (loops.size() == 0 || (loop_explorer->type != loop::NESTED_DO && loop_explorer->type != loop::NESTED_DO_LOOP)) {
                                emit_error("EXIT DO without DO");
                            } else {
                                loop_explorer->var = "1";
                                output->emit_a(N_B, TEMP_PREFIX, loop_explorer->label_exit);
                            }
                        } else if (name == "SELECT") {
                            get_lex();
                            if (loops.size() == 0 || loop_explorer->type != loop::NESTED_SELECT) {
                                emit_error("EXIT SELECT without SELECT");
                            } else {
                                output->emit_a(N_B, TEMP_PREFIX, loop_explorer->label_exit);
                            }
                        } else {
                            emit_error("only supported EXIT WHILE/FOR/DO/SELECT");
                            get_lex();
                        }
                    }
                } else if (name == "POKE") {  // POKE
                    class node *tree;
                    int type;
                    
                    get_lex();
                    tree = eval_level0(&type);
                    if (lex != C_COMMA)
                        emit_error("missing comma in POKE");
                    else
                        get_lex();
                    tree = new node(C_ASSIGN, 1, eval_level0(&type), tree);
                    tree->label();
                    optimized = false;
                    tree->generate(0, 0);
                    delete tree;
                    tree = NULL;
                } else if (name == "REM") {  // REM (comment)
                    line_pos = line_size;
                    get_lex();
                } else if (name == "CLS") {  // CLS
                    get_lex();
                    output->emit_a(N_CALL, "CLRSCR", -1);
                } else if (name == "WAIT") {  // WAIT
                    get_lex();
                    output->emit_a(N_CALL, "_wait", -1);
                } else if (name == "RESTORE") {
                    get_lex();
                    if (lex != C_NAME) {
                        emit_error("bad syntax for RESTORE");
                    } else {
                        if (labels[name] == 0) {
                            labels[name] = next_label;
                            name_mangling[next_label] = NAME_MANGLING_LABEL + name;
                            next_label++;
                        }
                        label_used[name] |= 1;
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
                    if (lex == C_NAME && name == "PACKED") {
                        int c;
                        int v;
                        int v1;
                        int v2;
                        
                        get_lex();
                        v = 0;
                        while (1) {
                            if (lex == C_STRING) {  // String found
                                for (c = 0; c < (int) name.length(); c++) {
                                    if (name[c] == 127 && c + 1 < (int) name.length()) {
                                        c++;
                                        if (v == 0)
                                            v1 = (name[c] & 0xff) + 127;
                                        else
                                            v2 = (name[c] & 0xff) + 127;
                                    } else {
                                        if (v == 0)
                                            v1 = (name[c] & 0xff);
                                        else
                                            v2 = (name[c] & 0xff);
                                    }
                                    v = !v;
                                    if (v == 0)
                                        output->emit_d(N_DECLE, (v1 << 8) | v2);
                                }
                                get_lex();
                            } else {
                                class node *tree;
                                int type;
                                
                                tree = eval_level0(&type);
                                if (tree->node_type() != C_NUM) {
                                    emit_error("not a constant expression in DATA");
                                    break;
                                }
                                if (v == 0) {
                                    v1 = tree->node_value();
                                    if (v1 < -128 || v1 > 255)
                                        emit_error("integer out of 8-bits range in DATA PACKED");
                                } else {
                                    v2 = tree->node_value();
                                    if (v2 < -128 || v2 > 255)
                                        emit_error("integer out of 8-bits range in DATA PACKED");
                                }
                                delete tree;
                                tree = NULL;
                                v = !v;
                                if (v == 0)
                                    output->emit_d(N_DECLE, (v1 << 8) | v2);
                            }
                            if (lex != C_COMMA)
                                break;
                            get_lex();
                        }
                        if (v != 0) {
                            v2 = 0;
                            output->emit_d(N_DECLE, (v1 << 8) | v2);
                        }
                    } else {
                        while (1) {
                            if (lex == C_NAME && name == "VARPTR") {  // Access to variable/array/label address
                                int temp;
                                int index;
                                int type2;
                                
                                get_lex();
                                if (lex != C_NAME) {
                                    emit_error("missing variable name for VARPTR");
                                } else {
                                    if (sneak_peek() == '(') {  // Indexed access
                                        class node *tree;
                                        
                                        if (arrays[name] != 0) {
                                            temp = arrays[name] >> 16;
                                        } else if (labels[name] != 0) {
                                            temp = labels[name];
                                            label_used[name] |= 1;
                                        } else {
                                            labels[name] = temp = next_label++;
                                            label_used[name] |= 1;
                                            name_mangling[temp] = NAME_MANGLING_LABEL + name;
                                        }
                                        get_lex();
                                        if (lex != C_LPAREN)
                                            emit_error("missing left parenthesis in array access");
                                        else
                                            get_lex();
                                        tree = eval_level0(&type2);
                                        if (tree->node_type() != C_NUM) {
                                            index = 0;
                                            emit_error("not a constant expression in array access");
                                        } else {
                                            index = tree->node_value();
                                        }
                                        if (lex != C_RPAREN)
                                            emit_error("missing right parenthesis in array access");
                                        else
                                            get_lex();
                                        output->emit_dlo(N_DECLE, LABEL_PREFIX, temp, index);
                                        delete tree;
                                        tree = NULL;
                                    } else {
                                        if ((constants[name] & 0x10000) != 0) {
                                            emit_error("constants doesn't have address for VARPTR");
                                            get_lex();
                                        } else {
                                            if (variables[name] == 0) {
                                                check_for_explicit(name);
                                                variables[name] = next_var;
                                                name_mangling_var[next_var] = NAME_MANGLING_VAR + name;
                                                next_var++;
                                           }
                                            temp = variables[name];
                                            get_lex();
                                            output->emit_dl(N_DECLE, VAR_PREFIX, temp);
                                        }
                                    }
                                }
                            } else if (lex == C_STRING) {
                                int c;
                                int v;
                                
                                for (c = 0; c < (int) name.length(); c++) {
                                    if (name[c] == 127 && c + 1 < (int) name.length()) {
                                        c++;
                                        v = (name[c] & 0xff) + 127;
                                    } else {
                                        v = (name[c] & 0xff);
                                    }
                                    output->emit_d(N_DECLE, v);
                                }
                                get_lex();
                            } else {
                                class node *tree;
                                int type;
                                
                                tree = eval_level0(&type);
                                if (tree->node_type() != C_NUM) {
                                    emit_error("not a constant expression in DATA");
                                    break;
                                }
                                output->emit_d(N_DECLE, tree->node_value());
                                delete tree;
                                tree = NULL;
                            }
                            if (lex != C_COMMA)
                                break;
                            get_lex();
                        }
                    }
                } else if (name == "DEFINE") {
                    int label;
                    class node *tree;
                    int type;
                    
                    get_lex();
                    if (lex == C_NAME && name == "ALTERNATE") {
                        get_lex();
                        eval_expr(0, 0);
                        output->emit_rl(N_MVO, 0, "_gram2_target", -1);
                        if (lex != C_COMMA)
                            emit_error("missing comma for DEFINE");
                        else
                            get_lex();
                        
                        tree = eval_level0(&type);
                        tree->label();
                        if (tree->node_type() == C_NUM && tree->node_value() > 16)
                            emit_warning("More than 16 GRAM defined per video frame");
                        optimized = false;
                        tree->generate(0, 0);
                        delete tree;
                        tree = NULL;
                        
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
                                    name_mangling[label] = NAME_MANGLING_LABEL + name;
                                    next_label++;
                                    label_used[name] |= 1;
                                } else {
                                    label = labels[name];
                                    label_used[name] |= 1;
                                }
                                output->emit_nr(N_MVII, LABEL_PREFIX, label, 0);
                                get_lex();
                            }
                            output->emit_rl(N_MVO, 0, "_gram2_bitmap", -1);
                        }
                    } else {
                        eval_expr(0, 0);
                        output->emit_rl(N_MVO, 0, "_gram_target", -1);
                        if (lex != C_COMMA)
                            emit_error("missing comma for DEFINE");
                        else
                            get_lex();
                        
                        tree = eval_level0(&type);
                        tree->label();
                        if (tree->node_type() == C_NUM && tree->node_value() > 16)
                            emit_warning("More than 16 GRAM defined per video frame");
                        optimized = false;
                        tree->generate(0, 0);
                        delete tree;
                        tree = NULL;
                        
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
                                    name_mangling[label] = NAME_MANGLING_LABEL + name;
                                    next_label++;
                                    label_used[name] |= 1;
                                } else {
                                    label = labels[name];
                                    label_used[name] |= 1;
                                }
                                output->emit_nr(N_MVII, LABEL_PREFIX, label, 0);
                                get_lex();
                            }
                            output->emit_rl(N_MVO, 0, "_gram_bitmap", -1);
                        }
                    }
                } else if (name == "SOUND") {
                    class node *tree;
                    int channel;
                    int type;
                    
                    get_lex();
                    tree = eval_level0(&type);
                    if (tree->node_type() != C_NUM) {
                        emit_error("only constant expression for first SOUND parameter");
                        channel = 0;
                    } else {
                        channel = tree->node_value();
                        if (channel < 0 || channel > 9)
                            emit_error("bad channel for SOUND");
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
                    class node *tree;
                    int sprite;
                    int add;
                    int type;
                    
                    get_lex();
                    tree = eval_level0(&type);
                    if (lex != C_COMMA)
                        emit_error("bad syntax for SPRITE");
                    else
                        get_lex();
                    if (tree->node_type() == C_NUM) {
                        sprite = tree->node_value();
                        delete tree;
                        tree = NULL;
                        if (sprite < 0 || sprite > 7)
                            emit_error("bad number (0-7) for SPRITE");
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
                    } else {
                        tree = new node(C_PLUS, 0, new node(C_NAME_RO, arrays["#MOBSHADOW"] >> 16, 0, 0), tree);
                        tree->label();
                        tree->generate(0, 0);
                        if (lex != C_COMMA) {
                            output->push(0);
                            eval_expr(0, 0);
                            output->pop();
                            output->emit_rr(N_MVOA, 0, 4);
                            sprite = 4;
                            add = 7;
                        } else {
                            sprite = 0;
                            add = 8;
                        }
                        if (lex == C_COMMA) {
                            get_lex();
                            if (lex != C_COMMA) {
                                output->push(sprite);
                                eval_expr(0, 0);
                                output->pop();
                                output->emit_nr(N_ADDI, "", add, 4);
                                output->emit_rr(N_MVOA, 0, 4);
                                sprite = 4;
                                add = 7;
                            } else {
                                add += 8;
                            }
                            if (lex == C_COMMA) {
                                get_lex();
                                output->push(sprite);
                                eval_expr(0, 0);
                                output->pop();
                                output->emit_nr(N_ADDI, "", add, 4);
                                output->emit_rr(N_MVOA, 0, 4);
                            }
                        }
                    }
                } else if (name == "PRINT") {
                    int start;
                    int type;
                    
                    get_lex();
                    start = 1;
                    if (lex == C_NAME && name == "AT") {
                        class node *final;
                        
                        get_lex();
                        final = eval_level0(&type);
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
                            size_t c;
                            int p;
                            int v;
                            
                            output->emit_lr(N_MVI, "_screen", -1, 4);
                            p = -1;
                            for (c = 0; c < (int) name.length(); c++) {
                                if (name[c] == 127 && c + 1 < (int) name.length()) {
                                    c++;
                                    v = ((name[c] & 0xff) + 127) * 8;
                                } else {
                                    v = (name[c] & 0xff) * 8;
                                }
                                if (v != p) {
                                    if (p != -1) {
                                        if ((p ^ v) == 0)
                                            output->emit(N_NOP);
                                        else
                                            output->emit_nr(N_XORI, "", (p ^ v), 0);
                                    } else {
                                        if (v == 0) {
                                            output->emit_lr(N_MVI, "_color", -1, 0);
                                        } else {
                                            output->emit_nr(N_MVII, "", v, 0);
                                            output->emit_lr(N_XOR, "_color", -1, 0);
                                        }
                                    }
                                    p = v;
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
                            numbers_used = true;
                        } else {
                            eval_expr(0, 0);
                            output->emit_lr(N_MVI, "_screen", -1, 4);
                            output->emit_rr(N_MVOA, 0, 4);
                        }
                        output->emit_rl(N_MVO, 4, "_screen", -1);
                    }
                } else if (name == "BITMAP") {
                    get_lex();
                    if (lex == C_NAME) {
                        if (name == "NORMAL") {
                            bitmap_state = 0;
                            get_lex();
                        } else if (name == "INVERSE") {
                            bitmap_state |= 1;
                            get_lex();
                        } else if (name == "MIRROR_X") {
                            bitmap_state |= 2;
                            get_lex();
                        }
                    } else if (lex != C_STRING || name.length() != 8) {
                        emit_error("syntax error in BITMAP");
                    } else {
                        int c;
                        
                        if (bitmap_state & 2) {     // Mirror in X
                            value = 0;
                            for (c = 0; c < 8; c++) {
                                if (name[c] != 0x10 && name[c] != 0x3f   // 0 and _
                                    && name[c] != 0x00 && name[c] != 0x0e)  // space and .
                                    value |= 0x01 << c;
                            }
                        } else {    // Normal
                            value = 0;
                            for (c = 0; c < 8; c++) {
                                if (name[c] != 0x10 && name[c] != 0x3f   // 0 and _
                                    && name[c] != 0x00 && name[c] != 0x0e)  // space and .
                                    value |= 0x80 >> c;
                            }
                        }
                        if (bitmap_state & 1)   // Inverse
                            value ^= 0xff;
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
                    scroll_used = true;
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
                } else if (name == "SIGNED") {
                    get_lex();
                    while (1) {
                        if (lex != C_NAME) {
                            emit_error("missing name in SIGNED");
                            break;
                        }
                        signedness[name] = 1;
                        get_lex();
                        if (lex != C_COMMA)
                            break;
                        get_lex();
                    }
                } else if (name == "UNSIGNED") {
                    get_lex();
                    while (1) {
                        if (lex != C_NAME) {
                            emit_error("missing name in UNSIGNED");
                            break;
                        }
                        signedness[name] = 2;
                        get_lex();
                        if (lex != C_COMMA)
                            break;
                        get_lex();
                    }
                } else if (name == "CONST") {
                    int c;
                    
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
                        int type;
                        
                        get_lex();
                        tree = eval_level0(&type);
                        if (tree->node_type() != C_NUM) {
                            emit_error("not a constant expression in CONST");
                        } else {
                            c = 0x10000;
                            if (type != 0)
                                c |= 0x20000;
                            constants[assigned] = (tree->node_value() & 0xffff) | c;
                        }
                        asm_output << NAME_MANGLING_CONST;
                        mangle(assigned);
                        asm_output << ":\tEQU " << (tree->node_value() & 0xffff) << "\n";
                        delete tree;
                        tree = NULL;
                    }
                } else if (name == "DIM") {
                    string array;
                    class node *tree = NULL;
                    int c;
                    int where;
                    int type;
                    
                    while (1) {
                        get_lex();
                        if (lex != C_NAME) {
                            emit_error("missing name in DIM");
                            break;
                        }
                        array = name;
                        get_lex();
                        if (lex == C_LPAREN) {
                            get_lex();
                            tree = eval_level0(&type);
                            if (tree->node_type() != C_NUM) {
                                emit_error("not a constant expression in DIM");
                                break;
                            }
                            c = tree->node_value();
                            if (c <= 0 || c >= 65535) {
                                emit_error("invalid dimension in DIM");
                                c = 1;
                            }
                            delete tree;
                            tree = NULL;
                            if (lex != C_RPAREN) {
                                emit_error("missing right parenthesis in DIM");
                            } else {
                                get_lex();
                            }
                            where = -1;
                            
                            // Maps an array directly to a memory address.
                            // Nothing that cannot be done with assembler.
                            //
                            // Not included in the docs because pros think there is no need for it.
                            // And newbies could ask too complicated questions about it. Not enough time ;)
                            if (lex == C_NAME && name == "AT") {
                                get_lex();
                                tree = eval_level0(&type);
                                if (tree->node_type() != C_NUM) {
                                    emit_error("not a constant expression in DIM AT");
                                    where = 0;
                                } else {
                                    where = tree->node_value();
                                    if (where <= 0 || where > 65535) {
                                        emit_error("invalid address in DIM AT");
                                        where = 0;
                                    }
                                }
                                delete tree;
                                tree = NULL;
                            }
                            
                            if (arrays[array] != 0) {
                                emit_error("already used name for DIM");
                            } else {
                                name_mangling[next_label] = NAME_MANGLING_ARRAY + array;
                                arrays[array] = c | (next_label++ << 16);
                                if (where >= 0) {
                                    mangle_label(LABEL_PREFIX, arrays[array] >> 16);
                                    asm_output << ":\tEQU " << where << "\t; " << array << "\n";
                                    arrays[array] = (arrays[array] - c) + 65535;  // Make length 65535
                                }
                            }
                        } else {
                            if (variables[array] != 0) {
                                string message;
                                
                                message = "variable '" + array + "' already defined";
                                emit_error(message);
                            }
                            variables[array] = next_var;
                            name_mangling_var[next_var] = NAME_MANGLING_VAR + array;
                            next_var++;
                        }
                        if (lex != C_COMMA)
                            break;
                    }
                } else if (name == "MODE") {    // Video mode selection
                    class node *tree = NULL;
                    class node *tree2 = NULL;
                    int mode;
                    int type;
                    
                    get_lex();
                    tree = eval_level0(&type);
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
                        tree = eval_level0(&type);
                        if (lex != C_COMMA)
                            emit_error("missing comma in MODE");
                        else
                            get_lex();
                        tree2 = eval_level0(&type);
                        tree2 = new node(C_MUL, 0, tree2, new node(C_NUM, 0x100, NULL, NULL));
                        tree = new node(C_PLUS, 0, tree, tree2);
                        if (lex != C_COMMA)
                            emit_error("missing comma in MODE");
                        else
                            get_lex();
                        tree2 = eval_level0(&type);
                        tree2 = new node(C_MUL, 0, tree2, new node(C_NUM, 0x1000, NULL, NULL));
                        tree = new node(C_PLUS, 0, tree, tree2);
                        if (lex != C_COMMA)
                            emit_error("missing comma in MODE");
                        else
                            get_lex();
                        tree2 = eval_level0(&type);
                        tree2 = new node(C_MUL, 0, tree2, new node(C_NUM, 0x10, NULL, NULL));
                        tree = new node(C_PLUS, 0, tree, tree2);
                        tree->label();
                        tree->generate(0, 0);
                        delete tree;
                        tree = NULL;
                        output->emit_rl(N_MVO, 0, "_color", -1);
                        output->emit_nr(N_MVII, "", 2, 0);
                        output->emit_rl(N_MVO, 0, "_mode_select", -1);
                    } else {    // Foreground/Background mode
                        output->emit_nr(N_MVII, "", 3, 0);
                        output->emit_rl(N_MVO, 0, "_mode_select", -1);
                    }
                } else if (name == "SCREEN") {  // Copy screen
                    int label;
                    
                    get_lex();
                    if (lex != C_NAME) {
                        emit_error("bad syntax for SCREEN");
                        break;
                    }
                    if (name == "ENABLE") {
                        get_lex();
                        output->emit_lr(N_MVI, "_mode_select", -1, 0);
                        output->emit_nr(N_ANDI, "", 3, 0);
                        output->emit_rl(N_MVO, 0, "_mode_select", -1);
                    } else if (name == "DISABLE") {
                        get_lex();
                        output->emit_lr(N_MVI, "_mode_select", -1, 0);
                        output->emit_nr(N_ANDI, "", 3, 0);
                        output->emit_nr(N_ADDI, "", 4, 0);
                        output->emit_rl(N_MVO, 0, "_mode_select", -1);
                    } else {
                        assigned = name;
                        if (arrays[name] != 0) {
                            label = arrays[name] >> 16;
                        } else if (labels[name] == 0) {
                            label = labels[name] = next_label;
                            name_mangling[label] = NAME_MANGLING_LABEL + name;
                            next_label++;
                            label_used[name] |= 1;
                        } else {
                            label = labels[name];
                            label_used[name] |= 1;
                        }
                        get_lex();
                        if (lex == C_COMMA) {  // There is a second argument?
                            class node *final;
                            int type;
                            
                            get_lex();
                            final = eval_level0(&type);  // Evaluate second argument (origin position)
                            final = new node(C_PLUS, 0, final, new node(C_NAME_RO, label, NULL, NULL));
                            final->label();
                            final->generate(0, 0);
                            delete final;
                            output->emit_r(N_PSHR, 0);
                            if (lex != C_COMMA) {
                                emit_error("missing comma after second parameter in SCREEN");
                                break;
                            }
                            get_lex();
                            final = eval_level0(&type);  // Evaluate third argument (target position)
                            final = new node(C_PLUS, 0, final, new node(C_NUM, 0x200, NULL, NULL));
                            final->label();
                            final->generate(0, 0);
                            delete final;
                            final = NULL;
                            output->emit_r(N_PSHR, 0);
                            if (lex != C_COMMA) {
                                emit_error("missing comma after third parameter in SCREEN");
                                break;
                            }
                            get_lex();
                            eval_expr(0, 0);    // Evaluate fourth argument (block width)
                            output->emit_r(N_PSHR, 0);
                            if (lex != C_COMMA) {
                                emit_error("missing comma after fourth parameter in SCREEN");
                                break;
                            }
                            get_lex();
                            eval_expr(0, 0);    // Evaluate fifth argument (block height)
                            if (lex == C_COMMA) {   // Sixth argument for SCREEN
                                output->emit_r(N_PSHR, 0);
                                get_lex();
                                eval_expr(0, 0);    // Evaluate sixth argument (origin width)
                                output->emit_a(N_CALL, "CPYBLK2", -1);
                            } else {
                                output->emit_r(N_PULR, 1);
                                output->emit_r(N_PULR, 2);
                                output->emit_r(N_PULR, 3);
                                output->emit_a(N_CALL, "CPYBLK", -1);
                            }
                        } else {
                            output->emit_nr(N_MVII, LABEL_PREFIX, label, 3);
                            output->emit_nr(N_MVII, "", 0x200, 2);
                            output->emit_nr(N_MVII, "", 20, 1);
                            output->emit_nr(N_MVII, "", 12, 0);
                            output->emit_a(N_CALL, "CPYBLK", -1);
                        }
                    }
                } else if (name == "PLAY") {
                    int label;
                    int c;
                    
                    get_lex();
                    if (lex != C_NAME) {
                        emit_error("bad syntax for PLAY");
                        break;
                    }
                    music_used = true;
                    if (name == "OFF") {
                        get_lex();
                        output->emit_r(N_CLRR, 0);
                        output->emit_a(N_CALL, "_play_music", -1);
                    } else if (name == "NONE") {
                        get_lex();
                        output->emit_r(N_CLRR, 3);
                        output->emit_rl(N_MVO, 3, "_music_mode", -1);
                    } else if (name == "SIMPLE") {
                        get_lex();
                        c = 3;
                        if (lex == C_NAME && name == "NO") {
                            get_lex();
                            if (lex == C_NAME && name == "DRUMS") {
                                get_lex();
                                c--;
                            } else {
                                emit_error("only allowed PLAY SIMPLE NO DRUMS");
                            }
                        }
                        output->emit_nr(N_MVII, "", c, 3);
                        output->emit_rl(N_MVO, 3, "_music_mode", -1);
                    } else if (name == "FULL") {
                        get_lex();
                        c = 5;
                        if (lex == C_NAME && name == "NO") {
                            get_lex();
                            if (lex == C_NAME && name == "DRUMS") {
                                get_lex();
                                c--;
                            } else {
                                emit_error("only allowed PLAY FULL NO DRUMS");
                            }
                        }
                        output->emit_nr(N_MVII, "", c, 3);
                        output->emit_rl(N_MVO, 3, "_music_mode", -1);
                    } else if (name == "VOLUME") {
                        get_lex();
                        eval_expr(0, 0);
                        output->emit_rl(N_MVO, 0, "_music_vol", -1);
                        playvol_used = true;
                    } else {
                        if (arrays[name] != 0) {
                            label = arrays[name] >> 16;
                        } else if (labels[name] == 0) {
                            label = labels[name] = next_label;
                            name_mangling[label] = NAME_MANGLING_LABEL + name;
                            next_label++;
                            label_used[name] |= 1;
                        } else {
                            label = labels[name];
                            label_used[name] |= 1;
                        }
                        output->emit_nr(N_MVII, LABEL_PREFIX, label, 0);
                        output->emit_a(N_CALL, "_play_music", -1);
                        get_lex();
                    }
                } else if (name == "MUSIC") {
                    int arg;
                    static int previous[8];
                    unsigned int notes;
                    int note;
                    int c;
                    int label;
                    
                    get_lex();
                    label = 0;
                    arg = 0;
                    while (1) {
                        if (arg == 0 || arg == 4)
                            notes = 0;
                        if (lex != C_NAME && lex != C_MINUS) {
                            emit_error("bad syntax for MUSIC");
                            break;
                        }
                        if (lex == C_MINUS) {
                            // Nothing to do
                        } else if (arg == 0 && name == "REPEAT") {
                            get_lex();
                            notes = 0xfd000000;
                            break;
                        } else if (arg == 0 && name == "STOP") {
                            get_lex();
                            notes = 0xfe000000;
                            break;
                        } else if (arg == 0 && name == "VOLUME") {
                            class node *tree;
                            int type;
                            int v;
                            
                            get_lex();
                            notes = 0xfa000000;
                            tree = eval_level0(&type);
                            if (tree->node_type() != C_NUM) {
                                emit_error("not a constant expression in MUSIC VOLUME");
                                v = 4;
                            } else {
                                v = tree->node_value();
                                if (v < 0 || v > 15) {
                                    emit_error("MUSIC VOLUME out of range (0-15)");
                                }
                            }
                            notes |= v;
                            delete tree;
                            tree = NULL;
                            playvol_used = true;
                            break;
                        } else if (arg == 0 && name == "SPEED") {
                            class node *tree;
                            int type;
                            int v;
                            
                            get_lex();
                            notes = 0xff000000;
                            tree = eval_level0(&type);
                            if (tree->node_type() != C_NUM) {
                                emit_error("not a constant expression in MUSIC SPEED");
                                v = 4;
                            } else {
                                v = tree->node_value();
                                if (v == 0) {
                                    emit_error("MUSIC SPEED is zero");
                                }
                            }
                            notes |= v;
                            delete tree;
                            tree = NULL;
                            break;
                        } else if (arg == 0 && name == "RETURN") {
                            get_lex();
                            notes = 0xfb000000;
                            break;
                        } else if (arg == 0 && (name == "JUMP" || name == "GOSUB")) {
                            if (name == "JUMP")
                                notes = 0xfe000000;
                            else
                                notes = 0xfc000000;
                            get_lex();
                            if (lex != C_NAME) {
                                emit_error("missing label for MUSIC JUMP");
                                break;
                            }
                            if (arrays[name] != 0) {
                                label = arrays[name] >> 16;
                            } else if (labels[name] == 0) {
                                label = labels[name] = next_label;
                                name_mangling[label] = NAME_MANGLING_LABEL + name;
                                next_label++;
                                label_used[name] |= 1;
                            } else {
                                label = labels[name];
                                label_used[name] |= 1;
                            }
                            get_lex();
                            break;
                        } else if (arg == 3 || arg == 7) {
                            if (name[0] != 'M' || name[1] < '1' || name[1] > '3') {
                                emit_error("bad syntax for drum in MUSIC");
                                break;
                            }
                            notes |= (name[1] - '0') << ((arg & 3) * 8);
                        } else if (name == "S") {
                            notes |= 0x3f << ((arg & 3) * 8);
                        } else {
                            notes |= previous[arg] << ((arg & 3) * 8);
                            c = 0;
                            switch (name[c++]) {
                                case 'C': note = 0; break;
                                case 'D': note = 2; break;
                                case 'E': note = 4; break;
                                case 'F': note = 5; break;
                                case 'G': note = 7; break;
                                case 'A': note = 9; break;
                                case 'B': note = 11; break;
                                default:
                                    note = 0;
                                    emit_error("bad syntax for note in MUSIC");
                                    break;
                            }
                            switch (name[c++]) {
                                case '2': note += 0 * 12; break;
                                case '3': note += 1 * 12; break;
                                case '4': note += 2 * 12; break;
                                case '5': note += 3 * 12; break;
                                case '6': note += 4 * 12; break;
                                case '7': if (note == 0) { note += 5 * 12; break; }
                                default:
                                    emit_error("bad syntax for note in MUSIC");
                                    break;
                            }
                            note++;
                            if (name[c] == '#') {
                                note++;
                                c++;
                            }
                            if (name[c] == 'W') {
                                previous[arg] = 0x00;
                                notes &= ~(0xc0 << ((arg & 3) * 8));
                                notes |= previous[arg] << ((arg & 3) * 8);
                            } else if (name[c] == 'X') {
                                previous[arg] = 0x40;
                                notes &= ~(0xc0 << ((arg & 3) * 8));
                                notes |= previous[arg] << ((arg & 3) * 8);
                            } else if (name[c] == 'Y') {
                                previous[arg] = 0x80;
                                notes &= ~(0xc0 << ((arg & 3) * 8));
                                notes |= previous[arg] << ((arg & 3) * 8);
                            } else if (name[c] == 'Z') {
                                previous[arg] = 0xc0;
                                notes &= ~(0xc0 << ((arg & 3) * 8));
                                notes |= previous[arg] << ((arg & 3) * 8);
                            }
                            notes |= note << ((arg & 3) * 8);
                        }
                        get_lex();
                        arg++;
                        if (lex != C_COMMA)
                            break;
                        if (arg == 8) {
                            emit_error("too many arguments for MUSIC");
                            break;
                        }
                        if (arg == 4) {
                            output->emit_d2(N_DECLE, (notes & 0xffff), (notes >> 16 & 0xffff));
                            music_ecs_used = true;
                            ecs_used = true;
                        }
                        get_lex();
                    }
                    if (label) {
                        output->emit_dl(N_DECLE, LABEL_PREFIX, label);
                        output->emit_d(N_DECLE, (notes >> 16 & 0xffff));
                    } else {
                        output->emit_d2(N_DECLE, (notes & 0xffff), (notes >> 16 & 0xffff));
                    }
                } else if (name == "ON") {
                    int label;
                    int table;
                    int c;
                    int max_value;
                    int gosub;
                    int fast;
                    int options[256];
                    const int ON_REG = 1;   // 0 is less optimized
                    
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
                            name_mangling[next_label] = NAME_MANGLING_LABEL + name;
                            next_label++;
                        }
                        label_used[name] |= 1;
                        frame_drive = labels[name];
                        get_lex();
                    } else {
                        eval_expr(ON_REG, 0);
                        fast = 0;
                        if (lex == C_NAME && name == "FAST") {
                            get_lex();
                            fast = 1;
                        }
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
                                    name_mangling[next_label] = NAME_MANGLING_LABEL + name;
                                    next_label++;
                                }
                                label_used[name] |= 1;
                                if (gosub != 0)
                                    label_used[name] |= CALLED_BY_GOSUB;
                                else {
                                    label_used[name] |= CALLED_BY_GOTO;
                                    proc_gotos[inside_proc].push_back(labels[name]);
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
                        if (fast == 0) {
                            output->emit_nr(N_CMPI, "", max_value, ON_REG);
                            output->emit_a(N_BC, TEMP_PREFIX, label);
                        }
                        if (gosub)
                            output->emit_nr(N_MVII, TEMP_PREFIX, label, 5);
                        output->emit_nr(N_ADDI, TEMP_PREFIX, table, ON_REG);
                        if (ON_REG == 0) {
                            output->emit_rr(N_MOVR, ON_REG, 1);
                            output->emit_rr(N_MVIA, 1, 7);
                        } else {
                            output->emit_rr(N_MVIA, ON_REG, 7);
                        }
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
                        voice_used = true;
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
                                name_mangling[label] = NAME_MANGLING_LABEL + name;
                                next_label++;
                                label_used[name] |= 1;
                            } else {
                                label = labels[name];
                                label_used[name] |= 1;
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
                                name_mangling[label] = NAME_MANGLING_LABEL + name;
                                next_label++;
                                label_used[name] |= 1;
                            } else {
                                label = labels[name];
                                label_used[name] |= 1;
                            }
                            output->emit_nr(N_MVII, LABEL_PREFIX, label, 0);
                            output->emit_a(N_CALL, "IV_PLAY.1", -1);
                            get_lex();
                        }
                    } else if (lex == C_NAME && name == "INIT") {
                        voice_used = true;
                        get_lex();
                        output->emit_a(N_CALL, "IV_HUSH", -1);
                    } else if (lex == C_NAME && name == "WAIT") {
                        voice_used = true;
                        get_lex();
                        output->emit_a(N_CALL, "IV_WAIT", -1);
                    } else if (lex == C_NAME && name == "NUMBER") {
                        voice_used = true;
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
                                } else if (name == "FORTY") {
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
                } else if (name == "FLASH") {
                    get_lex();
                    if (!jlp_used)
                        emit_error("Using FLASH statement without using --jlp option");
                    if (lex != C_NAME) {
                        emit_error("Syntax error in FLASH statement");
                    } else if (name == "INIT") {
                        get_lex();
                        if (lex == C_NAME && name == "SIZE") {
                            class node *tree;
                            int type2;

                            get_lex();
                            tree = eval_level0(&type2);
                            if (tree->node_type() != C_NUM) {
                                emit_error("not a constant expression in FLASH INIT SIZE");
                            } else {
                                jlp_flash_size = tree->node_value();
                            }
                        }
                        output->emit_a(N_CALL, "JF.INIT", -1);
                    } else if (name == "ERASE") {
                        get_lex();
                        eval_expr(0, 0);
                        output->emit_a(N_CALL, "JF.CMD", -1);
                        output->emit_dl(N_DECLE, "JF.erase", -1);
                    } else if (name == "READ") {
                        get_lex();
                        eval_expr(0, 0);
                        if (lex != C_COMMA)
                            emit_error("Missing comma in FLASH statement");
                        else
                            get_lex();
                        if (lex == C_NAME && name == "VARPTR") {
                            eval_expr(1, 0);
                        } else {
                            emit_error("Only VARPTR can be used in FLASH READ");
                        }
                        output->emit_a(N_CALL, "JF.CMD", -1);
                        output->emit_dl(N_DECLE, "JF.read", -1);
                    } else if (name == "WRITE") {
                        get_lex();
                        eval_expr(0, 0);
                        if (lex != C_COMMA)
                            emit_error("Missing comma in FLASH statement");
                        else
                            get_lex();
                        if (lex == C_NAME && name == "VARPTR") {
                            eval_expr(1, 0);
                        } else {
                            emit_error("Only VARPTR can be used in FLASH WRITE");
                        }
                        output->emit_a(N_CALL, "JF.CMD", -1);
                        output->emit_dl(N_DECLE, "JF.write", -1);
                    }
                    flash_used = true;
                } else if (name == "STACK_CHECK") { // Stack overflow check
                    get_lex();
                    stack_used = true;
                } else if (name == "CALL") {        // Call assembly language
                    class node *tree;
                    
                    get_lex();
                    tree = process_usr(1);
                    tree->label();
                    tree->generate(0, 0);
                    delete tree;
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
                            macros[function]->in_use = false;
                            if (lex != C_EQUAL) {
                                emit_error("missing = in DEF FN");
                            } else {
                                get_lex();
                                while (lex != C_END) {
                                    if (lex == C_ERR) {
                                        emit_error("bad syntax inside DEF FN replacement text");
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
                } else if (name == "OPTION") {
                    get_lex();
                    if (lex != C_NAME) {
                        emit_error("required name after OPTION");
                    } else if (name == "EXPLICIT") {
                        get_lex();
                        if (lex == C_NAME && name == "ON") {
                            get_lex();
                            option_explicit = true;
                        } else if (lex == C_NAME && name == "OFF") {
                            get_lex();
                            option_explicit = false;
                        } else {
                            option_explicit = true;
                        }
                    } else if (name == "WARNINGS") {
                        get_lex();
                        if (lex == C_NAME && name == "ON") {
                            get_lex();
                            option_warnings = true;
                        } else if (lex == C_NAME && name == "OFF") {
                            get_lex();
                            option_warnings = false;
                        }
                    } else if (name == "MAP") {
                        class node *tree;
                        int type;
                        int map;

                        get_lex();
                        tree = eval_level0(&type);
                        if (tree->node_type() != C_NUM) {
                            emit_error("only constant expression for OPTION MAP parameter");
                        } else {
                            map = tree->node_value();
                            if (map < 0 || map > 7)
                                emit_error("bad map for OPTION MAP");
                            intybasic_map = map;
                        }

                    } else {
                        emit_error("non-recognized OPTION");
                    }
                } else if (name == "SEGMENT") {
                    class node *tree;
                    int type;
                    int first = -1;
                    int second = -1;

                    get_lex();
                    tree = eval_level0(&type);
                    if (tree->node_type() != C_NUM) {
                        emit_error("only constant expression for SEGMENT parameter");
                    } else {
                        first = tree->node_value() & 0xffff;
                    }
                    delete tree;
                    tree = NULL;
                    if (lex == C_NAME && name == "BANK") {
                        get_lex();
                        tree = eval_level0(&type);
                        if (tree->node_type() != C_NUM) {
                            emit_error("only constant expression for SEGMENT BANK parameter");
                        } else {
                            second = tree->node_value() & 0xffff;
                        }
                        delete tree;
                        tree = NULL;
                    }
                    if (second == -1) {
                        asm_output << "ROM.SelectSegment " << first << "\n";
                    } else {
                        asm_output << "ROM.SelectBank " << first << "," << second << "\n";

                    }
                } else if (name == "BANK") {
                    class node *tree;
                    int type;

                    get_lex();
                    if (lex == C_NAME && name == "SELECT") {
                        int first = -1;
                        int second = -1;
                        
                        get_lex();
                        tree = eval_level0(&type);
                        if (tree->node_type() != C_NUM) {
                            emit_error("only constant expression for BANK SELECT parameter");
                        } else {
                            second = tree->node_value() & 0xffff;
                        }
                        delete tree;
                        tree = NULL;
                        if (lex == C_NAME && name == "SEGMENT") {
                            get_lex();
                            tree = eval_level0(&type);
                            if (tree->node_type() != C_NUM) {
                                emit_error("only constant expression for BANK SELECT SEGMENT parameter");
                            } else {
                                first = tree->node_value() & 0xffff;
                            }
                            delete tree;
                            tree = NULL;
                        }
                        asm_output << "ROM.SwitchBank " << first << "," << second << "\n";
                    } else {
                        tree = eval_level0(&type);
                        if (tree->node_type() != C_NUM) {
                            emit_error("only constant expression for BANK parameter");
                        } else {
                            asm_output << "ROM.SelectBank -1," << (tree->node_value() & 0xffff) << "\n";
                        }
                        delete tree;
                        tree = NULL;
                    }
                } else if (macros[name] != NULL) {  // Function (macro)
                    if (!replace_macro()) {
                        compile_statement(check_for_else);
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
    
    // Remove line endings (in special \r to allow \ to work)
    void chomp(string &str) {
        string::size_type pos = str.find_last_not_of("\r\n");
        str.erase(pos + 1);
    }
    
    //
    // Compile a BASIC file
    //
    int compile(char *current_path, ifstream &input, const char *library_path) {
        char *p;
        int eof;
        string procedure;
        
        strcpy(path, current_path);
        eof = 0;
        while (1) {
            int label_exists;
            string line2;
            
            line2 = "";
            while (1) {
                if (!getline(input, line)) {
                    eof = 1;
                    break;
                }
                chomp(line);
                line_number++;
                line = line2 + line;
                if (line.length() > 0 && line[line.length() - 1] == '\\') {
                    line.erase(line.length() - 1, 1);
                    line2 = line;
                    continue;
                } else {
                    break;
                }
            }
            if (eof)
                break;
            line_start = 1;
            line_pos = 0;
            line_size = line.length();
            asm_output << "\t;[" << line_number << "] " << line << "\n";
            asm_output << "\tSRCFILE \"" << current_path << "\"," << line_number << "\n";
            get_lex();
            if (lex == C_LABEL) {
                if (value == 0)
                    global_label = name;
                if (labels.find(name) != labels.end()) {
                    if (label_used[name] & 2) {
                        string temp = "already defined '" + name + "' label";
                        
                        emit_error(temp);
                    } else {
                        label_proc[labels[name]] = inside_proc;
                    }
                } else {
                    labels[name] = next_label;
                    label_proc[next_label] = inside_proc;
                    name_mangling[next_label] = NAME_MANGLING_LABEL + name;
                    next_label++;
                }
                label_used[name] |= 2;
                asm_output << "\t; " << name << "\n";
                mangle_label(LABEL_PREFIX, labels[name]);
                asm_output << ":";
                label_exists = 1;
                procedure = name;
                get_lex();
                output->trash_registers();
            } else {
                label_exists = 0;
                procedure = "";
            }
            if (lex == C_NAME) {
                if (name == "PROCEDURE") {
                    if (!label_exists)
                        emit_error("PROCEDURE without label in same line");
                    else
                        label_used[procedure] |= IT_IS_PROCEDURE;
                    if (inside_proc)
                        emit_error("starting PROCEDURE without ENDing previous PROCEDURE");
                    // as1600 requires that label and PROC are on same line
                    get_lex();
                    asm_output << "\tPROC\n\tBEGIN\n";
                    inside_proc = labels[procedure];
                    last_is_return = 0;
                    output->trash_registers();
                } else if (name == "END" && sneak_peek() != 'I' && sneak_peek() != 'S') {  // END (and not END IF)
                    if (!inside_proc)
                        emit_warning("END without PROCEDURE");
                    else if (loops.size() > 0)
                        emit_error("Ending PROCEDURE with control block still open");
                    get_lex();
                    if (!last_is_return)
                        asm_output << "\tRETURN\n";
                    asm_output << "\tENDP\n";
                    inside_proc = 0;
                    last_is_return = 0;
                    output->trash_registers();
                } else if (name == "INCLUDE") {
                    int quotes;
                    ifstream include;
                    int saved_line_number;
                    char path1[4096];
                    
                    while (line_pos < line_size && isspace(line[line_pos]))
                        line_pos++;
                    
                    // Separate filename, admit use of quotes
                    if (line_pos < line_size && line[line_pos] == '"') {
                        quotes = 1;
                        line_pos++;
                    } else {
                        quotes = 0;
                    }
                    p = &path1[0];
                    while (p < &path1[4095] && line_pos < line_size) {
                        if (quotes && line[line_pos] == '"')
                            break;
                        *p++ = line[line_pos++];
                    }
                    if (quotes) {
                        if (line_pos >= line_size || line[line_pos] != '"')
                            emit_error("missing quotes in INCLUDE");
                        else
                            line_pos++;
                    } else {
                        while (p > &path1[0] && isspace(*(p - 1)))
                            p--;
                    }
                    *p = '\0';
                    
                    // Try to open in current directory and then try library path.
                    include.open(path1);
                    if (!include.is_open() && strlen(library_path) + strlen(path1) + 2 < 4095) {
                        char path2[4096];
                        
                        strcpy(path2, path1);
                        strcpy(path1, library_path);
#ifdef _WIN32
                        if (strlen(path1) > 0 && path1[strlen(path1) - 1] != '\\')
                            strcat(path1, "\\");
#else
                        if (strlen(path1) > 0 && path1[strlen(path1) - 1] != '/')
                            strcat(path1, "/");
#endif
                        strcat(path1, path2);
                        include.open(path1);
                    }
                    if (!include.is_open()) {
                        emit_error("INCLUDE not successful");
                        err_code = 2;
                    } else {
                        active_include++;
                        saved_line_number = line_number;
                        line_number = 0;
                        asm_output << "\t;FILE " << path1 << "\n";
                        compile(path1, include, library_path);
                        include.close();
                        asm_output << "\t;ENDFILE\n";
                        active_include--;
                        line_number = saved_line_number;
                        asm_output << "\t;FILE " << current_path << "\n";
                        strcpy(path, current_path);
                    }
                    lex = C_END;
                } else {
                    if (label_exists)
                        asm_output << "\t";
                    compile_statement(false);
                    output->dump();
                }
            }
            if (lex != C_END) {
                emit_error("invalid extra characters");
            }
        }
    };
    
public:
    //
    // Starts compilation
    //
    int start(const char *input_file, const char *output_file, const char *library_path, int flags, int cc3_start) {
        int used_space;
        int available_vars;
        char *temporary_file;
        int c;
        
        active_include = 0;
        global_label = "";
        line_number = 0;
        next_label = 1;
        next_var = 1;
        scroll_used = false;
        keypad_used = false;
        music_used = false;
        music_ecs_used = false;
        stack_used = false;
        numbers_used = false;
        voice_used = false;
        ecs_used = false;
        flash_used = false;
        playvol_used = false;
        col_used = false;
        jlp_used = ((flags & 1) != 0);
        jlp_flash_size = 16;
        cc3_used = ((flags & 2) != 0);
        warnings = ((flags & 4) == 0);
        option_warnings = true;
        fastmult_used = false;
        fastdiv_used = false;
        intybasic_map = 0;
        
        frame_drive = -1;

        option_explicit = false;
        
        err_code = 0;
        
        temporary_file = tmpnam(NULL);
        
        input.open(input_file);
        if (!input.is_open()) {
            std::cerr << "Error: Unable to open input file: " << input_file << "\n";
            return 2;
        }
        output = new code;

        asm_output.open(temporary_file);
        if (!asm_output.is_open()) {
            std::cerr << "Error: Unable to open temporary output file: " << output_file << "\n";
            input.close();
            return 2;
        }
        asm_output << "\t; IntyBASIC compiler " << VERSION << "\n";

        //
        // Clean JLP/CC3 RAM
        //
        if (jlp_used || cc3_used) {
            asm_output << "\n";
            asm_output << "\tMVII #SYSTEM2,R5\n";
            asm_output << "\tMVII #_SYSTEM2-SYSTEM2-1,R1\n";
            asm_output << "\tCLRR R0\n";
            asm_output << "\tMVO@ R0,R5\n";
            asm_output << "\tDECR R1\n";
            asm_output << "\tBPL $-2\n";
            asm_output << "\n";
        }

        bitmap_byte = 0;
        bitmap_state = 0;
        inside_proc = 0;
        // Must be defined in this order
        arrays["#MOBSHADOW"] = 24 | (next_label << 16);  // #MOBSHADOW array (label Q1)
        next_label++;
        arrays["#BACKTAB"] = 240 | (next_label << 16);  // #BACKTAB array (label Q2)
        next_label++;

        asm_output << "\t;FILE " << input_file << "\n";
        compile((char *) input_file, input, library_path);
        asm_output << "\t;ENDFILE\n";

        // Final check
        if (inside_proc) {
            if (loops.size() > 0)
                emit_error("End of source with control block still open");
            else
                emit_warning("End of source without ending PROCEDURE");
            if (!last_is_return)
                asm_output << "\tRETURN\n";
            asm_output << "\tENDP\n";
            inside_proc = 0;
            last_is_return = 0;
            output->trash_registers();
        }
        
        // Finish compiled source code with epilogue
        asm_output << "\t;ENDFILE\n";
        asm_output << "\tSRCFILE \"\",0\n";
        
        asm_output.close();
        input.close();

        // Now open the real output file
        asm_output.open(output_file);
        if (!asm_output.is_open()) {
            std::cerr << "Error: Unable to open assembly output file: " << output_file << "\n";
            return 2;
        }
        asm_output << "\tROMW 16\n";

        asm_output << "intybasic_map:\tequ " << intybasic_map << "\t; ROM map used\n";
        asm_output << "intybasic_jlp:\tequ " << (jlp_used ? 1 : 0) << "\t; JLP is used\n";
        asm_output << "intybasic_cc3:\tequ " << (cc3_used ? (cc3_start >> 8) : 0) << "\t; CC3 is used and where is RAM\n";
        if (jlp_used) {
            asm_output << "\tIF DEFINED __FEATURE.CFGVAR\n";
            if (jlp_used) {
                if (flash_used)
                    asm_output << "\t\tCFGVAR \"jlp\" = 3\n";
                else
                    asm_output << "\t\tCFGVAR \"jlp\" = 1\n";
            }
            asm_output << "\tENDI\n";
        }
        asm_output << "intybasic_ecs:\tequ " << (ecs_used ? 1 : 0) << "\t; Forces to include ECS startup\n";
        asm_output << "intybasic_voice:\tequ " << (voice_used ? 1 : 0) << "\t; Forces to include voice library\n";
        asm_output << "intybasic_flash:\tequ " << (flash_used ? 1 : 0) << "\t; Forces to include Flash memory library\n";
        if (voice_used || ecs_used || flash_used) {
            asm_output << "\tIF DEFINED __FEATURE.CFGVAR\n";
            if (voice_used)
                asm_output << "\t\tCFGVAR \"voice\" = 1\n";
            if (ecs_used)
                asm_output << "\t\tCFGVAR \"ecs\" = 1\n";
            if (flash_used)
                asm_output << "\t\tCFGVAR \"jlpflash\" = " << jlp_flash_size << "\n";
            asm_output << "\tENDI\n";
        }
        asm_output << "intybasic_scroll:\tequ " << (scroll_used ? 1 : 0) << "\t; Forces to include scroll library\n";
        asm_output << "intybasic_col:\tequ " << (col_used ? 1 : 0) << "\t; Forces to include collision detection\n";
        asm_output << "intybasic_keypad:\tequ " << (keypad_used ? 1 : 0) << "\t; Forces to include keypad library\n";
        asm_output << "intybasic_music:\tequ " << (music_used ? 1 : 0) << "\t; Forces to include music library\n";
        asm_output << "intybasic_music_ecs:\tequ " << (music_ecs_used ? 1 : 0) << "\t; Forces to include music library\n";
        asm_output << "intybasic_music_volume:\tequ " << (playvol_used ? 1 : 0) << "\t; Forces to include music volume change\n";
        asm_output << "intybasic_stack:\tequ " << (stack_used ? 1 : 0) << "\t; Forces to include stack overflow checking\n";
        asm_output << "intybasic_numbers:\tequ " << (numbers_used ? 1 : 0) << "\t; Forces to include numbers library\n";
        asm_output << "intybasic_fastmult:\tequ " << (fastmult_used ? 1 : 0) << "\t; Forces to include fast multiplication\n";
        asm_output << "intybasic_fastdiv:\tequ " << (fastdiv_used ? 1 : 0) << "\t; Forces to include fast division/remainder\n";
        
        // All constants are now generated
        
        // Copy the prologue
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
                chomp(line);
                if (line.find(";IntyBASIC MARK DON'T CHANGE") != string::npos) {  // Location to replace title
                    asm_output << "\tBYTE " << program_year << ",'" << program_title << "',0\n";
                } else {
                    asm_output << line << "\n";
                }
            }
            included.close();
        } else {
            std::cerr << "Error: Unable to include prologue: " << path << "\n";
            err_code = 2;
        }

        // Copy the compiled program
        included.open(temporary_file);
        if (included.is_open()) {
            while (getline(included, line)) {
                chomp(line);
                asm_output << line << "\n";
            }
            included.close();
        } else {
            std::cerr << "Error: Unable to include compiled program: " << temporary_file << "\n";
            err_code = 2;
        }
        remove(temporary_file);

        // Copy the epilogue
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
                    if (frame_drive >= 0) {
                        asm_output << "\tCALL ";
                        mangle_label(LABEL_PREFIX, frame_drive);
                        asm_output << "\n";
                    }
                } else {
                    chomp(line);
                    asm_output << line << "\n";
                }
            }
            included2.close();
        } else {
            std::cerr << "Error: Unable to include epilogue: " << path << "\n";
            err_code = 2;
        }
        
        // Warns of read but non-assigned variables
        for (access = read_write.begin(); access != read_write.end(); access++) {
            if (access->second == 1) {
                if (warnings && option_warnings)
                    std::cerr << "Warning: variable '" << access->first << "' read but never assigned\n";
            } else if (access->second == 2) {
                if (warnings && option_warnings)
                    std::cerr << "Warning: variable '" << access->first << "' assigned but never read\n";
            }
        }
        
        // Warns of unused or undefined labels
        for (access = label_used.begin(); access != label_used.end(); access++) {
            if ((access->second & 3) == 1) {
                std::cerr << "Error: label '" << access->first << "' undefined\n";
                err_code = 1;
            } else if ((access->second & 3) == 2) {
                if (warnings && option_warnings)
                    std::cerr << "Warning: label '" << access->first << "' defined but never used\n";
            }
            if ((access->second & (CALLED_BY_GOTO | IT_IS_PROCEDURE)) == (CALLED_BY_GOTO | IT_IS_PROCEDURE)) {
                std::cerr << "Error: PROCEDURE '" << access->first << "' jumped in by GOTO (guaranteed crash)\n";
                err_code = 1;
            }
            if ((access->second & (CALLED_BY_GOSUB | IT_IS_PROCEDURE)) == CALLED_BY_GOSUB) {
                std::cerr << "Error: Common label '" << access->first << "' jumped in by GOSUB (guaranteed crash)\n";
                err_code = 1;
            }
        }
        
        // Warns of wrong code flow
        for (access2 = proc_gotos.begin(); access2 != proc_gotos.end(); access2++) {
            inside_proc = access2->first;
            while (access2->second.size() > 0) {
                int goto_label;
                string proc1;
                string proc2;
                
                goto_label = access2->second.front();
                if (label_proc[goto_label] != inside_proc) {
                    if (inside_proc == 0)
                        proc1 = "main code";
                    else
                        proc1 = "procedure " + name_mangling[inside_proc].substr(6);
                    if (label_proc[goto_label] == 0)
                        proc2 = "main code";
                    else
                        proc2 = "procedure " + name_mangling[label_proc[goto_label]].substr(6);
                    std::cerr << "Error: GOTO " << name_mangling[goto_label].substr(6) << " from " << proc1 << " flows inside " << proc2 << " (stack disruption)\n";
                    err_code = 1;
                }
                access2->second.pop_front();
            }
        }
        
        // Dumps 8-bits variables
        used_space = 0;
        for (access = variables.begin(); access != variables.end(); access++) {
            if (access->first[0] != '#') {
                int size;
                
                size = 1;
                mangle_label(VAR_PREFIX, access->second);
                asm_output << ":\tRMB "
                    << size << "\t; " << access->first << "\n";
                used_space += size;
            }
        }
        for (access = arrays.begin(); access != arrays.end(); access++) {
            if (access->first[0] != '#' && access->second != 0) {
                int size;
                int label;
                
                size = access->second & 0xffff;
                if (size != 65535) {
                    label = access->second >> 16;
                    mangle_label(LABEL_PREFIX, label);
                    asm_output << ":\tRMB " << size << "\t; " << access->first << "\n";
                    used_space += size;
                }
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
            available_vars -= 28;
        if (music_ecs_used)
            available_vars -= 22;
        if (playvol_used)
            available_vars -= 1;
        if (used_space > available_vars) {
            std::cerr << "Error: Use of 8-bits variables exceeds available space (" << used_space << " vs " << available_vars << ")\n";
            err_code = 1;
        } else {
            std::cerr << used_space << " used 8-bit variables of " << available_vars << " available\n";
        }
        asm_output << "_SCRATCH:\tEQU $\n";
        
        // Arranges stack
        asm_output << "\nSYSTEM:\tORG $2F0, $2F0, \"-RWBN\"\n";
        asm_output << "STACK:\tRMB 24\n";
        used_space = 0;
        if (flash_used) {
            asm_output << "JF.SYSRAM:\tRMB 5\t; 5 words in Intv for support routine\n";
            used_space += 5;
        }
        if (jlp_used) {
            asm_output << "_SYSTEM:\tEQU $\n";
            asm_output << "\nSYSTEM2:\tORG $8040, $8040, \"-RWBN\"\n";
            used_space = 0;
        } else if (cc3_used) {
            static const char *hex = "0123456789abcdef";
            char address[3];
            
            address[0] = hex[(cc3_start >> 12) & 0x0f];
            address[1] = hex[(cc3_start >> 8) & 0x0f];
            address[2] = '\0';
            asm_output << "_SYSTEM:\tEQU $\n";
            asm_output << "\nSYSTEM2:\tORG $" << address << "40, $" << address << "40, \"=RW\"\n";
            used_space = 0;
        }
        
        if (flash_used) {
            asm_output << "JF.SV:\tRMB 6\t; Space for saving Intv registers\n";
            asm_output << "JF.SV.ISR   EQU     JF.SV + 0\n";
            asm_output << "JF.SV.R0    EQU     JF.SV + 1\n";
            asm_output << "JF.SV.R1    EQU     JF.SV + 2\n";
            asm_output << "JF.SV.R2    EQU     JF.SV + 3\n";
            asm_output << "JF.SV.R4    EQU     JF.SV + 4\n";
            asm_output << "JF.SV.R5    EQU     JF.SV + 5\n";
            
            used_space += 6;
        }
        
        // Dumps 16-bits variables
        for (access = variables.begin(); access != variables.end(); access++) {
            if (access->first[0] == '#') {
                int size;
                
                size = 1;
                mangle_label(VAR_PREFIX, access->second);
                asm_output << ":\tRMB "
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
                    // Defined in intybasic_epilogue.asm to avoid as1600 warning
                    if (label != 1)  // Never should happen
                        std::cerr << "Error: non-synchronized label number for #MOBSHADOW\n";
                    // asm_output << LABEL_PREFIX << label << ":\tEQU _mobs\n";
                } else if (access->first == "#BACKTAB") {
                    // Defined in intybasic_epilogue.asm to avoid as1600 warning
                    if (label != 2)  // Never should happen
                        std::cerr << "Error: non-synchronized label number for #BACKTAB\n";
                    // asm_output << LABEL_PREFIX << label << ":\tEQU $0200\n";
                } else {
                    size = access->second & 0xffff;
                    if (size != 65535) {
                        mangle_label(LABEL_PREFIX, label);
                        asm_output << ":\tRMB " << size << "\t; " << access->first << "\n";
                        used_space += size;
                    }
                }
            }
        }
        if (jlp_used || cc3_used) {
            available_vars = 0x9f80 - 0x8040;
        } else {
            available_vars = 0x33f - 0x2f0 - 24;
            if (voice_used)
                available_vars -= 10;
            if (scroll_used)
                available_vars -= 20;
            if (col_used)
                available_vars -= 8;
        }
        if (used_space > available_vars) {
            std::cerr << "Error: Use of 16-bits variables exceeds available space (" << used_space << " vs " << available_vars << ")\n";
            err_code = 1;
        } else {
            std::cerr << used_space << " used 16-bit variables of " << available_vars << " available\n";
        }
        
        if (jlp_used || cc3_used) {
            asm_output << "_SYSTEM2:\tEQU $\n";
        } else {
            asm_output << "_SYSTEM:\tEQU $\n";
        }
        
#if 0
        // Dumps functions reference (not needed anymore)
        for (access = functions.begin(); access != functions.end(); access++) {
            if (access->second != 0)
                asm_output << FUNC_PREFIX << access->second << ":\tEQU " << access->first << "\n";
        }
#endif
    
        asm_output.close();
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
    int start;
    
    std::cerr << "\nIntyBASIC compiler " << VERSION << "\n";
    std::cerr << "(c) 2014-2025 Oscar Toledo G. http://nanochess.org/\n\n";
    
    // Get year and default title for program
    // And yep, use old-style C functions :)
    actual = time(0);
    date = localtime(&actual);
    program_year = date->tm_year;
    strcpy(program_title, "IntyBASIC program");
    
    // Process command line arguments
    base = 1;
    flags = 0;
    start = 0x8000;
    while (1) {
        if (argc > base && strcmp(argv[base], "--jlp") == 0) {
            flags |= 1;
            base++;
        } else if (argc > base && strcmp(argv[base], "--cc3") == 0) {
            flags |= 2;
            base++;
            if (argc > base && argv[base][0] == '0' && tolower(argv[base][1]) == 'x') {
                long int c;
                
                c = strtol(&argv[base][2], 0, 16);
                if (c)
                    start = (int) c;
                base++;
            }
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
        std::cerr << "    --jlp        Enables use of 8K-words extra memory feature of JLP\n";
        std::cerr << "                 and also usage of hardware acceleration for\n";
        std::cerr << "                 multiplication and division.\n";
        std::cerr << "    --cc3        Enables use of 8K-words extra memory feature of\n";
        std::cerr << "                 Cuttle Cart 3.\n";
        std::cerr << "    --cc3 0xc000 Enables Cuttle Cart 3 in indicated page, useful\n";
        std::cerr << "                 with Keyboard Component.\n";
        std::cerr << "    --title \"a\"  Selects title of the compiled program.\n";
        std::cerr << "                 By default this is \"IntyBASIC program\".\n";
        std::cerr << "                 Only appears in emulators/multicarts.\n";
        std::cerr << "    -w           Disable warnings globally (has priority over\n";
        std::cerr << "                 OPTION WARNINGS)\n\n";
        std::cerr << "    The library path is where the intybasic_prologue.asm and\n";
        std::cerr << "    intybasic_epilogue.asm files are searched for inclusion.\n";
        std::cerr << "\n";
        std::cerr << "    It will return a zero error code if compilation was\n";
        std::cerr << "    successful or non-zero otherwise.\n\n";
        std::cerr << "Many thanks to Albert, artrag, atari2600land, awhite2600, carlsson,\n";
        std::cerr << "catsfolly, ckblackm, CrazyBoss, Cybearg, DZ-Jay, First Spear,\n";
        std::cerr << "freewheel, GroovyBee, intvnut, Jess Ragan, Kiwi, RevEng, SpiceWare\n";
        std::cerr << "and Tarzilla.\n";
        std::cerr << "\n";
        return 0;
    }
    return basic.start(argv[base], argv[base + 1], (argc > base + 2) ? argv[base + 2] : "", flags, start & 0xff00);
}
