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
//  Revision: Jan/28/2014. Modified to be compilable under Visual C++ 2008. Relational
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
//                         pstring changed to C++ string and then merged with
//                         'name' variable. DEF FN now accepts strings. Macro
//                         replacement now works with strings. New LEN function.
//                         Reversed push order for macro replacement so it now
//                         works properly with nested macros.
//  Revision: Jul/31/2015. Added POS() function. Shows file when error or warning
//                         inside INCLUDE. New function emit_warning(). Added
//                         support for EXIT FOR and EXIT WHILE. Support for
//                         multiline IF and ELSEIF. Solved bug in processing of
//                         constants for GRAM characters. Added DO/LOOP with
//                         support for WHILE/UNTIL in both sides, also EXIT DO.
//                         Removed Tab from source code, Github disrupted it.
//  Revision: Aug/11/2015. Avoids bug of getting stuck in DEF FN when macro had
//                         unbalanced parenthesis.
//  Revision: Aug/19/2015. Solves bug where exit label for block IF was always 0.
//                         Optimized POKE code generation. Optimized plus + minus
//                         constant. Added #BACKTAB array.
//  Revision: Aug/21/2015. Now keeps stack in internal memory. Added support for
//                         JLP Flash with FLASH statement.
//  Revision: Aug/24/2015. Generates warnings for assigning values bigger than 8
//                         bits to variables and also for TO values. Now warns of
//                         unused, undefined and redefined labels.
//  Revision: Aug/28/2015. Solved a couple of small bugs where error code was
//                         set wrongly for warnings or not set for errors.
//  Revision: Aug/30/2015. Doesn't warn for assignment of -128 to -1 to 8 bits.
//                         Added support for signed 8-bit variables (SIGNED
//                         statement)
//  Revision: Aug/31/2015. Further optimization for use of 8-bit signed variables.
//                         SPRITE now supports expression in MOB index. SCREEN
//                         now generates more efficient code and allows fifth
//                         parameter to choose width of origin screen, useful for
//                         big maps. Added DIM AT for putting arrays at any
//                         address in memory, useful for unforeseen hardware.
//  Revision: Sep/01/2015. Corrected bug in access to #MOBSHADOW for SPRITE and
//                         added optimization for saving of register across
//                         expressions. Now labels for #MOBSHADOW and #BACKTAB are
//                         defined outside in intybasic_epilogue.asm. Solved bug
//                         where warnings could not be disabled.
//  Revision: Sep/11/2015. Optimizes sequences of ANDI/XORI instructions.
//  Revision: Sep/14/2015. Launches warning in case of variable assignment to name
//                         previously used with CONST. Solved bug where ELSEIF
//                         should be finished with ELSE.
//  Revision: Sep/19/2015. A few warnings emitted wrongly an error code.
//  Revision: Sep/22/2015. Warns about assignment to internal variable names.
//  Revision: Jan/22/2016. Added NO DRUMS syntax to PLAY SIMPLE and PLAY FULL.
//                         DO followed by colon now isn't taken as label.
//                         Updated copyright.
//  Revision: Jan/23/2016. Added MUSIC JUMP statement.
//  Revision: Jan/25/2016. SOUND now allows constant expressions as first
//                         parameter.
//  Revision: Jan/27/2016. Added MUSIC.PLAYING status.
//  Revision: Jan/28/2016. Added UNSIGNED statement and support for unsigned
//                         comparisons of 16-bits variables.
//  Revision: Feb/16/2016. Added support for strings in DATA. Added CALL (like
//                         USR but doesn't return value)
//  Revision: Mar/16/2016. Nobody ever tested CONT3 and CONT4, because these
//                         weren't enabled.
//  Revision: Apr/26/2016. Detects if source code ends without finishing
//                         PROCEDURE.
//  Revision: May/03/2016. Now is an error to start a PROCEDURE without closing
//                         the previous one. MODE now sets a different value in
//                         _mode_select. Generates warning when more than 16
//                         GRAM defined per video frame.
//  Revision: Aug/12/2016. Solved bug where DEFINE with VARPTR swallowed one
//                         extra lexical component.
//  Revision: Aug/25/2016. Solved bug when using qs_mpy16 like 1024-x*x. Optimizes
//                         array(4+index) and now subexpression also optimizes
//                         accesses using same index to any array/offset combo.
//  Revision: Sep/27/2016. Had to change extra memory flags so LTO-Flash can
//                         detect correctly the ROM type.
//  Revision: Oct/06/2016. Added OPTION EXPLICIT.
//  Revision: Oct/07/2016. Added DATA PACKED and OPTION WARNINGS.
//  Revision: Feb/05/2018. VOICE INIT now calls IV_HUSH.
//  Revision: Feb/08/2018. Size of Flash memory is now configurable.
//  Revision: Feb/10/2018. Added data of what labels are procedures and if called
//                         by GOTO and GOSUB to detect blatant errors that cause
//                         crashes. (Miner 2049er)
//  Revision: Feb/18/2018. Allows numbers in DATA PACKED (ARTRAG suggestion).
//  Revision: Mar/01/2018. Music now allows 8 channels (ECS support). Added
//                         MUSIC SPEED, MUSIC GOSUB, MUSIC RETURN and MUSIC
//                         VOLUME.
//  Revision: Apr/17/2018. Can optimize VARPTR array1(x) - VARPTR array2(y) like
//                         in array optimization.
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
#include <cstring>
#include <ctime>

using namespace std;

#include "global.h"     // Global definitions
#include "microcode.h"  // Class microcode
#include "code.h"       // Class code
#include "node.h"       // Class node

const string VERSION = "v1.4.0 Feb/05/2018";      // Compiler version

const string LABEL_PREFIX = "Q";    // Prefix for BASIC labels
const string TEMP_PREFIX = "T";     // Prefix for temporal labels
const string VAR_PREFIX = "V";      // Prefix for BASIC variables
const string FUNC_PREFIX = "F";     // Prefix for USR functions

const int CALLED_BY_GOTO = 0x04;
const int CALLED_BY_GOSUB = 0x08;
const int IT_IS_PROCEDURE = 0x10;

class code *output;

int next_local = 1;

ofstream asm_output;

bool optimized;      // Indicates if expression for IF statement jump was optimized
bool jlp_used;       // Indicates if JLP is used
int jlp_flash_size;  // Indicates JLP flash size
bool cc3_used;       // Indicates if CC3 is used
bool fastmult_used;  // Indicates if fast multiplication is used
bool fastdiv_used;   // Indicates if fast division/remainder is used
bool music_used;     // Indicates if music used
bool music_ecs_used; // Indicates if ECS music used
bool warnings;       // Indicates if warnings are generated

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
    int type;           // 0=FOR, 1=WHILE, 2=IF, 3=DO WHILE/UNTIL LOOP, 4=DO LOOP WHILE/UNTIL
    class node *step;
    class node *final;
    string var;
    int label_loop;     // Main label, in C this would be destination for 'continue'
    int label_exit;     // Exit label, in C this would be destination for 'break'
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
    list <struct loop> loops;
    list <struct loop>::iterator loop_explorer;
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
            
            value = 0;
            while (line_pos < line_size && isdigit(line[line_pos]))
                value = (value * 10) + line[line_pos++] - '0';
            if (value > 65535) {
                emit_warning("number exceeds 16 bits");
            }
            if (line_pos < line_size && line[line_pos] == '.'
                && line_pos + 1 < line_size && isdigit(line[line_pos + 1])) {
                if (value > 255) {
                    emit_warning("fixed number exceeds basic 8 bits");
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
            name = "";
            while (line_pos < line_size && line[line_pos] != '"') {
                int c;
                
                if (line[line_pos] == '\\') {
                    line_pos++;
                    if (line_pos < line_size && line[line_pos] == '"') {
                        c = line[line_pos++] - 32;
                        if (c < 0)
                            c = 0;
                        if (c > 95)
                            c = 95;
                    } else {
                        c = 0;
                        while (line_pos < line_size && isdigit(line[line_pos])) {
                            c = c * 10 + (line[line_pos] - '0');
                            line_pos++;
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
        
        if (lex == C_MINUS) {
            get_lex();
            left = eval_level7(type);
            left = new node(C_NEG, 0, left, NULL);
        } else if (lex == C_NAME && name == "NOT") {
            get_lex();
            left = eval_level7(type);
            left = new node(C_NOT, 0, left, NULL);
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
                    variables[name] = next_var++;
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
                emit_error("syntax error in MUSIC");
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
                temp = constants[name] & 0xffff;
                get_lex();
                return new node(C_NUM, temp, NULL, NULL);
            }
            read_write[name] = (read_write[name] | 1);
            if (variables[name] == 0) {
                check_for_explicit(name);
                variables[name] = next_var++;
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
                arrays[name] = 10 | (next_label++ << 16);
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
            variables[name] = next_var++;
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
                            next_label++;
                        }
                        label_used[name] |= 1;
                        label_used[name] |= CALLED_BY_GOTO;
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
                            new_loop.type = 2;
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
                    } else if (loops.front().type != 2 || loops.front().label_loop == 0) {
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
                    } else if (loops.front().type != 2) {
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
                    if (lex != C_NAME || name != "IF") {
                        emit_error("wrong END");
                    } else {
                        get_lex();
                        if (loops.size() == 0 || loops.front().type != 2) {
                            emit_error("Bad nested END IF");
                        } else {
                            if (loops.front().var == "1")
                                output->emit_l(TEMP_PREFIX, loops.front().label_exit);
                            if (loops.front().label_loop != 0)
                                output->emit_l(TEMP_PREFIX, loops.front().label_loop);
                            loops.pop_front();
                        }
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
                    new_loop.type = 0;
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
                    new_loop.type = 1;
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
                    } else if (loops.front().type != 1) {
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
                        new_loop.type = 3;  // Condition at top
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
                        new_loop.type = 3;  // Condition at top
                    } else {
                        new_loop.var = ""; // Doesn't use exit label (yet)
                        new_loop.type = 4;  // Condition at bottom
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
                    } else if (loops.front().type == 3) {
                        output->emit_a(N_B, TEMP_PREFIX, loops.front().label_loop);
                        output->emit_l(TEMP_PREFIX, loops.front().label_exit);
                        loops.pop_front();
                    } else if (loops.front().type == 4) {
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
                } else if (name == "EXIT") {
                    get_lex();
                    
                    // Avoid IF blocks
                    loop_explorer = loops.begin();
                    while (loop_explorer != loops.end()) {
                        if (loop_explorer->type != 2)
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
                            if (loops.size() == 0 || loop_explorer->type != 0) {
                                emit_error("EXIT FOR without FOR");
                            } else {
                                if (loop_explorer->label_exit == 0)
                                    loop_explorer->label_exit = next_local++;
                                output->emit_a(N_B, TEMP_PREFIX, loop_explorer->label_exit);
                            }
                        } else if (name == "WHILE") {
                            get_lex();
                            if (loops.size() == 0 || loop_explorer->type != 1) {
                                emit_error("EXIT WHILE without WHILE");
                            } else {
                                output->emit_a(N_B, TEMP_PREFIX, loop_explorer->label_exit);
                            }
                        } else if (name == "DO") {
                            get_lex();
                            if (loops.size() == 0 || (loop_explorer->type != 3 && loop_explorer->type != 4)) {
                                emit_error("EXIT DO without DO");
                            } else {
                                loop_explorer->var = "1";
                                output->emit_a(N_B, TEMP_PREFIX, loop_explorer->label_exit);
                            }
                        } else {
                            emit_error("only supported EXIT WHILE/FOR/DO");
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
                                for (c = 0; c < name.length(); c++) {
                                    if (name[c] == 127 && c + 1 < name.length()) {
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
                                                variables[name] = next_var++;
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
                                
                                for (c = 0; c < name.length(); c++) {
                                    if (name[c] == 127 && c + 1 < name.length()) {
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
                            for (c = 0; c < name.length(); c++) {
                                if (name[c] == 127 && c + 1 < name.length()) {
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
                    if (lex != C_STRING || name.length() != 8) {
                        emit_error("syntax error in BITMAP");
                    } else {
                        int c;
                        
                        value = 0;
                        for (c = 0; c < 8; c++) {
                            if (name[c] != 0x10 && name[c] != 0x3f   // 0 and _
                             && name[c] != 0x00 && name[c] != 0x0e)  // space and .
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
                            constants[assigned] = (tree->node_value() & 0xffff) | 0x10000;
                        }
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
                                arrays[array] = c | (next_label++ << 16);
                                if (where >= 0) {
                                    asm_output << LABEL_PREFIX << (arrays[array] >> 16) << ":\tEQU " << where << "\t; " << array << "\n";
                                    arrays[array] = (arrays[array] - c) + 65535;  // Make length 65535
                                }
                            }
                        } else {
                            if (variables[array] != 0) {
                                string message;
                                
                                message = "variable '" + array + "' already defined";
                                emit_error(message);
                            }
                            variables[array] = next_var++;
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
                    assigned = name;
                    if (arrays[name] != 0) {
                        label = arrays[name] >> 16;
                    } else if (labels[name] == 0) {
                        label = labels[name] = next_label;
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
                            music_ecs_used = 1;
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
                        label_used[name] |= 1;
                        frame_drive = labels[name];
                        get_lex();
                    } else {
                        eval_expr(0, 0);
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
                                    next_label++;
                                }
                                label_used[name] |= 1;
                                if (gosub != 0)
                                    label_used[name] |= CALLED_BY_GOSUB;
                                else
                                    label_used[name] |= CALLED_BY_GOTO;
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
                            output->emit_nr(N_CMPI, "", max_value, 0);
                            output->emit_a(N_BC, TEMP_PREFIX, label);
                        }
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
                    } else {
                        emit_error("non-recognized OPTION");
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
    
public:
    //
    // Starts compilation
    //
    int start(const char *input_file, const char *output_file, const char *library_path, int flags) {
        int used_space;
        int available_vars;
        int inside_proc;
        char *p;
        int eof;
        string procedure;
        
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
        jlp_used = ((flags & 1) != 0);
        jlp_flash_size = 16;
        cc3_used = ((flags & 2) != 0);
        warnings = ((flags & 4) == 0);
        option_warnings = true;
        fastmult_used = false;
        fastdiv_used = false;
        
        frame_drive = -1;

        option_explicit = false;
        
        active_include = 0;
        next_include = 0;
        err_code = 0;
        input.open(input_file);
        if (!input.is_open()) {
            std::cerr << "Error: Unable to open input file: " << input_file << "\n";
            return 2;
        }
        output = new code;
        asm_output.open(output_file);
        if (!asm_output.is_open()) {
            std::cerr << "Error: Unable to open output file: " << output_file << "\n";
            input.close();
            return 2;
        }
        asm_output << "\t; IntyBASIC compiler " << VERSION << "\n";
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
            std::cerr << "Error: Unable to include prologue: " << path << "\n";
            err_code = 2;
        }

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

        asm_output << "\t;FILE " << input_file << "\n";
        bitmap_byte = 0;
        inside_proc = 0;
        // Must be defined in this order
        arrays["#MOBSHADOW"] = 24 | (next_label++ << 16);  // #MOBSHADOW array (label Q1)
        arrays["#BACKTAB"] = 240 | (next_label++ << 16);  // #BACKTAB array (label Q2)
        eof = 0;
        while (1) {
            int label_exists;
            string line2;
            
            line2 = "";
            while (1) {
                if (active_include) {
                    if (!getline(include[next_include], line)) {
                        include[next_include].close();
                        asm_output << "\t;ENDFILE\n";
                        next_include++;
                        active_include = 0;
                        line_number = saved_line_number;
                        asm_output << "\t;FILE " << input_file << "\n";
                    } else {
                        line_number++;
                    }
                }
                if (!active_include) {
                    if (!getline(input, line)) {
                        eof = 1;
                        break;
                    }
                    line_number++;
                }
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
//            std::cerr << line << "\n";
            line_start = 1;
            line_pos = 0;
            line_size = line.length();
            asm_output << "\t;[" << line_number << "] " << line << "\n";
            asm_output << "\tSRCFILE \"" << (active_include ? path : input_file) << "\"," << line_number << "\n";
            get_lex();
            if (lex == C_LABEL) {
                if (labels.find(name) != labels.end()) {
                    if (label_used[name] & 2) {
                        string temp = "already defined '" + name + "' label";
                        
                        emit_error(temp);
                    }
                } else {
                    labels[name] = next_label;
                }
                label_used[name] |= 2;
                asm_output << "\t; " << name << "\n";
                asm_output << LABEL_PREFIX << labels[name] << ":";
                next_label++;
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
                    inside_proc = 1;
                    last_is_return = 0;
                    output->trash_registers();
                } else if (name == "END" && sneak_peek() != 'I') {  // END (and not END IF)
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
                    
                    if (next_include == 50) {  // No more than 50 INCLUDE
                        emit_error("more than 50 INCLUDE used");
                    } else if (active_include) {  // No nested INCLUDE
                        emit_error("trying to use INCLUDE inside INCLUDE");
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
                                emit_error("missing quotes in INCLUDE");
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
                                emit_error("too many INCLUDE used");
                            } else {
                                include[next_include].open(path);
                            }
                        }
                        if (!include[next_include].is_open()) {
                            emit_error("INCLUDE not successful");
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
                    compile_statement(false);
                    output->dump();
                }
            }
            if (lex != C_END) {
                emit_warning("invalid extra characters");
            }
            
        }
    
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
        if (voice_used || ecs_used || flash_used) {
            asm_output << "\tIF DEFINED __FEATURE.CFGVAR\n";
            if (voice_used)
                asm_output << "\t\tCFGVAR \"voice\" = 1\n";
            if (ecs_used)
                asm_output << "\t\tCFGVAR \"ecs\" = 1\n";
            if (flash_used)
                asm_output << "\t\tCFGVAR \"jlpflash\" = " << jlp_flash_size << "\n";  // Currently hard-coded 100 sectors
            asm_output << "\tENDI\n";
        }
        if (scroll_used)
            asm_output << "intybasic_scroll:\tequ 1\t; Forces to include scroll library\n";
        if (keypad_used)
            asm_output << "intybasic_keypad:\tequ 1\t; Forces to include keypad library\n";
        if (music_used)
            asm_output << "intybasic_music:\tequ 1\t; Forces to include music library\n";
        if (music_ecs_used)
            asm_output << "intybasic_music_ecs:\tequ 1\t; Forces to include music library\n";
        if (playvol_used)
            asm_output << "intybasic_music_volume:\tequ 1\t; Forces to include music volume change\n";
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
        if (flash_used)
            asm_output << "intybasic_flash:\tequ 1\t; Forces to include Flash memory library\n";
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
                if (size != 65535) {
                    label = access->second >> 16;
                    asm_output << LABEL_PREFIX << label << ":\tRMB " << size << "\t; " << access->first << "\n";
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
        if (jlp_used || cc3_used) {
            asm_output << "_SYSTEM:\tEQU $\n";
            asm_output << "\nSYSTEM2:\tORG $8040, $8040, \"-RWBN\"\n";
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
                        asm_output << LABEL_PREFIX << label << ":\tRMB " << size << "\t; " << access->first << "\n";
                        used_space += size;
                    }
                }
            }
        }
        if (jlp_used || cc3_used) {
            available_vars = 0x9f80 - 0x8040;
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
        
        if (jlp_used || cc3_used) {
            asm_output << "_SYSTEM2:\tEQU $\n";
        } else {
            asm_output << "_SYSTEM:\tEQU $\n";
        }
        
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
    std::cerr << "(c) 2014-2016 Oscar Toledo G. http://nanochess.org/\n\n";
    
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
        std::cerr << "                multiplication and division.\n";
        std::cerr << "    --cc3       Enables use of 8K-words extra memory feature of\n";
        std::cerr << "                Cuttle Cart 3.\n";
        std::cerr << "    --title \"a\" Selects title of the compiled program.\n";
        std::cerr << "                By default this is \"IntyBASIC program\".\n";
        std::cerr << "                Only appears in emulators/multicarts.\n";
        std::cerr << "    -w          Disable warnings globally (has priority over\n";
        std::cerr << "                OPTION WARNINGS)\n\n";
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
    return basic.start(argv[base], argv[base + 1], (argc > base + 2) ? argv[base + 2] : "", flags);
}
