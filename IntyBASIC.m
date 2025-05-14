/*
**  intybasic.m
**  intybasic
**
**  Created by Oscar Toledo on 13/01/14.
**  Copyright (c) 2014 Oscar Toledo. All rights reserved.
**
**  Revision: Oct/20/2014. Ported to Objective C++.
**  Revision: Oct/24/2014. Port completed.
*/

#import <Foundation/Foundation.h>
#include <stdio.h>
#include <ctype.h>

#define VERSION        "v1.0a1 Oct/24/2014"    /* Compiler version */
#define LABEL_PREFIX   "Q"                     /* Prefix for BASIC labels */
#define TEMP_PREFIX    "T"                     /* Prefix for temporal labels */
#define VAR_PREFIX     "V"                     /* Prefix for BASIC variables */

enum lexical_component {C_END, C_NAME, C_NAME_RO, C_STRING, C_LABEL, C_NUM,
    C_OR, C_XOR, C_AND, C_NOT, C_NEG, C_PEEK, C_VAR, C_ASSIGN,
    C_EQUAL, C_NOTEQUAL, C_LESS, C_LESSEQUAL, C_GREATER, C_GREATEREQUAL,
    C_PLUS, C_MINUS, C_PLUSF, C_MINUSF, C_MUL, C_DIV, C_MOD,
    C_LPAREN, C_RPAREN, C_COLON, C_PERIOD, C_COMMA,
    C_ERR};

FILE *output;
int next_local = 1;
int optimized;

@interface node : NSObject { 
    enum lexical_component _type;
    int _value;
    int _regs;
    node *_left;
    node *_right;
}
-(id) initWithType: (enum lexical_component) type value: (int) value left: (node *) left right: (node *) right;
-   (enum lexical_component) nodeType;
-   (int) nodeValue;
-   (void) label;
-   (void) generateReg: (int) reg decision: (int) decision;
@end

@implementation node 
    
    /*
    ** Builds an expression node
    */
-(id) initWithType: (enum lexical_component) type value: (int) value left: (node *) left right: (node *) right {
    self = [super init];
    if (self) {
        self->_type = type;
        self->_value = value;
        self->_regs = 0;
        self->_left = left;
        self->_right = right;
        // Optimizes constant expressions
        if (type == C_PLUS && left->_type == C_NUM && right->_type == C_NUM) {
            self->_type = C_NUM;
            self->_value = left->_value + right->_value;
            self->_left = NULL;
            self->_right = NULL;
        }
        if (type == C_MINUS && left->_type == C_NUM && right->_type == C_NUM) {
            self->_type = C_NUM;
            self->_value = left->_value - right->_value;
            self->_left = NULL;
            self->_right = NULL;
        }
        if (type == C_MUL && left->_type == C_NUM && right->_type == C_NUM) {
            self->_type = C_NUM;
            self->_value = left->_value * right->_value;
            self->_left = NULL;
            self->_right = NULL;
        }
        if (type == C_DIV && left->_type == C_NUM && right->_type == C_NUM && right->_value) {
            self->_type = C_NUM;
            self->_value = left->_value / right->_value;
            self->_left = NULL;
            self->_right = NULL;
        }
        if (type == C_MOD && left->_type == C_NUM && right->_type == C_NUM && right->_value) {
            self->_type = C_NUM;
            self->_value = left->_value % right->_value;
            self->_left = NULL;
            self->_right = NULL;
        }
        if (type == C_AND && left->_type == C_NUM && right->_type == C_NUM) {
            self->_type = C_NUM;
            self->_value = left->_value & right->_value;
            self->_left = NULL;
            self->_right = NULL;
        }
        if (type == C_OR && left->_type == C_NUM && right->_type == C_NUM) {
            self->_type = C_NUM;
            self->_value = left->_value | right->_value;
            self->_left = NULL;
            self->_right = NULL;
        }
        if (type == C_XOR && left->_type == C_NUM && right->_type == C_NUM) {
            self->_type = C_NUM;
            self->_value = left->_value ^ right->_value;
            self->_left = NULL;
            self->_right = NULL;
        }
        if (type == C_NOT && left->_type == C_NUM) {
            self->_type = C_NUM;
            self->_value = ~left->_value;
            self->_left = NULL;
        }
        if (type == C_NEG && left->_type == C_NUM) {
            self->_type = C_NUM;
            self->_value = -left->_value;
            self->_left = NULL;
        }
    }
    return self;
}
    
    /*
    ** Get node type
    */
-   (enum lexical_component) nodeType {
        return _type;
    }

    /*
    ** Get node value
    */
-   (int) nodeValue {
        return _value;
    }
    
    /*
    ** Analyze each node and calculate number of registers used
    */
-   (void) label {
        
        /* Terminal nodes */
        if (_left == NULL && _right == NULL) {
            _regs = 1;
            return;
        }
        
        /* Unary nodes */
        if (_right == NULL) {
            [_left label];
            _regs = _left->_regs;
            return;
        }
        
        /* Binary nodes */
        [_left label];
        [_right label];
        if (_right->_type == C_NUM || _right->_type == C_NAME) {
            _regs = _left->_regs;
        } else {
            
            /* Switch arguments for addition operators in order to optimize them */
            if (_type == C_PLUS || _type == C_PLUSF) {
                if (_right->_type != C_NUM && _right->_type != C_NAME
                    && (_left->_type == C_NUM || _left->_type == C_NAME)) {
                    node *temp;
                    
                    temp = _left;
                    _left = _right;
                    _right = temp;
                }
            }
            if (_left->_regs > _right->_regs)
                _regs = _left->_regs;
            else if (_right->_regs > _left->_regs)
                _regs = _right->_regs;
            else
                _regs = _left->_regs + 1;
        }
    }
    
    /*
    ** Generates code for an expression tree
    */
-   (void) generateReg: (int) reg decision: (int) decision {
        if (reg >= 4) {
            fprintf(stderr, "Too complex expression\n");
            return;
        }
        switch (_type) {
            default:    /* Non-defined node, never should happen */
                fprintf(stderr, "\t; >>> Houston, we have a problem <<<\n");
                break;
            case C_NUM: /* Number */
                if ((_value & 0xffff) == 0)
                    fprintf(output, "\tCLRR R%d\n", reg);
                else
                    fprintf(output, "\tMVII #%d,R%d\n", (_value & 0xffff), reg);
                break;
            case C_NAME:    /* Variable */
                fprintf(output, "\tMVI " VAR_PREFIX "%d,R%d\n", _value, reg);
                break;
            case C_NAME_RO: /* Access to label */
                fprintf(output, "\tMVII #" LABEL_PREFIX "%d,R%d\n", _value, reg);
                break;
            case C_NOT: /* NOT */
                [_left generateReg: reg decision: 0];
                fprintf(output, "\tCOMR R%d\n", reg);
                break;
            case C_NEG: /* Negation */
                [_left generateReg: reg decision: 0];
                fprintf(output, "\tNEGR R%d\n", reg);
                break;
            case C_VAR: /* Special variable */
                if (_value == 0)
                    fprintf(output, "\tMVI _col0,R%d\n", reg);
                if (_value == 1)
                    fprintf(output, "\tMVI _col1,R%d\n", reg);
                if (_value == 2)
                    fprintf(output, "\tMVI _col2,R%d\n", reg);
                if (_value == 3)
                    fprintf(output, "\tMVI _col3,R%d\n", reg);
                if (_value == 4)
                    fprintf(output, "\tMVI _col4,R%d\n", reg);
                if (_value == 5)
                    fprintf(output, "\tMVI _col5,R%d\n", reg);
                if (_value == 6)
                    fprintf(output, "\tMVI _col6,R%d\n", reg);
                if (_value == 7)
                    fprintf(output, "\tMVI _col7,R%d\n", reg);
                if (_value == 8)
                    fprintf(output, "\tMVI _frame,R%d\n", reg);
                if (_value == 9)
                    fprintf(output, "\tMVI _rand,R%d\n", reg);
                if (_value == 10)
                    fprintf(output, "\tMVI _cnt1_key,R%d\n", reg);
                if (_value == 11)
                    fprintf(output, "\tMVI _cnt2_key,R%d\n", reg);
                if (_value == 12)
                    fprintf(output, "\tMVI _ntsc,R%d\n", reg);
                break;
            case C_PEEK:    /* PEEK() */
                if (_left->_type == C_NUM) {
                    fprintf(output, "\tMVI %d,R%d\n", _left->_value, reg);
                } else {
                    [_left generateReg: reg decision: 0];
                    fprintf(output, "\tMOVR R%d,R4\n", reg);
                    fprintf(output, "\tMVI@ R4,R%d\n", reg);
                }
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
                
                /* Optimize right side when it's constant */
                if (_right->_type == C_NUM && _type != C_ASSIGN) {
                    [_left generateReg: reg decision: 0];
                    if (_type == C_PLUS) {
                        if ((_right->_value & 0xffff) == 1)
                            fprintf(output, "\tINCR R%d\n", reg);
                        else if ((_right->_value & 0xffff) != 0)
                            fprintf(output, "\tADDI #%d,R%d\n", (_right->_value & 0xffff), reg);
                    } else if (_type == C_MINUS) {
                        if ((_right->_value & 0xffff) == 1)
                            fprintf(output, "\tDECR R%d\n", reg);
                        else if ((_right->_value & 0xffff) != 0)
                            fprintf(output, "\tSUBI #%d,R%d\n", (_right->_value & 0xffff), reg);
                    } else if (_type == C_PLUSF) {
                        if ((_right->_value & 0xffff) != 0) {
                            fprintf(output, "\tADDI #%d,R%d\n", (_right->_value & 0xffff), reg);
                            fprintf(output, "\tADCR R%d\n", reg);
                        }
                    } else if (_type == C_MINUSF) {
                        if ((_right->_value & 0xffff) != 0) {
                            fprintf(output, "\tSUBI #%d,R%d\n", (_right->_value & 0xffff), reg);
                            fprintf(output, "\tADCR R%d\n", reg);
                            fprintf(output, "\tDECR R%d\n", reg);
                        }
                    } else if (_type == C_AND) {
                        if ((_right->_value & 0xffff) != 0xffff)
                            fprintf(output, "\tANDI #%d,R%d\n", (_right->_value & 0xffff), reg);
                    } else if (_type == C_XOR) {
                        if ((_right->_value & 0xffff) != 0x0000)
                            fprintf(output, "\tXORI #%d,R%d\n", (_right->_value & 0xffff), reg);
                    } else if (_type == C_OR) {
                        if ((_right->_value & 0xffff) != 0x0000) {
                            fprintf(output, "\tANDI #%d,R%d\n", (~_right->_value & 0xffff), reg);
                            fprintf(output, "\tXORI #%d,R%d\n", (_right->_value & 0xffff), reg);
                        }
                    } else if (_type == C_EQUAL) {
                        int label = next_local++;
                        
                        fprintf(output, "\tCMPI #%d,R%d\n", (_right->_value & 0xffff), reg);
			if (decision) {
			    fprintf(output, "\tBNE " TEMP_PREFIX "%d\n", decision);
			    optimized = 1;
			} else {
			    fprintf(output, "\tMVII #-1,R%d\n", reg);
			    fprintf(output, "\tBEQ " TEMP_PREFIX "%d\n", label);
			    fprintf(output, "\tINCR R%d\n", reg);
			    fprintf(output, TEMP_PREFIX "%d:\n", label);
			}
                    } else if (_type == C_NOTEQUAL) {
                        int label = next_local++;
                        
			fprintf(output, "\tCMPI #%d,R%d\n", (_right->_value & 0xffff), reg);
			if (decision) {
			    fprintf(output, "\tBEQ " TEMP_PREFIX "%d\n", decision);
			    optimized = 1;
		        } else {
			    fprintf(output, "\tMVII #-1,R%d\n", reg);
			    fprintf(output, "\tBNE " TEMP_PREFIX "%d\n", label);
			    fprintf(output, "\tINCR R%d\n", reg);
			    fprintf(output, TEMP_PREFIX "%d:\n", label);
			}			
                    } else if (_type == C_LESS) {
                        int label = next_local++;
                        
                        fprintf(output, "\tCMPI #%d,R%d\n", (_right->_value & 0xffff), reg);
			if (decision) {
			    fprintf(output, "\tBGE " TEMP_PREFIX "%d\n", decision);
			    optimized = 1;
			} else {
			    fprintf(output, "\tMVII #-1,R%d\n", reg);
			    fprintf(output, "\tBLT " TEMP_PREFIX "%d\n", label);
			    fprintf(output, "\tINCR R%d\n", reg);
			    fprintf(output, TEMP_PREFIX "%d:\n", label);
			}
                    } else if (_type == C_LESSEQUAL) {
                        int label = next_local++;
                        
                        fprintf(output, "\tCMPI #%d,R%d\n", (_right->_value & 0xffff), reg);
			if (decision) {
			    fprintf(output, "\tBGT " TEMP_PREFIX "%d\n", decision);
			    optimized = 1;
			} else {
			    fprintf(output, "\tMVII #-1,R%d\n", reg);
			    fprintf(output, "\tBLE " TEMP_PREFIX "%d\n", label);
			    fprintf(output, "\tINCR R%d\n", reg);
			    fprintf(output, TEMP_PREFIX "%d:\n", label);
			}
                    } else if (_type == C_GREATER) {
                        int label = next_local++;
                        
                        fprintf(output, "\tCMPI #%d,R%d\n", (_right->_value & 0xffff), reg);
			if (decision) {
			    fprintf(output, "\tBLE " TEMP_PREFIX "%d\n", decision);
			    optimized = 1;
		        } else {
			    fprintf(output, "\tMVII #-1,R%d\n", reg);
			    fprintf(output, "\tBGT " TEMP_PREFIX "%d\n", label);
			    fprintf(output, "\tINCR R%d\n", reg);
			    fprintf(output, TEMP_PREFIX "%d:\n", label);
			}
                    } else if (_type == C_GREATEREQUAL) {
                        int label = next_local++;
                        
                        fprintf(output, "\tCMPI #%d,R%d\n", (_right->_value & 0xffff), reg);
		        if (decision) {
			    fprintf(output, "\tBLT " TEMP_PREFIX "%d\n", decision);
			    optimized = 1;
			} else {
			    fprintf(output, "\tMVII #-1,R%d\n", reg);
			    fprintf(output, "\tBGE " TEMP_PREFIX "%d\n", label);
			    fprintf(output, "\tINCR R%d\n", reg);
			    fprintf(output, TEMP_PREFIX "%d:\n", label);
			}
                    } else if (_type == C_MUL) {
                        if ((_right->_value & 0xffff) == 0) {
                            fprintf(output, "\tCLRR R%d\n", reg);
                        } if ((_right->_value & 0xffff) == 1) {
                            // Nothing to do
                        } if ((_right->_value & 0xffff) == 2) {
                            fprintf(output, "\tSLL R%d,1\n", reg);
                        } else if ((_right->_value & 0xffff) == 4) {
                            fprintf(output, "\tSLL R%d,2\n", reg);
                        } else if ((_right->_value & 0xffff) == 8) {
                            fprintf(output, "\tSLL R%d,2\n", reg);
                            fprintf(output, "\tSLL R%d,1\n", reg);
                        } else if ((_right->_value & 0xffff) == 16) {
                            fprintf(output, "\tSLL R%d,2\n", reg);
                            fprintf(output, "\tSLL R%d,2\n", reg);
                        } else if ((_right->_value & 0xffff) == 256) {
                            fprintf(output, "\tSWAP R%d\n", reg);
                            fprintf(output, "\tANDI #$ff00,R%d\n", reg);
                        } else {
                            int label = next_local++;
                        
                            fprintf(output, "\tMVII #%d,R4\n", ((_right->_value & 0xffff) - 1));
                            fprintf(output, "\tMOVR R%d,R5\n", reg);
                            fprintf(output, TEMP_PREFIX "%d:\n", label);
                            fprintf(output, "\tADDR R5,R%d\n", reg);
                            fprintf(output, "\tDECR R4\n");
                            fprintf(output, "\tBNE " TEMP_PREFIX "%d\n", label);
                        }
                    } else if (_type == C_DIV) {
                        if ((_right->_value & 0xffff) == 0) {
                            fprintf(stderr, "division by zero in expression\n");
                        } else if ((_right->_value & 0xffff) == 1) {
                            // Nada que hacer
                        } else if ((_right->_value & 0xffff) == 2) {
                            fprintf(output, "\tSAR R%d,1\n", reg);
                        } else if ((_right->_value & 0xffff) == 4) {
                            fprintf(output, "\tSAR R%d,2\n", reg);
                        } else if ((_right->_value & 0xffff) == 8) {
                            fprintf(output, "\tSAR R%d,2\n", reg);
                            fprintf(output, "\tSAR R%d,1\n", reg);
                        } else if ((_right->_value & 0xffff) == 16) {
                            fprintf(output, "\tSAR R%d,2\n", reg);
                            fprintf(output, "\tSAR R%d,2\n", reg);
                        } else if ((_right->_value & 0xffff) == 256) {
                            fprintf(output, "\tSWAP R%d\n", reg);
                            fprintf(output, "\tANDI #$00ff,R%d\n", reg);
                        } else {
                            int label = next_local++;
                        
                            fprintf(output, "\tMVII #-1,R4\n");
                            fprintf(output, TEMP_PREFIX "%d:\n", label);
                            fprintf(output, "\tINCR R4\n");
                            fprintf(output, "\tSUBI #%d,R%d\n", (_right->_value & 0xffff), reg);
                            fprintf(output, "\tBC " TEMP_PREFIX "%d\n", label);
                            fprintf(output, "\tMOVR R4,R%d\n", reg);
                        }
                    } else if (_type == C_MOD) {
                        if ((_right->_value & 0xffff) == 0) {
                            fprintf(stderr, "modulus by zero in expression\n");
                        } else if ((_right->_value & 0xffff) == 1) {
                            fprintf(output, "\tCLRR R%d\n", reg);
                        } else if ((_right->_value & 0xffff) == 2) {
                            fprintf(output, "\tANDI #1,R%d\n", reg);
                        } else if ((_right->_value & 0xffff) == 4) {
                            fprintf(output, "\tANDI #3,R%d\n", reg);
                        } else if ((_right->_value & 0xffff) == 8) {
                            fprintf(output, "\tANDI #7,R%d\n", reg);
                        } else if ((_right->_value & 0xffff) == 16) {
                            fprintf(output, "\tANDI #15,R%d\n", reg);
                        } else if ((_right->_value & 0xffff) == 32) {
                            fprintf(output, "\tANDI #31,R%d\n", reg);
                        } else if ((_right->_value & 0xffff) == 64) {
                            fprintf(output, "\tANDI #63,R%d\n", reg);
                        } else if ((_right->_value & 0xffff) == 128) {
                            fprintf(output, "\tANDI #127,R%d\n", reg);
                        } else if ((_right->_value & 0xffff) == 256) {
                            fprintf(output, "\tANDI #255,R%d\n", reg);
                        } else {
                            int label = next_local++;
                            
                            fprintf(output, TEMP_PREFIX "%d:\n", label);
                            fprintf(output, "\tSUBI #%d,R%d\n", (_right->_value & 0xffff), reg);
                            fprintf(output, "\tBC " TEMP_PREFIX "%d\n", label);
                            fprintf(output, "\tADDI #%d,R%d\n", (_right->_value & 0xffff), reg);
                        }
                    }
                    
                // Optimize right side when it's variable
                } else if (_right->_type == C_NAME && _type != C_ASSIGN) {
                    [_left generateReg: reg decision: 0];
                    if (_type == C_PLUS) {
                        fprintf(output, "\tADD " VAR_PREFIX "%d,R%d\n", _right->_value, reg);
                    } else if (_type == C_MINUS) {
                        fprintf(output, "\tSUB " VAR_PREFIX "%d,R%d\n", _right->_value, reg);
                    } else if (_type == C_PLUSF) {
                        fprintf(output, "\tADD " VAR_PREFIX "%d,R%d\n", _right->_value, reg);
                        fprintf(output, "\tADCR R%d\n", reg);
                    } else if (_type == C_MINUSF) {
                        fprintf(output, "\tSUB " VAR_PREFIX "%d,R%d\n", _right->_value, reg);
                        fprintf(output, "\tADCR R%d\n", reg);
                        fprintf(output, "\tDECR R%d\n", reg);
                    } else if (_type == C_AND) {
                        fprintf(output, "\tAND " VAR_PREFIX "%d,R%d\n", _right->_value, reg);
                    } else if (_type == C_XOR) {
                        fprintf(output, "\tXOR " VAR_PREFIX "%d,R%d\n", _right->_value, reg);
                    } else if (_type == C_OR) {
                        fprintf(output, "\tMVI " VAR_PREFIX "%d,R4\n", _right->_value);
                        fprintf(output, "\tCOMR R4\n");
                        fprintf(output, "\tANDR R4,R%d\n", reg);
                        fprintf(output, "\tXOR " VAR_PREFIX "%d,R%d\n", _right->_value, reg);
                    } else if (_type == C_EQUAL) {
                        int label = next_local++;
                        
                        fprintf(output, "\tCMP " VAR_PREFIX "%d,R%d\n", _right->_value, reg);
			if (decision) {
			    fprintf(output, "\tBNE " TEMP_PREFIX "%d\n", decision);
			    optimized = 1;
			} else {
			    fprintf(output, "\tMVII #-1,R%d\n", reg);
			    fprintf(output, "\tBEQ " TEMP_PREFIX "%d\n", label);
			    fprintf(output, "\tINCR R%d\n", reg);
			    fprintf(output, TEMP_PREFIX "%d:\n", label);
			}
                    } else if (_type == C_NOTEQUAL) {
                        int label = next_local++;
                        
                        fprintf(output, "\tCMP " VAR_PREFIX "%d,R%d\n", _right->_value, reg);
   			if (decision) {
			    fprintf(output, "\tBEQ " TEMP_PREFIX "%d\n", decision);
			    optimized = 1;
		        } else {
			    fprintf(output, "\tMVII #-1,R%d\n", reg);
			    fprintf(output, "\tBNE " TEMP_PREFIX "%d\n", label);
			    fprintf(output, "\tINCR R%d\n", reg);
			    fprintf(output, TEMP_PREFIX "%d:\n", label);
			}
                    } else if (_type == C_LESS) {
                        int label = next_local++;
                        
                        fprintf(output, "\tCMP " VAR_PREFIX "%d,R%d\n", _right->_value, reg);
			if (decision) {
			    fprintf(output, "\tBGE " TEMP_PREFIX "%d\n", decision);
			    optimized = 1;
			} else {
			    fprintf(output, "\tMVII #-1,R%d\n", reg);
			    fprintf(output, "\tBLT " TEMP_PREFIX "%d\n", label);
			    fprintf(output, "\tINCR R%d\n", reg);
			    fprintf(output, TEMP_PREFIX "%d:\n", label);
			}
                    } else if (_type == C_LESSEQUAL) {
                        int label = next_local++;
                        
                        fprintf(output, "\tCMP " VAR_PREFIX "%d,R%d\n", _right->_value, reg);
			if (decision) {
			    fprintf(output, "\tBGT " TEMP_PREFIX "%d\n", decision);
			    optimized = 1;
			} else {
			    fprintf(output, "\tMVII #-1,R%d\n", reg);
			    fprintf(output, "\tBLE " TEMP_PREFIX "%d\n", label);
			    fprintf(output, "\tINCR R%d\n", reg);
			    fprintf(output, TEMP_PREFIX "%d:\n", label);
			}
                    } else if (_type == C_GREATER) {
                        int label = next_local++;
                        
                        fprintf(output, "\tCMP " VAR_PREFIX "%d,R%d\n", _right->_value, reg);
			if (decision) {
			    fprintf(output, "\tBLE " TEMP_PREFIX "%d\n", decision);
			    optimized = 1;
			} else {
			    fprintf(output, "\tMVII #-1,R%d\n", reg);
			    fprintf(output, "\tBGT " TEMP_PREFIX "%d\n", label);
			    fprintf(output, "\tINCR R%d\n", reg);
			    fprintf(output, TEMP_PREFIX "%d:\n", label);
			}
                    } else if (_type == C_GREATEREQUAL) {
                        int label = next_local++;
                        
                        fprintf(output, "\tCMP " VAR_PREFIX "%d,R%d\n", _right->_value, reg);
			if (decision) {
			    fprintf(output, "\tBLT " TEMP_PREFIX "%d\n", decision);
			    optimized = 1;
			} else {
			    fprintf(output, "\tMVII #-1,R%d\n", reg);
			    fprintf(output, "\tBGE " TEMP_PREFIX "%d\n", label);
			    fprintf(output, "\tINCR R%d\n", reg);
			    fprintf(output, TEMP_PREFIX "%d:\n", label);
			}
                    } else if (_type == C_MUL) {
                        int label = next_local++;
                        int label2 = next_local++;
                        
                        fprintf(output, "\tMVI " VAR_PREFIX "%d,R4\n", _right->_value);
                        fprintf(output, "\tMOVR R%d,R5\n", reg);
                        fprintf(output, "\tCLRR R%d\n", reg);
                        fprintf(output, "\tTSTR R4\n");
                        fprintf(output, "\tBEQ " TEMP_PREFIX "%d\n", label);
                        fprintf(output, TEMP_PREFIX "%d:\n", label2);
                        fprintf(output, "\tADDR R5,R%d\n", reg);
                        fprintf(output, "\tDECR R4\n");
                        fprintf(output, "\tBNE " TEMP_PREFIX "%d\n", label2);
                        fprintf(output, TEMP_PREFIX "%d:\n", label);
                    } else if (_type == C_DIV) {
                        int label = next_local++;
                        int label2 = next_local++;
                        
                        fprintf(output, "\tMVI " VAR_PREFIX "%d,R4\n", _right->_value);
                        fprintf(output, "\tMOVR R%d,R5\n", reg);
                        fprintf(output, "\tTSTR R4\n");
                        fprintf(output, "\tBEQ " TEMP_PREFIX "%d\n", label);
                        fprintf(output, "\tMVII #-1,R%d\n", reg);
                        fprintf(output, TEMP_PREFIX "%d:\n", label2);
                        fprintf(output, "\tINCR R%d\n", reg);
                        fprintf(output, "\tSUBR R4,R5\n");
                        fprintf(output, "\tBC " TEMP_PREFIX "%d\n", label2);
                        fprintf(output, TEMP_PREFIX "%d:\n", label);
                    } else if (_type == C_MOD) {
                        int label = next_local++;
                        int label2 = next_local++;
                        
                        fprintf(output, "\tMVI " VAR_PREFIX "%d,R4\n", _right->_value);
                        fprintf(output, "\tTSTR R4\n");
                        fprintf(output, "\tBEQ " TEMP_PREFIX "%d\n", label);
                        fprintf(output, TEMP_PREFIX "%d:\n", label2);
                        fprintf(output, "\tSUBR R4,R%d\n", reg);
                        fprintf(output, "\tBC " TEMP_PREFIX "%d\n", label2);
                        fprintf(output, "\tADDR R4,R%d\n", reg);
                        fprintf(output, TEMP_PREFIX "%d:\n", label);
                    }
                } else if (_type == C_ASSIGN && _right->_type == C_PLUS
                           && _right->_left->_type == C_NAME_RO 
                           && _right->_right->_type == C_NUM) {
                    [_left generateReg: reg decision: 0];
                    fprintf(output, "\tMVO R%d," LABEL_PREFIX "%d+%d\n", reg, _right->_left->_value, _right->_right->_value);
                } else {
                    
                    /* Common tree generation */
                    if (_left->_regs > 4 - reg && _right->_regs > 4 - reg) {
                        [_right generateReg: reg decision: 0];
                        fprintf(output, "\tPSHR R%d\n", reg);
                        [_left generateReg: reg decision: 0];
                        fprintf(output, "\tPULR R%d\n", reg + 1);
                    } else if (_left->_regs >= _right->_regs) {
                        [_left generateReg: reg decision: 0];
                        [_right generateReg: reg + 1 decision: 0];
                    } else {
                        [_right generateReg: reg decision: 0];
			[_left generateReg: reg + 1 decision: 0];
                        if (_type != C_PLUS && _type != C_MUL && _type != C_AND
                         && _type != C_OR && _type != C_XOR) { /* Interchange registers */
                            fprintf(output, "\tXORR R%d,R%d\n", reg, reg + 1);
                            fprintf(output, "\tXORR R%d,R%d\n", reg + 1, reg);
                            fprintf(output, "\tXORR R%d,R%d\n", reg, reg + 1);
                        }
                    }
                    if (_type == C_PLUS) {
                        fprintf(output, "\tADDR R%d,R%d\n", reg + 1, reg);
                    } else if (_type == C_MINUS) {
                        fprintf(output, "\tSUBR R%d,R%d\n", reg + 1, reg);
                    } else if (_type == C_PLUSF) {
                        fprintf(output, "\tADDR R%d,R%d\n", reg + 1, reg);
                        fprintf(output, "\tADCR R%d\n", reg);
                    } else if (_type == C_MINUSF) {
                        fprintf(output, "\tSUBR R%d,R%d\n", reg + 1, reg);
                        fprintf(output, "\tADCR R%d\n", reg);
                        fprintf(output, "\tDECR R%d\n", reg);
                    } else if (_type == C_AND) {
                        fprintf(output, "\tANDR R%d,R%d\n", reg + 1, reg);
                    } else if (_type == C_XOR) {
                        fprintf(output, "\tXORR R%d,R%d\n", reg + 1, reg);
                    } else if (_type == C_OR) {
                        fprintf(output, "\tCOMR R%d\n", reg + 1);
                        fprintf(output, "\tANDR R%d,R%d\n", reg + 1, reg);
                        fprintf(output, "\tCOMR R%d\n", reg + 1);
                        fprintf(output, "\tXORR R%d,R%d\n", reg + 1, reg);
                    } else if (_type == C_EQUAL) {
                        int label = next_local++;
                        
                        fprintf(output, "\tCMPR R%d,R%d\n", reg + 1, reg);
			if (decision) {
			    fprintf(output, "\tBNE " TEMP_PREFIX "%d\n", decision);
			    optimized = 1;
			} else {
			    fprintf(output, "\tMVII #-1,R%d\n", reg);
			    fprintf(output, "\tBEQ " TEMP_PREFIX "%d\n", label);
			    fprintf(output, "\tINCR R%d\n", reg);
			    fprintf(output, TEMP_PREFIX "%d:\n", label);
			}
                    } else if (_type == C_NOTEQUAL) {
                        int label = next_local++;
                        
                        fprintf(output, "\tCMPR R%d,R%d\n", reg + 1, reg);
			if (decision) {
			    fprintf(output, "\tBEQ " TEMP_PREFIX "%d\n", decision);
			    optimized = 1;
			} else {
			    fprintf(output, "\tMVII #-1,R%d\n", reg);
			    fprintf(output, "\tBNE " TEMP_PREFIX "%d\n", label);
			    fprintf(output, "\tINCR R%d\n", reg);
			    fprintf(output, TEMP_PREFIX "%d:\n", label);
			}
                    } else if (_type == C_LESS) {
                        int label = next_local++;
                        
                        fprintf(output, "\tCMPR R%d,R%d\n", reg + 1, reg);
			if (decision) {
			    fprintf(output, "\tBGE " TEMP_PREFIX "%d\n", decision);
			    optimized = 1;
			} else {
			    fprintf(output, "\tMVII #-1,R%d\n", reg);
			    fprintf(output, "\tBLT " TEMP_PREFIX "%d\n", label);
			    fprintf(output, "\tINCR R%d\n", reg);
			    fprintf(output, TEMP_PREFIX "%d:\n", label);
			}
                    } else if (_type == C_LESSEQUAL) {
                        int label = next_local++;
                        
                        fprintf(output, "\tCMPR R%d,R%d\n", reg + 1, reg);
			if (decision) {
			    fprintf(output, "\tBGT " TEMP_PREFIX "%d\n", decision);
			    optimized = 1;
			} else {
	                    fprintf(output, "\tMVII #-1,R%d\n", reg);
			    fprintf(output, "\tBLE " TEMP_PREFIX "%d\n", label);
			    fprintf(output, "\tINCR R%d\n", reg);
			    fprintf(output, TEMP_PREFIX "%d:\n", label);
			}
                    } else if (_type == C_GREATER) {
                        int label = next_local++;
                        
                        fprintf(output, "\tCMPR R%d,R%d\n", reg + 1, reg);
			if (decision) {
			    fprintf(output, "\tBLE " TEMP_PREFIX "%d\n", decision);
			    optimized = 1;
			} else {
			    fprintf(output, "\tMVII #-1,R%d\n", reg);
			    fprintf(output, "\tBGT " TEMP_PREFIX "%d\n", label);
			    fprintf(output, "\tINCR R%d\n", reg);
			    fprintf(output, TEMP_PREFIX "%d:\n", label);
			}
                    } else if (_type == C_GREATEREQUAL) {
                        int label = next_local++;
                        
                        fprintf(output, "\tCMPR R%d,R%d\n", reg + 1, reg);
			if (decision) {
			    fprintf(output, "\tBLT " TEMP_PREFIX "%d\n", decision);
			    optimized = 1;
			} else {
			    fprintf(output, "\tMVII #-1,R%d\n", reg);
			    fprintf(output, "\tBGE " TEMP_PREFIX "%d\n", label);
			    fprintf(output, "\tINCR R%d\n", reg);
			    fprintf(output, TEMP_PREFIX "%d:\n", label);
			}
                    } else if (_type == C_MUL) {
                        int label = next_local++;
                        int label2 = next_local++;
                        
                        fprintf(output, "\tMOVR R%d,R4\n", reg + 1);
                        fprintf(output, "\tMOVR R%d,R5\n", reg);
                        fprintf(output, "\tCLRR R%d\n", reg);
                        fprintf(output, "\tTSTR R4\n");
                        fprintf(output, "\tBEQ " TEMP_PREFIX "%d\n", label);
                        fprintf(output, TEMP_PREFIX "%d:\n", label2);
                        fprintf(output, "\tADDR R5,R%d\n", reg);
                        fprintf(output, "\tDECR R4\n");
                        fprintf(output, "\tBNE " TEMP_PREFIX "%d\n", label2);
                        fprintf(output, TEMP_PREFIX "%d:\n", label);
                    } else if (_type == C_DIV) {
                        int label = next_local++;
                        int label2 = next_local++;
                        
                        fprintf(output, "\tMOVR R%d,R5\n", reg);
                        fprintf(output, "\tTSTR R%d\n", reg + 1);
                        fprintf(output, "\tBEQ " TEMP_PREFIX "%d\n", label);
                        fprintf(output, "\tMVII #-1,R%d\n", reg);
                        fprintf(output, TEMP_PREFIX "%d:\n", label2);
                        fprintf(output, "\tINCR R%d\n", reg);
                        fprintf(output, "\tSUBR R%d,R5\n", reg + 1);
                        fprintf(output, "\tBC " TEMP_PREFIX "%d\n", label2);
                        fprintf(output, TEMP_PREFIX "%d:\n", label);
                    } else if (_type == C_MOD) {
                        int label = next_local++;
                        int label2 = next_local++;
                        
                        fprintf(output, "\tTSTR R%d\n", reg + 1);
                        fprintf(output, "\tBEQ " TEMP_PREFIX "%d\n", label);
                        fprintf(output, TEMP_PREFIX "%d:\n", label2);
                        fprintf(output, "\tSUBR R%d,R%d\n", reg + 1, reg);
                        fprintf(output, "\tBC " TEMP_PREFIX "%d\n", label2);
                        fprintf(output, "\tADDR R%d,R%d\n", reg + 1, reg);
                        fprintf(output, TEMP_PREFIX "%d:\n", label);
                    } else if (_type == C_ASSIGN) {
                        fprintf(output, "\tMVO@ R%d,R%d\n", reg, reg + 1);
                    }
                }
                break;
        }
    }
@end

/*
** Representation for a loop
*/
@interface loop : NSObject 
@property (assign) node *step;
@property (assign) node *final;
@property (assign) NSString *var;
@property int label;
@end

@implementation loop 
@end

/*
** The compiler
*/
@interface compiler : NSObject {
}
@end

@implementation compiler {
    FILE *input;
    FILE *included;
    FILE *included2;
    int saved_line_number;
    int active_include;
    int next_include;
    FILE *include[50];
    NSMutableString *line;
    int line_start;
    NSMutableString *name;
    NSMutableString *assigned;
    int pstring[256];
    int offset;
    int value;
    size_t line_pos;
    size_t line_size;
    NSMutableDictionary *constants;
    NSMutableDictionary *labels;
    NSMutableDictionary *variables;
    NSMutableDictionary *arrays;
    NSMutableArray *loops;
    int line_number;
    int next_label;
    int next_var;
    enum lexical_component lex;
    int bitmap_value;
    int bitmap_byte;
    int last_is_return; // Indicates if last statement processed was a RETURN
    int scroll_used;    // Indicates if scroll used
    int keypad_used;    // Indicates if keypad used
    int music_used;     // Indicates if music used
    int stack_used;     // Indicates if stack check used
}

    /*
    ** Avoid spaces
    */
-   (void) skip_spaces {
        while (line_pos < line_size && isspace([line characterAtIndex:line_pos]))
            line_pos++;
    }
    
    /*
    ** Sneak-peek to next character
    */
-   (unichar) sneak_peek {
        [self skip_spaces];
        if (line_pos == line_size)
            return '\0';
        return [line characterAtIndex:line_pos];
    }

    /*
    ** Gets another lexical component
    */
-   (void) get_lex {
	unichar current;

        [self skip_spaces];
        if (line_pos == line_size) {
            lex = C_END;
            return;
        }
        current = [line characterAtIndex:line_pos];
	if (isalpha(current) || current == '#') {  // Name or label
            name = [NSMutableString string];
            [name appendString:[[NSString stringWithCharacters:&current length:1] uppercaseString]];
	    line_pos++;
            while (line_pos < line_size) {
                current = [line characterAtIndex:line_pos];
                if (!isalnum(current) && current != '_' && current != '#')
                    break;
                [name appendString:[[NSString stringWithCharacters:&current length:1] uppercaseString]];
                line_pos++;
            }
            if (line_pos < line_size && [line characterAtIndex:line_pos] == ':' && line_start) {
                lex = C_LABEL;
                line_pos++;
            } else {
                lex = C_NAME;
            }
	    line_start = 0;
            return;
        }
        if (isdigit(current)) {  // Decimal number
            int fraction;
            
            value = 0;
            while (line_pos < line_size && isdigit([line characterAtIndex:line_pos]))
                value = (value * 10) + [line characterAtIndex:line_pos++] - '0';
            if (value > 65535)
                fprintf(stderr, "Warning: Number exceeds 16 bits in line %d\n", line_number);
            if (line_pos < line_size && [line characterAtIndex:line_pos] == '.'
                && line_pos + 1 < line_size && isdigit([line characterAtIndex:line_pos + 1])) {
                if (value > 255)
                    fprintf(stderr, "Warning: Fixed number exceeds basic 8 bits in line %d\n", line_number);
                line_pos++;
                fraction = 0;
                if (line_pos < line_size && isdigit([line characterAtIndex:line_pos]))
                    fraction += ([line characterAtIndex:line_pos++] - '0') * 100;
                if (line_pos < line_size && isdigit([line characterAtIndex:line_pos]))
                    fraction += ([line characterAtIndex:line_pos++] - '0') * 10;
                if (line_pos < line_size && isdigit([line characterAtIndex:line_pos]))
                    fraction += ([line characterAtIndex:line_pos++] - '0');
                while (line_pos < line_size && isdigit([line characterAtIndex:line_pos]))
                    line_pos++;
                value += (int) (fraction * (256.0 / 1000.0) + 0.5) * 256;
            }
            lex = C_NUM;
	    line_start = 0;
            return;
        }
        if (current == '$' && line_pos + 1 < line_size
         && isxdigit([line characterAtIndex:line_pos + 1])) {  // Hexadecimal number
            value = 0;
            line_pos++;
            while (line_pos < line_size && isxdigit([line characterAtIndex:line_pos])) {
                int temp;
                
                temp = toupper([line characterAtIndex:line_pos]) - '0';
                if (temp > 9)
                    temp -= 7;
                value = (value << 4) | temp;
                line_pos++;
            }
            lex = C_NUM;
	    line_start = 0;
            return;
        }
        if (current == '&' && line_pos + 1 < line_size
         && ([line characterAtIndex:line_pos + 1] == '0' 
          || [line characterAtIndex:line_pos + 1] == '1')) {  // Binary number
            value = 0;
            line_pos++;
            while (line_pos < line_size 
                && ([line characterAtIndex:line_pos] == '0' 
                 || [line characterAtIndex:line_pos] == '1')) {
                value = (value << 1) | ([line characterAtIndex:line_pos] & 1);
                line_pos++;
            }
            lex = C_NUM;
	    line_start = 0;
            return;
        }
        if ([line characterAtIndex:line_pos] == '"') {  // String
            line_pos++;
            offset = 0;
            while (line_pos < line_size && [line characterAtIndex:line_pos] != '"') {
                if (offset == 255) {
                    [self emitError:@"string too long"];
                    do {
                        line_pos++;
                    } while (line_pos < line_size && [line characterAtIndex:line_pos] != '"') ;
                    break;
                }
                if ([line characterAtIndex:line_pos] == '\\') {
                    int c;
                    
                    line_pos++;
                    if (line_pos < line_size && [line characterAtIndex:line_pos] == '"') {
                        c = [line characterAtIndex:line_pos] - 32;
                        if (c < 0)
                            c = 0;
                        line_pos++;
                    } else {
                        c = 0;
                        while (line_pos < line_size && isdigit([line characterAtIndex:line_pos])) {
                            c = c * 10 + ([line characterAtIndex:line_pos] - '0');
                            line_pos++;
                        }
                    }
                    pstring[offset++] = c;
                } else {
                    int c;
                    
                    c = [line characterAtIndex:line_pos] - 32;
                    if (c < 0)
                        c = 0;
                    line_pos++;
                    pstring[offset++] = c;
                }
            }
            if (line_pos < line_size && [line characterAtIndex:line_pos] == '"') {
                line_pos++;
            } else {
                [self emitError:@"unfinished string"];
            }
            lex = C_STRING;
	    line_start = 0;
            return;
        }
   	line_start = 0;
        switch (current) {
            case '=':
                line_pos++;
                lex = C_EQUAL;
                break;
            case '+':
                line_pos++;
                if (line_pos < line_size && [line characterAtIndex:line_pos] == '.') {
                    lex = C_PLUSF;
                    line_pos++;
                } else {
                    lex = C_PLUS;
                }
                break;
            case '-':
                line_pos++;
                if (line_pos < line_size && [line characterAtIndex:line_pos] == '.') {
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
                if ([line characterAtIndex:line_pos] == '=') {
                    line_pos++;
                    lex = C_LESSEQUAL;
                } else if ([line characterAtIndex:line_pos] == '>') {
                    line_pos++;
                    lex = C_NOTEQUAL;
                }
                break;
            case '>':
                line_pos++;
                lex = C_GREATER;
                if ([line characterAtIndex:line_pos] == '=') {
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
    
    /*
    ** Evaluates an expression
    ** Result in R0
    */
-   (void) evalExprReg: (int) reg decision: (int) decision
    {
        node *tree;
        
        tree = [self evalLevel0];
        [tree label];
	optimized = 0;
        [tree generateReg: reg decision: decision];
        tree = NULL;
    }
    
    /*
    ** Expression evaluation: Level 0 (OR)
    */
-   (node *) evalLevel0
    {
        node *left;
        node *right;
        
        left = [self evalLevel1];
        while (1) {
            if (lex == C_NAME && [name isEqualToString:@"OR"]) {
                [self get_lex];
                right = [self evalLevel1];
                left = [[node alloc] initWithType: C_OR value: 0 left: left right: right];
            } else {
                break;
            }
        }
        return left;
    }
    
    /*
    ** Expression evaluation: Level 1 (XOR)
    */
-   (node *) evalLevel1
    {
        node *left;
        node *right;
        
        left = [self evalLevel2];
        while (1) {
            if (lex == C_NAME && [name isEqualToString:@"XOR"]) {
                [self get_lex];
                right = [self evalLevel2];
                left = [[node alloc] initWithType: C_XOR value: 0 left: left right: right];
            } else {
                break;
            }
        }
        return left;
    }
    
    /*
    ** Expression evaluation: Level 2 (AND)
    */
-   (node *) evalLevel2
    {
        node *left;
        node *right;
        
        left = [self evalLevel3];
        while (1) {
            if (lex == C_NAME && [name isEqualToString:@"AND"]) {
                [self get_lex];
                right = [self evalLevel3];
                left = [[node alloc] initWithType: C_AND value: 0 left: left right: right];
            } else {
                break;
            }
        }
        return left;
    }

    /*
    ** Expression evaluation: Level 3 (= <> < <= > >=)
    */
-   (node *) evalLevel3
    {
        node *left;
        node *right;
        
        left = [self evalLevel4];
        while (1) {
            if (lex == C_EQUAL) {
                [self get_lex];
                right = [self evalLevel4];
                left = [[node alloc] initWithType: C_EQUAL value: 0 left: left right: right];
            } else if (lex == C_NOTEQUAL) {
                [self get_lex];
                right = [self evalLevel4];
                left = [[node alloc] initWithType: C_NOTEQUAL value: 0 left: left right: right];
            } else if (lex == C_LESS) {
                [self get_lex];
                right = [self evalLevel4];
                left = [[node alloc] initWithType: C_LESS value: 0 left: left right: right];
            } else if (lex == C_LESSEQUAL) {
                [self get_lex];
                right = [self evalLevel4];
                left = [[node alloc] initWithType: C_LESSEQUAL value: 0 left: left right: right];
            } else if (lex == C_GREATER) {
                [self get_lex];
                right = [self evalLevel4];
                left = [[node alloc] initWithType: C_GREATER value: 0 left: left right: right];
            } else if (lex == C_GREATEREQUAL) {
                [self get_lex];
                right = [self evalLevel4];
                left = [[node alloc] initWithType: C_GREATEREQUAL value: 0 left: left right: right];
            } else {
                break;
            }
        }
        return left;
    }
    
    /*
    ** Expression evaluation: Level 4 (+ -)
    */
-   (node *) evalLevel4
    {
        node *left;
        node *right;
        
        left = [self evalLevel5];
        while (1) {
            if (lex == C_PLUS) {
                [self get_lex];
                right = [self evalLevel5];
                left = [[node alloc] initWithType: C_PLUS value: 0 left: left right: right];
            } else if (lex == C_MINUS) {
                [self get_lex];
                right = [self evalLevel5];
                left = [[node alloc] initWithType: C_MINUS value: 0 left: left right: right];
            } else if (lex == C_PLUSF) {
                [self get_lex];
                right = [self evalLevel5];
                left = [[node alloc] initWithType: C_PLUSF value: 0 left: left right: right];
            } else if (lex == C_MINUSF) {
                [self get_lex];
                right = [self evalLevel5];
                left = [[node alloc] initWithType: C_MINUSF value: 0 left: left right: right];
            } else {
                break;
            }
        }
        return left;
    }

    /*
    ** Expression evaluation: Level 5 (* / %)
    */
-   (node *) evalLevel5
    {
        node *left;
        node *right;
        
        left = [self evalLevel6];
        while (1) {
            if (lex == C_MUL) {
                [self get_lex];
                right = [self evalLevel6];
                left = [[node alloc] initWithType: C_MUL value: 0 left: left right: right];
            } else if (lex == C_DIV) {
                [self get_lex];
                right = [self evalLevel6];
                left = [[node alloc] initWithType: C_DIV value: 0 left: left right: right];
            } else if (lex == C_MOD) {
                [self get_lex];
                right = [self evalLevel6];
                left = [[node alloc] initWithType: C_MOD value: 0 left: left right: right];
            } else {
                break;
            }
        }
        return left;
    }
    
    /*
    ** Expression evaluation: Level 6 (NOT)
    */
-   (node *) evalLevel6
    {
        node *left;
        
        if (lex == C_MINUS) {
            [self get_lex];
            left = [self evalLevel7];
            left = [[node alloc] initWithType: C_NEG value: 0 left: left right: NULL];
        } else if (lex == C_NAME && [name isEqualToString:@"NOT"]) {
            [self get_lex];
            left = [self evalLevel7];
            left = [[node alloc] initWithType: C_NOT value: 0 left: left right: NULL];
        } else {
            left = [self evalLevel7];
        }
        return left;
    }
    
    /*
    ** Expression evaluation: Level 7 (parenthesis, functions, variables and values)
    */
-   (node *) evalLevel7
    {
        NSNumber *number;

        if (lex == C_LPAREN) {
            node *tree;
            
            [self get_lex];
            tree = [self evalLevel0];
            if (lex != C_RPAREN)
                [self emitError:@"missing right parenthesis"];
            else
                [self get_lex];
            return tree;
        }
        if (lex == C_STRING) {
            int temp;
            
            if (offset == 0) {
                [self emitError:@"empty string"];
                temp = 0;
            } else {
                temp = pstring[0];
            }
            [self get_lex];
            return [[node alloc] initWithType: C_NUM value: temp left: NULL right: NULL];
        }
        if (lex == C_NAME) {
            int temp;
            
            if ([name isEqualToString:@"PEEK"]) {
                node *tree;
                
                [self get_lex];
                if (lex != C_LPAREN)
                    [self emitError:@"missing left parenthesis in PEEK"];
                else
                    [self get_lex];
                tree = [self evalLevel0];
                if (lex != C_RPAREN)
                    [self emitError:@"missing right parenthesis in PEEK"];
                else
                    [self get_lex];
                return [[node alloc] initWithType: C_PEEK value: 0 left: tree right: NULL];
            } else if ([name isEqualToString:@"CONT2"] 
                    || [name isEqualToString:@"CONT1"]) {
                node *tree;
                int c;
                
                if ([name isEqualToString:@"CONT2"])
                    tree = [[node alloc] initWithType: C_NUM value: c = 0x01fe left: NULL right: NULL];
                else
                    tree = [[node alloc] initWithType: C_NUM value: c = 0x01ff left: NULL right: NULL];
                tree = [[node alloc] initWithType: C_PEEK value: 0 left: tree right: NULL];
                tree = [[node alloc] initWithType: C_NOT value: 0 left: tree right: NULL];
                [self get_lex];
                if (lex == C_PERIOD) {
                    [self get_lex];
                    if (lex != C_NAME)
                        [self emitError:@"missing name for CONT? syntax"];
                    else if ([name isEqualToString:@"DOWN"])
                        tree = [[node alloc] initWithType: C_AND value: 0 left: tree right: [[node alloc] initWithType: C_NUM value: 0x01 left: NULL right: NULL]];
                    else if ([name isEqualToString:@"RIGHT"])
                        tree = [[node alloc] initWithType: C_AND value: 0 left: tree right: [[node alloc] initWithType: C_NUM value: 0x02 left: NULL right: NULL]];
                    else if ([name isEqualToString:@"UP"])
                        tree = [[node alloc] initWithType: C_AND value: 0 left: tree right: [[node alloc] initWithType: C_NUM value: 0x04 left: NULL right: NULL]];
                    else if ([name isEqualToString:@"LEFT"])
                        tree = [[node alloc] initWithType: C_AND value: 0 left: tree right: [[node alloc] initWithType: C_NUM value: 0x08 left: NULL right: NULL]];
                    else if ([name isEqualToString:@"BUTTON"])
                        tree = [[node alloc] initWithType: C_AND value: 0 left: tree right: [[node alloc] initWithType: C_NUM value: 0xe0 left: NULL right: NULL]];
                    else if ([name isEqualToString:@"B0"])
                        tree = [[node alloc] initWithType: C_EQUAL value: 0 left:
                                        [[node alloc] initWithType: C_AND value: 0 left: tree right: [[node alloc] initWithType: C_NUM value: 0xe0 left: NULL right: NULL]] right:
                                        [[node alloc] initWithType: C_NUM value: 0xa0 left: NULL right: NULL]];
                    else if ([name isEqualToString:@"B1"])
                        tree = [[node alloc] initWithType: C_EQUAL value: 0 left:
                                        [[node alloc] initWithType: C_AND value: 0 left: tree right: [[node alloc] initWithType: C_NUM value: 0xe0 left: NULL right: NULL]] right:
                                        [[node alloc] initWithType: C_NUM value: 0x60 left: NULL right: NULL]];
                    else if ([name isEqualToString:@"B2"])
                        tree = [[node alloc] initWithType: C_EQUAL value: 0 left:
                                        [[node alloc] initWithType: C_AND value: 0 left: tree right: [[node alloc] initWithType: C_NUM value: 0xe0 left: NULL right: NULL]] right:
                                        [[node alloc] initWithType: C_NUM value: 0xc0 left: NULL right: NULL]];
                    else if ([name isEqualToString:@"KEY"]) {
                        tree = [[node alloc] initWithType: C_VAR value: (c == 0x01ff) ? 10 : 11 left: NULL right: NULL];
                        keypad_used = 1;
                    } else
                        [self emitError:@"wrong name for CONT? syntax"];
                    [self get_lex];
                }
                return tree;
            } else if ([name isEqualToString:@"COL0"]) {
                [self get_lex];
                return [[node alloc] initWithType: C_VAR value: 0 left: NULL right: NULL];
            } else if ([name isEqualToString:@"COL1"]) {
                [self get_lex];
                return [[node alloc] initWithType: C_VAR value: 1 left: NULL right: NULL];
            } else if ([name isEqualToString:@"COL2"]) {
                [self get_lex];
                return [[node alloc] initWithType: C_VAR value: 2 left: NULL right: NULL];
            } else if ([name isEqualToString:@"COL3"]) {
                [self get_lex];
                return [[node alloc] initWithType: C_VAR value: 3 left: NULL right: NULL];
            } else if ([name isEqualToString:@"COL4"]) {
                [self get_lex];
                return [[node alloc] initWithType: C_VAR value: 4 left: NULL right: NULL];
            } else if ([name isEqualToString:@"COL5"]) {
                [self get_lex];
                return [[node alloc] initWithType: C_VAR value: 5 left: NULL right: NULL];
            } else if ([name isEqualToString:@"COL6"]) {
                [self get_lex];
                return [[node alloc] initWithType: C_VAR value: 6 left: NULL right: NULL];
            } else if ([name isEqualToString:@"COL7"]) {
                [self get_lex];
                return [[node alloc] initWithType: C_VAR value: 7 left: NULL right: NULL];
            } else if ([name isEqualToString:@"FRAME"]) {
                [self get_lex];
                return [[node alloc] initWithType: C_VAR value: 8 left: NULL right: NULL];
            } else if ([name isEqualToString:@"RAND"]) {
                [self get_lex];
                return [[node alloc] initWithType: C_VAR value: 9 left: NULL right: NULL];
            } else if ([name isEqualToString:@"NTSC"]) {
                [self get_lex];
                return [[node alloc] initWithType: C_VAR value: 12 left: NULL right: NULL];
            }
            if ([self sneak_peek] == '(') {  // Indexed access
                node *tree;
                
                if ((number = [arrays objectForKey:name]) != nil) {
                    temp = [number intValue] >> 16;
                } else if ((number = [labels objectForKey:name]) != nil) {
                    temp = [number intValue];
                } else {
                    [labels setObject:[NSNumber numberWithInt: temp = next_label++] forKey:name];
                }
                [self get_lex];
                if (lex != C_LPAREN)
                    [self emitError:@"missing left parenthesis in array access"];
                else
                    [self get_lex];
                tree = [self evalLevel0];
                if (lex != C_RPAREN)
                    [self emitError:@"missing right parenthesis in array access"];
                else
                    [self get_lex];
                return [[node alloc] initWithType: C_PEEK value: 0 left:
                                [[node alloc] initWithType: C_PLUS value: 0 left:
                                         [[node alloc] initWithType: C_NAME_RO value: temp left: NULL right: NULL] right: tree] right: NULL];
            }
            number = [constants objectForKey:name];
            if (number != nil) {
                temp = [number intValue] & 0xffff;
                [self get_lex];
                return [[node alloc] initWithType: C_NUM value: temp left: NULL right: NULL];
            }
            number = [variables objectForKey:name];
            if (number == nil)
                [variables setObject:[NSNumber numberWithInt:next_var++] forKey:name];
            temp = [(NSNumber *) [variables objectForKey:name] intValue];
            [self get_lex];
            return [[node alloc] initWithType: C_NAME value: temp left: NULL right: NULL];
        }
        if (lex == C_NUM) {
            int temp;
            
            temp = value;
            [self get_lex];
            return [[node alloc] initWithType: C_NUM value: temp left: NULL right: NULL];
        }
        [self emitError:@"bad syntax for expression"];
        return [[node alloc] initWithType: C_NUM value: 0 left: NULL right: NULL];
    }
    
    /*
    ** Generates an error message
    */
-   (void) emitError: (NSString *) message
    {
        fprintf(stderr, "Error: %s in line %d\n", [message UTF8String], line_number);
    }

    /*
    ** Assignment processing
    */
-   (void) compileAssignment
    {
        if (lex != C_NAME) {
            [self emitError:@"name required for assignment"];
            return;
        }
        if ([self sneak_peek] == '(') {
            node *tree;
            node *tree2;
            int temp;
            
            if ([arrays objectForKey:name] == nil) {
                [self emitError:@"using array without previous DIM, autoassigning DIM(10)"];
                [arrays setObject:[NSNumber numberWithInt: 10 | (next_label++ << 16)] forKey:name];
            }
            temp = [(NSNumber *) [arrays objectForKey:name] intValue] >> 16;
            [self get_lex];
            if (lex != C_LPAREN)
                [self emitError:@"missing left parenthesis in array access"];
            else
                [self get_lex];
            tree = [self evalLevel0];
            if (lex != C_RPAREN)
                [self emitError:@"missing right parenthesis in array access"];
            else
                [self get_lex];
            if (lex != C_EQUAL)
                [self emitError:@"required '=' for assignment"];
            else
                [self get_lex];
            tree2 = [self evalLevel0];
            tree = [[node alloc] initWithType: C_ASSIGN value: 0 left: tree2 right:
                            [[node alloc] initWithType: C_PLUS value: 0 left:
                                     [[node alloc] initWithType: C_NAME_RO value: temp left: NULL right: NULL] right: tree]];
            [tree label];
            optimized = 0;
            [tree generateReg: 0 decision: 0];
            tree = NULL;
            return;
        }
        if ([variables objectForKey:name] == nil)
            [variables setObject:[NSNumber numberWithInt:next_var++] forKey:name];
        assigned = [NSMutableString stringWithString:name];
        [self get_lex];
        if (lex != C_EQUAL) {
            [self emitError:@"required '=' for assignment"];
            return;
        }
        [self get_lex];
        [self evalExprReg: 0 decision: 0];
        fprintf(output, "\tMVO R0," VAR_PREFIX "%d\n", [(NSNumber *) [variables objectForKey:assigned] intValue]);
    }
    
    /*
    ** Process a BASIC statement
    */
-   (void) compileStatement
    {
        while (1) {
            if (lex == C_NAME) {
                last_is_return = 0;
                if ([name isEqualToString:@"ELSE"])
                    break;
                if ([name isEqualToString:@"GOTO"]) {
                    [self get_lex];
                    if (lex != C_NAME) {
                        [self emitError:@"bad syntax for GOTO"];
                    } else {
                        if ([labels objectForKey:name] == nil) {
                            [labels setObject:[NSNumber numberWithInt:next_label] forKey:name];
                            next_label++;
                        }
                        fprintf(output, "\tB " LABEL_PREFIX "%d\n", [(NSNumber *) [labels objectForKey:name] intValue]);
                        [self get_lex];
                    }
                } else if ([name isEqualToString:@"GOSUB"]) {
                    [self get_lex];
                    if (lex != C_NAME) {
                        [self emitError:@"bad syntax for GOSUB"];
                    } else {
                        if ([labels objectForKey:name] == nil) {
                            [labels setObject:[NSNumber numberWithInt:next_label] forKey:name];
                            next_label++;
                        }
                        fprintf(output, "\tCALL " LABEL_PREFIX "%d\n", [(NSNumber *) [labels objectForKey:name] intValue]);
                        [self get_lex];
                    }
                } else if ([name isEqualToString:@"RETURN"]) {
                    [self get_lex];
                    fprintf(output, "\tRETURN\n");
                    last_is_return = 1;
                } else if ([name isEqualToString:@"IF"]) {
                    int label;
                    int rama_else;
                    int label2;
                    
                    [self get_lex];
                    label = next_local++;
                    [self evalExprReg: 0 decision:label];
		    if (!optimized) {
			fprintf(output, "\tTSTR R0\n");
			fprintf(output, "\tBEQ " TEMP_PREFIX "%d\n", label);
		    }
                    if (lex == C_NAME && [name isEqualToString:@"GOTO"]) {
                        [self compileStatement];
                    } else if (lex != C_NAME || ![name isEqualToString:@"THEN"]) {
                        [self emitError:@"missing ELSE in IF"];
                    } else {
                        [self get_lex];
                        [self compileStatement];
                    }
                    if (lex == C_NAME && [name isEqualToString:@"ELSE"]) {
                        rama_else = 1;
                        [self get_lex];
                        label2 = next_local++;
                        fprintf(output, "\tB " TEMP_PREFIX "%d\n", label2);
                    } else {
                        rama_else = 0;
                    }
                    fprintf(output, TEMP_PREFIX "%d:\n", label);
                    if (rama_else) {
                        [self compileStatement];
                        fprintf(output, TEMP_PREFIX "%d:\n", label2);
                    }
                } else if ([name isEqualToString:@"FOR"]) {
                    int label1;
		    loop *new_loop = [[loop alloc] init];
                    NSMutableString *loop;
                    node *final = NULL;
                    node *step = NULL;
                    
                    [self get_lex];
                    [self compileAssignment];
                    loop = [NSMutableString stringWithString:assigned];
                    label1 = next_local++;
                    fprintf(output, TEMP_PREFIX "%d:\n", label1);
                    if (lex != C_NAME || ![name isEqualToString:@"TO"]) {
                        [self emitError:@"missing TO in FOR"];
                    } else {
                        [self get_lex];
                        final = [self evalLevel0];
                        if (lex == C_NAME && [name isEqualToString:@"STEP"]) {
                            [self get_lex];
                            if (lex == C_MINUS) {
                                [self get_lex];
                                step = [self evalLevel0];
                                step = [[node alloc] initWithType: C_MINUS value: 0 left:
                                                [[node alloc] initWithType:C_NAME value: [(NSNumber *) [variables objectForKey:loop] intValue] left: 0 right: 0] right: step];
                            } else {
                                step = [self evalLevel0];
                                step = [[node alloc] initWithType: C_PLUS value: 0 left:
                                                [[node alloc] initWithType:C_NAME value: [(NSNumber *) [variables objectForKey:loop] intValue] left: 0 right: 0] right: step];
                            }
                        }
                    }
		    new_loop.step = step;
		    new_loop.final = final;
		    new_loop.var = loop;
		    new_loop.label = label1;
                    [loops addObject:new_loop];
                } else if ([name isEqualToString:@"NEXT"]) {
                    [self get_lex];
                    if ([loops count] == 0) {
                        [self emitError:@"NEXT without FOR"];
                    } else {
                        node *final = ((loop *) loops[[loops count] - 1]).final;
                        node *step = ((loop *) loops[[loops count] - 1]).step;
                        int label1 = ((loop *) loops[[loops count] - 1]).label;
                        NSString *loopvar = ((loop *) loops[[loops count] - 1]).var;
                        
                        if (lex == C_NAME) {
                            if ([loops count] > 0 && ![name isEqualToString:loopvar])
                                [self emitError:@"bad nested NEXT"];
                            [self get_lex];
                        }
                        if (step != NULL) {
                            [step label];
                            [step generateReg: 0 decision: 0];
                        } else {
                            fprintf(output, "\tMVI " VAR_PREFIX "%d,R0\n", [(NSNumber *) [variables objectForKey:loopvar] intValue]);
                            fprintf(output, "\tINCR R0\n");
                        }
                        fprintf(output, "\tMVO R0," VAR_PREFIX "%d\n", [(NSNumber *) [variables objectForKey:loopvar] intValue]);
                        [final label];
                        [final generateReg: 0 decision: 0];
                        fprintf(output, "\tCMP " VAR_PREFIX "%d,R0\n", [(NSNumber *) [variables objectForKey:loopvar] intValue]);
                        if (step == NULL || [step nodeType] == C_PLUS)
                            fprintf(output, "\tBGE " TEMP_PREFIX "%d\n", label1);
                        else
                            fprintf(output, "\tBLE " TEMP_PREFIX "%d\n", label1);
                        final = nil;
                        step = nil;
                        [loops removeLastObject];
                    }
                } else if ([name isEqualToString:@"POKE"]) {
                    [self get_lex];
                    [self evalExprReg: 0 decision: 0];
                    fprintf(output, "\tPSHR R0\n");
                    if (lex != C_COMMA)
                        [self emitError:@"missing comma in POKE"];
                    else
                        [self get_lex];
                    [self evalExprReg: 0 decision: 0];
                    fprintf(output, "\tPULR R4\n");
                    fprintf(output, "\tMVO@ R0,R4\n");
                    
                } else if ([name isEqualToString:@"REM"]) {
                    line_pos = line_size;
                    [self get_lex];
                } else if ([name isEqualToString:@"CLS"]) {
                    [self get_lex];
                    fprintf(output, "\tCALL CLRSCR\n");
                    fprintf(output, "\tMVII #$200,R0\n");
                    fprintf(output, "\tMVO R0,_screen\n");
                } else if ([name isEqualToString:@"WAIT"]) {
                    [self get_lex];
                    fprintf(output, "\tCALL _wait\n");
                } else if ([name isEqualToString:@"RESTORE"]) {
                    [self get_lex];
                    if (lex != C_NAME) {
                        [self emitError:@"bad syntax for RESTORE"];
                    } else {
                        if ([labels objectForKey:name] == nil) {
                            [labels setObject:[NSNumber numberWithInt:next_label] forKey:name];
                            next_label++;
                        }
                        fprintf(output, "\tMVII #" LABEL_PREFIX "%d,R4\n", [(NSNumber *) [labels objectForKey:name] intValue]);
                        fprintf(output, "\tMVO R4,_read\n");
                        [self get_lex];
                    }
                } else if ([name isEqualToString:@"READ"]) {
                    [self get_lex];
                    while (1) {
                        if (lex != C_NAME) {
                            [self emitError:@"missing variable in READ"];
                            break;
                        }
                        if ([variables objectForKey:name] == nil)
                            [variables setObject:[NSNumber numberWithInt:next_var++] forKey:name];
                        fprintf(output, "\tMVI _read,R4\n");
                        fprintf(output, "\tMVI@ R4,R0\n");
                        fprintf(output, "\tMVO R4,_read\n");
                        fprintf(output, "\tMVO R0," VAR_PREFIX "%d\n", [(NSNumber *) [variables objectForKey:name] intValue]);
                        [self get_lex];
                        if (lex != C_COMMA)
                            break;
                        [self get_lex];
                    }
                } else if ([name isEqualToString:@"DATA"]) {
                    [self get_lex];
                    while (1) {
                        node *tree;
                        
                        tree = [self evalLevel0];
                        if ([tree nodeType] != C_NUM) {
                            [self emitError:@"not a constant expression in DATA"];
                            break;
                        }
                        fprintf(output, "\tDECLE %d\n", [tree nodeValue]);
                        tree = nil;
                        if (lex != C_COMMA)
                            break;
                        [self get_lex];
                    }
                } else if ([name isEqualToString:@"DEFINE"]) {
                    [self get_lex];
                    [self evalExprReg: 0 decision: 0];
                    fprintf(output, "\tMVO R0,_gram_target\n");
                    if (lex != C_COMMA)
                        [self emitError:@"missing comma for DEFINE"];
                    else
                        [self get_lex];
                    [self evalExprReg: 0 decision: 0];
                    fprintf(output, "\tMVO R0,_gram_total\n");
                    if (lex != C_COMMA)
                        [self emitError:@"missing comma for DEFINE"];
                    else
                        [self get_lex];
                    if (lex != C_NAME) {
                        [self emitError:@"bad syntax for DEFINE"];
                    } else {
                        if ([labels objectForKey:name] == nil) {
                            [labels setObject:[NSNumber numberWithInt:next_label] forKey:name];
                            next_label++;
                        }
                        fprintf(output, "\tMVII #" LABEL_PREFIX "%d,R4\n", [(NSNumber *) [labels objectForKey:name] intValue]);
                        fprintf(output, "\tMVO R4,_gram_bitmap\n");
                        [self get_lex];
                    }
                    fprintf(output, "\tMVII #1,R0\n");
                    fprintf(output, "\tMVO R0,_load_gram\n");
                } else if ([name isEqualToString:@"SOUND"]) {
                    int canal;
                    
                    [self get_lex];
                    if (lex != C_NUM) {
                        [self emitError:@"bad syntax for SOUND"];
                        canal = 0;
                    } else {
                        canal = value;
                        if (canal < 0 || canal > 4)
                            [self emitError:@"bad channel for SOUND"];
                        [self get_lex];
                    }
                    if (lex != C_COMMA) {
                        [self emitError:@"bad syntax for SOUND"];
                    } else {
                        [self get_lex];
                    }
                    if (lex != C_COMMA) {
                        [self evalExprReg: 0 decision: 0];
                        if (canal == 0)
                            fprintf(output, "\tMVO R0,$01f0\n");
                        if (canal == 1)
                            fprintf(output, "\tMVO R0,$01f1\n");
                        if (canal == 2)
                            fprintf(output, "\tMVO R0,$01f2\n");
                        if (canal == 3)
                            fprintf(output, "\tMVO R0,$01f3\n");
                        if (canal == 4)
                            fprintf(output, "\tMVO R0,$01f9\n");
                        if (canal <= 3)
                            fprintf(output, "\tSWAP R0\n");
                        if (canal == 0)
                            fprintf(output, "\tMVO R0,$01f4\n");
                        if (canal == 1)
                            fprintf(output, "\tMVO R0,$01f5\n");
                        if (canal == 2)
                            fprintf(output, "\tMVO R0,$01f6\n");
                        if (canal == 3)
                            fprintf(output, "\tMVO R0,$01f7\n");
                    }
                    if (lex == C_COMMA) {
                        [self get_lex];
                        [self evalExprReg: 0 decision: 0];
                        if (canal == 0)
                            fprintf(output, "\tMVO R0,$01fb\n");
                        if (canal == 1)
                            fprintf(output, "\tMVO R0,$01fc\n");
                        if (canal == 2)
                            fprintf(output, "\tMVO R0,$01fd\n");
                        if (canal == 3)
                            fprintf(output, "\tMVO R0,$01fa\n");
                        if (canal == 4)
                            fprintf(output, "\tMVO R0,$01f8\n");
                    }
                } else if ([name isEqualToString:@"SPRITE"]) {
                    int sprite;
                    
                    [self get_lex];
                    if (lex != C_NUM) {
                        [self emitError:@"bad syntax for SPRITE"];
                        sprite = 0;
                    } else {
                        sprite = value;
                        if (sprite < 0 || sprite > 7)
                            [self emitError:@"bad number (0-7) for SPRITE"];
                        [self get_lex];
                        if (lex != C_COMMA)
                            [self emitError:@"bad syntax for SPRITE"];
                        else
                            [self get_lex];
                    }
                    if (lex != C_COMMA) {
                        [self evalExprReg: 0 decision: 0];
                        fprintf(output, "\tMVO R0,_mobs+%d\n", (0 + sprite));
                    }
                    if (lex == C_COMMA) {
                        [self get_lex];
                        if (lex != C_COMMA) {
                            [self evalExprReg: 0 decision: 0];
                            fprintf(output, "\tMVO R0,_mobs+%d\n", (8 + sprite));
                        }
                        if (lex == C_COMMA) {
                            [self get_lex];
                            [self evalExprReg: 0 decision: 0];
                            fprintf(output, "\tMVO R0,_mobs+%d\n", (16 + sprite));
                        }
                    }
                } else if ([name isEqualToString:@"PRINT"]) {
                    int start;
                    
                    [self get_lex];
                    start = 1;
                    if (lex == C_NAME && [name isEqualToString:@"AT"]) {
                        node *final;
                        
                        [self get_lex];
                        final = [self evalLevel0];
                        final = [[node alloc] initWithType:C_PLUS value:0 left:final right:[[node alloc] initWithType: C_NUM value: 0x200 left: NULL right: NULL]];
                        [final label];
                        [final generateReg: 0 decision: 0];
                        fprintf(output, "\tMVO R0,_screen\n");
                        start = 0;
                    }
                    if (lex == C_NAME && [name isEqualToString:@"COLOR"]) {
                        [self get_lex];
                        [self evalExprReg: 0 decision: 0];
                        fprintf(output, "\tMVO R0,_color\n");
                        start = 0;
                    }
                    while (1) {
                        if (!start) {
                            if (lex != C_COMMA)
                                break;
                            [self get_lex];
                        }
                        start = 0;
                        if (lex == C_STRING) {
                            int c;
							int p;
                            
                            fprintf(output, "\tMVI _screen,R4\n");
							p = -1;
                            for (c = 0; c < offset; c++) {
				if (pstring[c] * 8 != p) {
				    fprintf(output, "\tMVII #%d,R0\n", (pstring[c] * 8));
				    fprintf(output, "\tXOR _color,R0\n");
				    p = pstring[c] * 8;
				} else {
				    p = -1;  /* Avoids more than two MVO@ in sequence */
				}
                                fprintf(output, "\tMVO@ R0,R4\n");
                            }
                            fprintf(output, "\tMVO R4,_screen\n");
                            [self get_lex];
                            continue;
                        }
                        [self evalExprReg: 0 decision: 0];
                        fprintf(output, "\tMVI _screen,R4\n");
                        fprintf(output, "\tMVO@ R0,R4\n");
                        fprintf(output, "\tMVO R4,_screen\n");
                    }
                } else if ([name isEqualToString:@"BITMAP"]) {
                    [self get_lex];
                    if (lex != C_STRING || offset != 8) {
                        [self emitError:@"syntax error in BITMAP"];
                    } else {
                        int c;
                        
                        value = 0;
                        for (c = 0; c < 8; c++) {
                            if (pstring[c] != 0x10 && pstring[c] != 0x3f   /* 0 and _ */
                             && pstring[c] != 0x00 && pstring[c] != 0x0e)  /* space and . */
                                value |= 0x80 >> c;
                        }
                        [self get_lex];
                        if (bitmap_byte == 0) {
                            bitmap_value = value;
                            bitmap_byte = 1;
                        } else {
                            bitmap_value |= value << 8;
                            bitmap_byte = 0;
                            fprintf(output, "\tDECLE %d\n", bitmap_value);
                        }
                    }
                } else if ([name isEqualToString:@"SCROLL"]) {
                    [self get_lex];
                    scroll_used = 1;
                    if (lex != C_COMMA) {
                        [self evalExprReg: 0 decision: 0];
                        fprintf(output, "\tMVO R0,_scroll_x\n");
                    }
                    if (lex == C_COMMA) {
                        [self get_lex];
                        if (lex != C_COMMA) {
                            [self evalExprReg: 0 decision: 0];
                            fprintf(output, "\tMVO R0,_scroll_y\n");
                        }
                        if (lex == C_COMMA) {
                            [self get_lex];
                            [self evalExprReg: 0 decision: 0];
                            fprintf(output, "\tMVO R0,_scroll_d\n");
                        }
                    }
                } else if ([name isEqualToString:@"BORDER"]) {
                    [self get_lex];
                    if (lex != C_COMMA) {
                        [self evalExprReg: 0 decision: 0];
                        fprintf(output, "\tMVO R0,_border_color\n");
                    }
                    if (lex == C_COMMA) {
                        [self get_lex];
                        [self evalExprReg: 0 decision: 0];
                        fprintf(output, "\tMVO R0,_border_mask\n");
                    }
                } else if ([name isEqualToString:@"CONST"]) {
                    NSNumber *number;

                    [self get_lex];
                    if (lex != C_NAME) {
                        [self emitError:@"name required for constant assignment"];
                        return;
                    }
                    number = [constants objectForKey:name];
                    if (number != nil)
                        [self emitError:@"constant redefined"];
                    assigned = [NSMutableString stringWithString:name];
                    [self get_lex];
                    if (lex != C_EQUAL) {
                        [self emitError:@"required '=' for constant assignment"];
                    } else {
                        node *tree;
                        
                        [self get_lex];
                        tree = [self evalLevel0];
                        if ([tree nodeType] != C_NUM) {
                            [self emitError:@"not a constant expression in CONST"];
                        } else {
                            [constants setObject:[NSNumber numberWithInt:([tree nodeValue] & 0xffff) | 0x10000] forKey: assigned];
                        }
                        tree = NULL;
                    }
                } else if ([name isEqualToString:@"DIM"]) {
                    NSString *array;
                    node *tree = NULL;
                    int c;
                    
                    while (1) {
                        [self get_lex];
                        if (lex != C_NAME) {
                            [self emitError:@"missing name in DIM"];
                            break;
                        }
                        array = [NSString stringWithString:name];
                        [self get_lex];
                        if (lex != C_LPAREN) {
                            [self emitError:@"missing left parenthesis in DIM"];
                        } else {
                            [self get_lex];
                        }
                        tree = [self evalLevel0];
                        if ([tree nodeType] != C_NUM) {
                            [self emitError:@"not a constant expression in DIM"];
                            break;
                        }
                        c = [tree nodeValue];
                        if (c <= 0 || c > 65535) {
                            [self emitError:@"invalid dimension in DIM"];
                            c = 1;
                        }
                        if ([arrays objectForKey:array] != nil)
                            [self emitError:@"already used name for DIM"];
                        else
                            [arrays setObject: [NSNumber numberWithInt:c | (next_label++ << 16)] forKey:array];
                        tree = NULL;
                        if (lex != C_RPAREN) {
                            [self emitError:@"missing right parenthesis in DIM"];
                        } else {
                            [self get_lex];
                        }
                        if (lex != C_COMMA)
                            break;
                    }
                } else if ([name isEqualToString:@"MODE"]) {    /* Video mode selection */
                    node *tree = NULL;
                    node *tree2 = NULL;
                    int mode;
                    
                    [self get_lex];
                    tree = [self evalLevel0];
                    if ([tree nodeType] != C_NUM) {
                        [self emitError:@"not a constant expression in MODE"];
                        break;
                    }
                    mode = [tree nodeValue];
                    if (mode != 0 && mode != 1) {
                        [self emitError:@"invalid MODE"];
                        break;
                    }
                    tree = nil;
                    if (mode == 0) {  /* Color Stack mode */
                        if (lex != C_COMMA)
                            [self emitError:@"missing comma in MODE"];
                        else
                            [self get_lex];
                        tree = [self evalLevel0];
                        if (lex != C_COMMA)
                            [self emitError:@"missing comma in MODE"];
                        else
                            [self get_lex];
                        tree2 = [self evalLevel0];
                        tree2 = [[node alloc] initWithType:C_MUL value:0 left:tree2 right:[[node alloc] initWithType:C_NUM value: 0x100 left: NULL right: NULL]];
                        tree = [[node alloc] initWithType:C_PLUS value:0 left:tree right:tree2];
                        [tree label];
                        [tree generateReg: 0 decision: 0];
                        tree = nil;
                        fprintf(output, "\tMVO R0,_screen\n");
                        if (lex != C_COMMA)
                            [self emitError:@"missing comma in MODE"];
                        else
                            [self get_lex];
                        tree = [self evalLevel0];
                        if (lex != C_COMMA)
                            [self emitError:@"missing comma in MODE"];
                        else
                            [self get_lex];
                        tree2 = [self evalLevel0];
                        tree2 = [[node alloc] initWithType:C_MUL value:0 left:tree2 right:[[node alloc] initWithType:C_NUM value: 0x100 left: NULL right: NULL]];
                        tree = [[node alloc] initWithType:C_PLUS value:0 left:tree right:tree2];
                        [tree label];
                        [tree generateReg: 0 decision: 0];
                        tree = nil;
                        fprintf(output, "\tMVO R0,_color\n");
                        fprintf(output, "\tMVII #1,R0\n");
                        fprintf(output, "\tMVO R0,_mode_select\n");
                    } else {    /* Foreground/Background mode */
                        fprintf(output, "\tMVII #2,R0\n");
                        fprintf(output, "\tMVO R0,_mode_select\n");
                    }
                } else if ([name isEqualToString:@"SCREEN"]) {  /* Copy screen */
                    [self get_lex];
                    if (lex != C_NAME) {
                        [self emitError:@"bad syntax for SCREEN"];
                        break;
                    }
                    assigned = [NSMutableString stringWithString:name];
                    if ([labels objectForKey:name] == nil) {
                        [labels setObject:[NSNumber numberWithInt:next_label] forKey:name];
                        next_label++;
                    }
                    [self get_lex];
                    if (lex == C_COMMA) {
                        [self get_lex];
                        [self evalExprReg: 0 decision: 0];
                        fprintf(output, "\tADDI #" LABEL_PREFIX "%d,R0\n", [(NSNumber *) [labels objectForKey:assigned] intValue]);
                        fprintf(output, "\tPSHR R0\n");
                        if (lex != C_COMMA) {
                            [self emitError:@"missing comma after second parameter in SCREEN"];
                            break;
                        }
                        [self get_lex];
                        [self evalExprReg: 0 decision: 0];
                        fprintf(output, "\tADDI #$200,R0\n");
                        fprintf(output, "\tPSHR R0\n");
                        if (lex != C_COMMA) {
                            [self emitError:@"missing comma after third parameter in SCREEN"];
                            break;
                        }
                        [self get_lex];
                        [self evalExprReg: 0 decision: 0];
                        fprintf(output, "\tPSHR R0\n");
                        if (lex != C_COMMA) {
                            [self emitError:@"missing comma after fourth parameter in SCREEN"];
                            break;
                        }
                        [self get_lex];
                        [self evalExprReg: 0 decision: 0];
                        fprintf(output, "\tPULR R1\n");
                        fprintf(output, "\tPULR R2\n");
                        fprintf(output, "\tPULR R3\n");
                        fprintf(output, "\tCALL CPYBLK\n");
                    } else {
                        fprintf(output, "\tMVII #" LABEL_PREFIX "%d,R3\n", [(NSNumber *) [labels objectForKey:name] intValue]);
                        fprintf(output, "\tMVII #$200,R2\n");
                        fprintf(output, "\tMVII #20,R1\n");
                        fprintf(output, "\tMVII #12,R0\n");
                        fprintf(output, "\tCALL CPYBLK\n");
                    }
                } else if ([name isEqualToString:@"PLAY"]) {
                    [self get_lex];
                    if (lex != C_NAME) {
                        [self emitError:@"bad syntax for PLAY"];
                        break;
                    }
                    music_used = 1;
                    if ([name isEqualToString:@"OFF"]) {
                        fprintf(output, "\tCLRR R0\n");
                        fprintf(output, "\tCALL _play_music\n");
                    } else if ([name isEqualToString:@"SIMPLE"]) {
                        fprintf(output, "\tCLRR R3\n");
                        fprintf(output, "\tMVO R3,_music_mode\n");
                    } else if ([name isEqualToString:@"FULL"]) {
                        fprintf(output, "\tMVII #1,R3\n");
                        fprintf(output, "\tMVO R3,_music_mode\n");
                    } else {
                        if ([labels objectForKey:name] == nil) {
                            [labels setObject:[NSNumber numberWithInt:next_label] forKey:name];
                            next_label++;
                        }
                        fprintf(output, "\tMVII #" LABEL_PREFIX "%d,R0\n", [(NSNumber *) [labels objectForKey:name] intValue]);
                        fprintf(output, "\tCALL _play_music\n");
                    }
                    [self get_lex];
                } else if ([name isEqualToString:@"MUSIC"]) {
                    int arg;
                    static int previous[4];
                    unsigned int notes;
                    int note;
                    int c;
                    
                    [self get_lex];
                    notes = 0;
                    arg = 0;
                    while (1) {
                        if (lex != C_NAME && lex != C_MINUS) {
                            [self emitError:@"bad syntax for MUSIC"];
                            break;
                        }
                        if (lex == C_MINUS) {
                            /* Nothing to do */
                        } else if (arg == 0 && [name isEqualToString:@"REPEAT"]) {
                            [self get_lex];
                            notes = 0xfd;
                            break;
                        } else if (arg == 0 && [name isEqualToString:@"STOP"]) {
                            [self get_lex];
                            notes = 0xfe;
                            break;
			} else if (arg == 3) {
			    if ([name characterAtIndex:0] != 'M' || ([name length] > 0 && [name characterAtIndex:1] < '1') || ([name length] > 0 && [name characterAtIndex:1] > '3')) {
				[self emitError:@"bad syntax for drum in MUSIC"];
				break;
			    }
                            notes |= ([name characterAtIndex:1] - '0') << (arg * 8);
                        } else if ([name isEqualToString:@"S"]) {
                            notes |= 0x3f << (arg * 8);
						} else {
                            notes |= previous[arg] << (arg * 8);
                            c = 0;
                            switch (c < [name length] ? [name characterAtIndex:c++] : 0) {
                                case 'C': note = 0; break;
                                case 'D': note = 2; break;
                                case 'E': note = 4; break;
                                case 'F': note = 5; break;
                                case 'G': note = 7; break;
                                case 'A': note = 9; break;
                                case 'B': note = 11; break;
                                default: [self emitError:@"bad syntax for note in MUSIC"]; break;
                            }
                            switch (c < [name length] ? [name characterAtIndex:c++] : 0) {
                                case '2': note += 0 * 12; break;
                                case '3': note += 1 * 12; break;
                                case '4': note += 2 * 12; break;
                                case '5': note += 3 * 12; break;
                                case '6': note += 4 * 12; break;
                                case '7': if (note == 0) { note += 5 * 12; break; }
                                default: [self emitError:@"bad syntax for note in MUSIC"]; break;
                            }
                            note++;
                            if (c < [name length] && [name characterAtIndex:c] == '#') {
                                note++;
                                c++;
                            }
                            if (c < [name length] && [name characterAtIndex:c] == 'W') {
                                previous[arg] = 0x00;
                                notes &= ~(0xc0 << (arg * 8));
                                notes |= previous[arg] << (arg * 8);
                            } else if (c < [name length] && [name characterAtIndex:c] == 'X') {
                                previous[arg] = 0x40;
                                notes &= ~(0xc0 << (arg * 8));
                                notes |= previous[arg] << (arg * 8);
                            } else if (c < [name length] && [name characterAtIndex:c] == 'Y') {
                                previous[arg] = 0x80;
                                notes &= ~(0xc0 << (arg * 8));
                                notes |= previous[arg] << (arg * 8);
                            } else if (c < [name length] && [name characterAtIndex:c] == 'Z') {
                                previous[arg] = 0xc0;
                                notes &= ~(0xc0 << (arg * 8));
                                notes |= previous[arg] << (arg * 8);
                            }
                            notes |= note << (arg * 8);
                        }
                        [self get_lex];
                        arg++;
                        if (lex != C_COMMA)
                            break;
                        if (arg == 4) {
                            [self emitError:@"too many arguments for MUSIC"];
                            break;
                        }
                        [self get_lex];
                    }
                    fprintf(output, "\tDECLE %d,%d\n", (notes & 0xffff), (notes >> 16 & 0xffff));
                } else if ([name isEqualToString:@"ON"]) {
                    int label;
                    int table;
                    int c;
                    int max_value;
                    int gosub;
                    int options[256];
                    
                    [self get_lex];
                    [self evalExprReg: 0 decision: 0];
                    gosub = 0;
                    if (lex != C_NAME || (![name isEqualToString:@"GOTO"] && ![name isEqualToString:@"GOSUB"])) {
                        [self emitError:@"required GOTO or GOSUB after ON"];
                    } else if ([name isEqualToString:@"GOTO"]) {
                        [self get_lex];
                    } else if ([name isEqualToString:@"GOSUB"]) {
                        [self get_lex];
                        gosub = 1;
                    }
                    max_value = 0;
                    while (1) {
                        if (max_value == sizeof(options) / sizeof(int)) {
                            [self emitError:@"too many options for ON statement"];
                            max_value--;
                        }
                        if (lex == C_NAME) {
                            if ([labels objectForKey:name] == nil) {
                                [labels setObject:[NSNumber numberWithInt:next_label] forKey:name];
                                next_label++;
                            }
                            options[max_value++] = [(NSNumber *) [labels objectForKey:name] intValue];
                            [self get_lex];
                        } else {
                            options[max_value++] = 0;
                        }
                        if (lex != C_COMMA)
                            break;
                        [self get_lex];
                    }
                    table = next_local++;
                    label = next_local++;
                    fprintf(output, "\tCMPI #%d,R0\n", max_value);
                    fprintf(output, "\tBC " TEMP_PREFIX "%d\n", label);
                    if (gosub)
                        fprintf(output, "\tMVII #" TEMP_PREFIX "%d,R5\n", label);
                    fprintf(output, "\tADDI #" TEMP_PREFIX "%d,R0\n", label);
                    fprintf(output, "\tMOVR R0,R1\n");
                    fprintf(output, "\tMVI@ R1,PC\n");
                    
                    fprintf(output, TEMP_PREFIX "%d:\n", table);
                    for (c = 0; c < max_value; c++) {
                        if (options[c])
                            fprintf(output, "\tDECLE " LABEL_PREFIX "%d\n", options[c]);
                        else
                            fprintf(output, "\tDECLE " TEMP_PREFIX "%d\n", label);
                    }
                    fprintf(output, TEMP_PREFIX "%d:\n", label);
                } else if ([name isEqualToString:@"STACK_CHECK"]) {  /* Stack overflow check */
                    [self get_lex];
                    stack_used = 1;
                } else if ([name isEqualToString:@"ASM"]) {         /* ASM statement for inserting assembly code */
                    fprintf(output, "%s\n", [[line substringFromIndex:line_pos] UTF8String]);
                    line_pos = line_size;
                    [self get_lex];
                } else {
                    [self compileAssignment];
                }
            } else {
                last_is_return = 0;
                [self emitError:@"syntax error in statement"];
            }
            if (lex != C_COLON)
                break;
            [self get_lex];
        }
    }

    /*
    ** Starts compilation
    */
-   (int) startInput: (const char *) input_file output: (const char *) output_file library: (const char *) library_path 
    {
        int used_space;
        int available_vars;
        int inside_proc;
        char path[4096];        
        char *p;
        
        constants = [[NSMutableDictionary alloc] init];
        labels = [[NSMutableDictionary alloc] init];
        variables = [[NSMutableDictionary alloc] init];
        arrays = [[NSMutableDictionary alloc] init];
        loops = [[NSMutableArray alloc] init];
        line_number = 0;
	next_label = 1;
	next_var = 1;
        scroll_used = 0;
        keypad_used = 0;
        music_used = 0;
        stack_used = 0;
        active_include = 0;
        next_include = 0;
        input = fopen(input_file, "r");
        if (!input) {
	    fprintf(stderr, "Unable to open input file: %s\n", input_file);
            return -1;
        }
        output = fopen(output_file, "w");
        if (!output) {
	    fprintf(stderr, "Unable to open output file: %s\n", output_file);
            fclose(input);
            return -1;
        }
        fprintf(output, "\t; IntyBASIC compiler %s\n", VERSION);
        strcpy(path, library_path);
        if (strlen(path) > 0 && path[strlen(path) - 1] != '/')
            strcat(path, "/");
        strcat(path, "intybasic_prologue.asm");
        included = fopen(path, "r");
        if (included) {
            while ([self getline:included]) {
                fprintf(output, "%s\n", [line UTF8String]);
            }
            fclose(included);
	} else {
	    fprintf(stderr, "Warning: unable to include: %s\n", path);
        }
        bitmap_byte = 0;
        inside_proc = 0;
        while (1) {
            int label_exists;
            
            if (active_include) {
                if (![self getline:include[next_include]]) {
                    fclose(include[next_include]);
                    next_include++;
                    active_include = 0;
                    line_number = saved_line_number;
                }
            }
            if (!active_include) {
                if (![self getline:input])
                    break;
            }
            line_number++;
	    line_start = 1;
            line_pos = 0;
            line_size = [line length];
	    fprintf(output, "\t; %s\n", [line UTF8String]);
            [self get_lex];
            if (lex == C_LABEL) {
                if ([labels objectForKey:name] != nil) {
/*                    NSString temp = @"already defined '" + name + @"' label"); */
/*                    [self emitError:@temp); */
                } else {
                    [labels setObject:[NSNumber numberWithInt:next_label] forKey:name];
                }
                fprintf(output, "\t; %s\n", [name UTF8String]);
                fprintf(output, LABEL_PREFIX "%d:", [(NSNumber *) [labels objectForKey:name] intValue]);
                next_label++;
                label_exists = 1;
                [self get_lex];
            } else {
                label_exists = 0;
            }
            if (lex == C_NAME) {
                if ([name isEqualToString:@"PROCEDURE"]) {
                    if (inside_proc)
                        fprintf(stderr, "Warning: starting PROCEDURE without ENDing previous PROCEDURE\n");
                    /* as1600 requires that label and PROC are on same line */
                    [self get_lex];
                    fprintf(output, "\tPROC\n\tBEGIN\n");
                    inside_proc = 1;
                    last_is_return = 0;
                } else if ([name isEqualToString:@"END"]) {
                    if (!inside_proc)
                        fprintf(stderr, "Warning: END without PROCEDURE\n");
                    [self get_lex];
                    if (!last_is_return)
                        fprintf(output, "\tRETURN\n");
                    fprintf(output, "\tENDP\n");
                    inside_proc = 0;
                    last_is_return = 0;
                } else if ([name isEqualToString:@"INCLUDE"]) {
                    if (next_include == 50) {
                        fprintf(stderr, "Error: more than 50 INCLUDE used at line %d\n", line_number);
                    } else if (active_include) {
                        fprintf(stderr, "Error: trying to use INCLUDE inside INCLUDE in line %d\n", line_number);
                    } else {
                        while (line_pos < line_size && isspace([line characterAtIndex:line_pos]))
                            line_pos++;
                        p = &path[0];
                        while (p < &path[4095] && line_pos < line_size)
                            *p++ = [line characterAtIndex:line_pos++];
                        while (p > &path[0] && isspace(*(p - 1)))
                            p--;
                        *p = '\0';
                        include[next_include] = fopen(path, "r");
                        if (!include[next_include]) {
                            fprintf(stderr, "Error: INCLUDE not successful in line %d\n", line_number);
                            next_include++;
                        } else {
                            saved_line_number = line_number;
                            line_number = 0;
                            active_include = 1;
                        }
                    }
                    lex = C_END;
                } else {
                    if (label_exists == 1)
                        fprintf(output, "\t");
                    [self compileStatement];
                }
            }
            if (lex != C_END)
                fprintf(stderr, "Warning: Invalid extra characters in line %d\n", line_number);            
        }
        if (scroll_used)
            fprintf(output, "intybasic_scroll:\tequ 1\t; Forces to include scroll library\n");
        if (keypad_used)
            fprintf(output, "intybasic_keypad:\tequ 1\t; Forces to include keypad library\n");
        if (music_used)
            fprintf(output, "intybasic_music:\tequ 1\t; Forces to include music library\n");
        if (stack_used)
            fprintf(output, "intybasic_stack:\tequ 1\t; Forces to include stack overflow checking\n");
        strcpy(path, library_path);
        if (strlen(path) > 0 && path[strlen(path) - 1] != '/')
            strcat(path, "/");
        strcat(path, "intybasic_epilogue.asm");
        included2 = fopen(path, "r");
        if (included2) {
            while ([self getline:included2]) {
                fprintf(output, "%s\n", [line UTF8String]);
            }
            fclose(included2);
	} else {
	    fprintf(stderr, "Warning: unable to include: %s\n", path);
	}
        
        /* Dumps 8-bits variables */
        used_space = 8;
        for (id key in variables) {
	    if ([key characterAtIndex:0] != '#') {
                int size;
                
                size = 1;
                fprintf(output, VAR_PREFIX "%d:\tRMB %d\t; %s\n", [(NSNumber *) [variables objectForKey: key] intValue], size, [key UTF8String]);
                used_space += size;
            }
        }
        for (id key in arrays) {
	    if ([key characterAtIndex:0] != '#' 
             && [(NSNumber *) [arrays objectForKey: key] intValue] != 0) {
                int size;
                int label;
                
                size = [(NSNumber *) [arrays objectForKey: key] intValue] & 0xffff;
                label = [(NSNumber *) [arrays objectForKey: key] intValue] >> 16;
                fprintf(output, LABEL_PREFIX "%d:\tRMB %d\t; %s\n", label, size,
                    [key UTF8String]);
                used_space += size;
            }
        }
        available_vars = 224 - 10;
        if (scroll_used)
            available_vars -= 3;
        if (keypad_used)
            available_vars -= 6;
        if (music_used)
            available_vars -= 26;
        if (used_space > available_vars)
            fprintf(stderr, "Warning: Use of 8-bits variables exceeds available space (%d vs %d)\n", used_space, available_vars);
        fprintf(output, "_SCRATCH:\tEQU $\n");
        
        /* Arranges stack */
        fprintf(output, "\nSYSTEM:\tORG $2F0, $2F0, \"-RWBN\"\n");
        fprintf(output, "STACK:\tRMB 24\n");

        /* Dumps 16-bits variables */
        used_space = 0;
        for (id key in variables) {
	    if ([key characterAtIndex:0] == '#') {
                int size;
                
                size = 1;
                fprintf(output, VAR_PREFIX "%d:\tRMB %d\t; %s\n", [(NSNumber *) [variables objectForKey: key] intValue], size, [key UTF8String]);
                used_space += size;
            }
        }
        for (id key in arrays) {
	    if ([key characterAtIndex:0] == '#' && [(NSNumber *) [arrays objectForKey: key] intValue] != 0) {
                int size;
                int label;
                
                size = [(NSNumber *) [arrays objectForKey: key] intValue] & 0xffff;
                label = [(NSNumber *) [arrays objectForKey: key] intValue] >> 16;
                fprintf(output, LABEL_PREFIX "%d:\tRMB %d\t; %s\n", label, size,
                    [key UTF8String]);
                used_space += size;
            }
        }
        available_vars = 48;
        if (scroll_used)
            available_vars -= 20;
        if (used_space > available_vars)
            fprintf(stderr, "Warning: Use of 16-bits variables exceeds available space (%d vs %d)\n", used_space, available_vars);
        
        fprintf(output, "_SYSTEM:\tEQU $\n");

        fclose(output);
        fclose(input);
        fprintf(stderr, "Compilation finished\n\n");
        return 0;
    }

    /*
    ** Get a line
    */
-   (int) getline: (FILE *) file
    {
        int c;
        unichar current;

        line = [NSMutableString string];
        while (1) {
            c = fgetc(file);
            if (c == '\n' || c == EOF)
                break;
            current = c;
            [line appendString:[NSString stringWithCharacters:&current length:1]];
        }  
        if (c == EOF && [line length] == 0)
            return 0;
        return 1;
    }

@end

/*
** Main program
*/
int main(int argc, const char * argv[])
{
    compiler *basic = [compiler new];
    
    fprintf(stderr, "\nIntyBASIC compiler " VERSION "\n");
    fprintf(stderr, "(c) 2014 Oscar Toledo G. http://nanochess.org/\n\n");
    if (argc != 3 && argc != 4) {
        fprintf(stderr, "Usage: intybasic infile.bas outfile.asm [library_path]\n\n");
        return -1;
    }
    return [basic startInput: argv[1] output: argv[2] library: (argc == 4) ? argv[3] : ""];
}
