REM Module:			TinyFont.bas
REM
REM Description:	An example of using a 4 pixel wide font.
REM Author(s):		Mark Ball
REM Date:			21/03/16
REM Version:		1.00F
REM
REM HISTORY
REM -------
REM 1.00F 21/03/16 - First release.
REM
REM -------------------------------------------------------------------------

	' We need some important constants.
    include "constants.bas"

	REM -------------------------------------------------------------------------
REM Initialisation.
REM -------------------------------------------------------------------------

	' Display a long string in the tiny font.
    call TINYFONT(varptr string(0))

REM -------------------------------------------------------------------------
REM Main loop
REM -------------------------------------------------------------------------	
loop:
    wait
    goto loop

REM -------------------------------------------------------------------------
REM Message to print on screen.
REM -------------------------------------------------------------------------
REM 1st element - The destination on screen (must use constants).
REM 2nd element - The colour of the text (with GRAM flag set).
REM 3rd element - The start index in GRAM (DEF00 to DEF63).
REM 4rd element - The string to print (no punctuation).
REM 5th element - The string terminator (any negative number).
REM -------------------------------------------------------------------------
string:
    data SCREENADDR(0,0), CS_CYAN+GRAM, DEF00, "HELLO WORLD! "
    data "THE QUICK BROWN FOX JUMPED OVER THE LAZY DOG. SOME PUNCTUATION (+-[]=<>!)%"
    data "....... THATS ALL FOLKS!", -1

REM -------------------------------------------------------------------------
REM TINYFONT - Display a string in a 4 pixel wide font.
REM -------------------------------------------------------------------------
REM
REM Notes:
REM - Takes N VBLANK tick to execute (depends on string length).
REM
REM Usage:
REM CALL TINYFONT(A)
REM
REM Input:
REM		A - VARPTR of a message data structure. Where :-
REM         1st data element is the destination address in RAM.
REM 		2nd data element is the text/background colour+GRAM.
REM 		3rd data element is the GRAM index to start the string bitmap in.
REM         4th-N data element is the string in upper case with limited punctuation.
REM         N+1 data element is the string terminator (any negative number).
REM
REM Returns:
REM Nothing!
REM
REM Trashes:
REM r0-r5
REM
REM -------------------------------------------------------------------------
    asm TINYFONT: PROC

    asm ; Font character indexes into the bitmaps.
    asm @@INDEX_A:          EQU ( 0*4)
    asm @@INDEX_0:          EQU (26*4)
    asm @@INDEX_DOT:        EQU (36*4)
    asm @@INDEX_COMMA:      EQU (37*4)
    asm @@INDEX_SEMICOLON:  EQU (38*4)
    asm @@INDEX_COLON:      EQU (39*4)
    asm @@INDEX_MINUS:      EQU (40*4)
    asm @@INDEX_PLUS:       EQU (41*4)
    asm @@INDEX_STAR:       EQU (42*4)
    asm @@INDEX_FSLASH:     EQU (43*4)
    asm @@INDEX_PLING:      EQU (44*4)
    asm @@INDEX_QUOTE:      EQU (45*4)
    asm @@INDEX_PERCENT:    EQU (46*4)
    asm @@INDEX_QUESTION:   EQU (47*4)
    asm @@INDEX_EQUALS:     EQU (48*4)
    asm @@INDEX_LESSTHAN:   EQU (49*4)
    asm @@INDEX_MORETHAN:   EQU (50*4)
    asm @@INDEX_RBRACKET:   EQU (51*4)
    asm @@INDEX_LBRACKET:   EQU (52*4)
    asm @@INDEX_LSBRACKET:  EQU (53*4)
    asm @@INDEX_RSBRACKET:  EQU (54*4)
    asm @@INDEX_BSLASH:     EQU (55*4)
    asm @@INDEX_DOLLAR:     EQU (56*4)

	asm pshr r5

    asm movr r0, r5     ; Get the argument pointer.
    asm mvi@ r5, r0     ; Get position on screen.
    asm pshr r0         ; Save it for later!

    asm mvi@ r5, r0     ; Get colour.
    asm pshr r0         ; Save it for later.

    asm mvi@ r5, r0     ; Get GRAM card to start putting letters into.
    asm sll r0, 2
    asm sll r0, 1       ; x8.
    asm pshr r0         ; Save it for later.
	asm addi #$3800, r0 ; Add base address of GRAM...
    asm movr r0, r4     ; to create a destination pointer.
    asm pshr r4         ; Save it for later

    asm ; GRAM nlock filling main loop.
    asm @@BlockFillLoop:
    asm pshr r5         ; Save message pointer.
    asm pshr r4         ; Save GRAM pointer.
    asm call _wait      ; Wait for VBLANK
    asm pulr r4         ; Restore GRAM pointer.
    asm pulr r5         ; Restore message pointer.

    asm ; Copy up to 7 completed characters to GRAM.
    asm mvii #7, r1
    asm @@L1:

    asm ; Get left side character's data pointer.
    asm mvi@ r5, r2
    asm tstr r2
    asm bmi @@PrintMessageOnScreen
    asm addi #@@LeftFontTable, r2
    asm mvi@ r2, r2

    asm ; Get right side character's data pointer.
    asm mvi@ r5, r3
    asm tstr r3
    asm bmi @@DoLeftHalfOfLastCharacter
    asm addi #@@RightFontTable, r3
    asm mvi@ r3, r3

    asm ; Copy the sources and combine them into the destination.
    asm REPEAT 4
    asm mvi@ r2, r0
    asm incr r2
    asm xor@ r3, r0
    asm incr r3
    asm mvo@ r0, r4
    asm swap r0
    asm mvo@ r0, r4
    asm ENDR

    asm ; Any more to do?
    asm decr r1
    asm bne @@L1
    asm b @@BlockFillLoop

    asm ; Message now exists in the GRAM cards, now display it.
    asm @@PrintMessageOnScreen:
    asm pulr r2         ; Get start address in GRAM.
    asm subr r2, r4     ; Subtract it from the end address.
    asm movr r4, r2
    asm slr r2, 2
    asm slr r2, 1       ; /8 = no of cards to display.

    asm pulr r0         ; Get GRAM start index.
    asm pulr r1         ; Get text colour.
    asm xorr r1, r0     ; Create BACKTAB starting card.

    asm pulr r5         ; Get destination on screen.
    asm @@CopyToScreenLoop:
    asm mvo@ r0, r5
    asm addi #1 SHL 3, r0
    asm decr r2
    asm bne @@CopyToScreenLoop

    asm pulr pc         ; All done!

    asm ; Right side of the last character to display is empty.
    asm @@DoLeftHalfOfLastCharacter:
    asm REPEAT 4
    asm mvi@ r2, r0
    asm incr r2
    asm mvo@ r0, r4
    asm swap r0
    asm mvo@ r0, r4
    asm ENDR
    asm b @@PrintMessageOnScreen

    asm ; Left chracter look up table (first 62 characters only).
    asm @@LeftFontTable:
    asm DECLE @@FontSpaceData                       ; " "
    asm DECLE @@LeftFont_bitmaps+@@INDEX_PLING      ; "!"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_QUESTION   ; """
    asm DECLE @@LeftFont_bitmaps+@@INDEX_QUESTION   ; "#"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_DOLLAR     ; "$"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_PERCENT    ; "%"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_QUESTION   ; "&"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_QUOTE      ; "'"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_LBRACKET   ; "("
    asm DECLE @@LeftFont_bitmaps+@@INDEX_RBRACKET   ; ")"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_QUESTION   ; "o"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_PLUS       ; "+"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_COMMA      ; ","
    asm DECLE @@LeftFont_bitmaps+@@INDEX_MINUS      ; "-"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_DOT        ; "."
    asm DECLE @@LeftFont_bitmaps+@@INDEX_FSLASH     ; "/"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_0+(0*4)    ; "0"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_0+(1*4)    ; "1"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_0+(2*4)    ; "2"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_0+(3*4)    ; "3"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_0+(4*4)    ; "4"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_0+(5*4)    ; "5"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_0+(6*4)    ; "6"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_0+(7*4)    ; "7"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_0+(8*4)    ; "8"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_0+(9*4)    ; "9"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_COLON      ; ":"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_SEMICOLON  ; ";"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_LESSTHAN   ; "<"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_EQUALS     ; "="
    asm DECLE @@LeftFont_bitmaps+@@INDEX_MORETHAN   ; ">"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_QUESTION   ; "?"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_QUESTION   ; "@"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+( 0*4)   ; "A"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+( 1*4)   ; "B"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+( 2*4)   ; "C"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+( 3*4)   ; "D"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+( 4*4)   ; "E"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+( 5*4)   ; "F"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+( 6*4)   ; "G"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+( 7*4)   ; "H"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+( 8*4)   ; "I"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+( 9*4)   ; "J"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+(10*4)   ; "K"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+(11*4)   ; "L"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+(12*4)   ; "M"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+(13*4)   ; "N"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+(14*4)   ; "O"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+(15*4)   ; "P"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+(16*4)   ; "Q"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+(17*4)   ; "R"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+(18*4)   ; "S"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+(19*4)   ; "T"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+(20*4)   ; "U"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+(21*4)   ; "V"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+(22*4)   ; "W"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+(23*4)   ; "X"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+(24*4)   ; "Y"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_A+(25*4)   ; "Z"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_LSBRACKET  ; "["
    asm DECLE @@LeftFont_bitmaps+@@INDEX_BSLASH     ; "\"
    asm DECLE @@LeftFont_bitmaps+@@INDEX_RSBRACKET  ; "]"

    asm ; Right character look up table (first 62 characters only).
    asm @@RightFontTable:
    asm DECLE @@FontSpaceData                       ; " "
    asm DECLE @@RightFont_bitmaps+@@INDEX_PLING      ; "!"
    asm DECLE @@RightFont_bitmaps+@@INDEX_QUESTION   ; """
    asm DECLE @@RightFont_bitmaps+@@INDEX_QUESTION   ; "#"
    asm DECLE @@RightFont_bitmaps+@@INDEX_DOLLAR     ; "$"
    asm DECLE @@RightFont_bitmaps+@@INDEX_PERCENT    ; "%"
    asm DECLE @@RightFont_bitmaps+@@INDEX_QUESTION   ; "&"
    asm DECLE @@RightFont_bitmaps+@@INDEX_QUOTE      ; "'"
    asm DECLE @@RightFont_bitmaps+@@INDEX_LBRACKET   ; "("
    asm DECLE @@RightFont_bitmaps+@@INDEX_RBRACKET   ; ")"
    asm DECLE @@RightFont_bitmaps+@@INDEX_QUESTION   ; "o"
    asm DECLE @@RightFont_bitmaps+@@INDEX_PLUS       ; "+"
    asm DECLE @@RightFont_bitmaps+@@INDEX_COMMA      ; ","
    asm DECLE @@RightFont_bitmaps+@@INDEX_MINUS      ; "-"
    asm DECLE @@RightFont_bitmaps+@@INDEX_DOT        ; "."
    asm DECLE @@RightFont_bitmaps+@@INDEX_FSLASH     ; "/"
    asm DECLE @@RightFont_bitmaps+@@INDEX_0+(0*4)    ; "0"
    asm DECLE @@RightFont_bitmaps+@@INDEX_0+(1*4)    ; "1"
    asm DECLE @@RightFont_bitmaps+@@INDEX_0+(2*4)    ; "2"
    asm DECLE @@RightFont_bitmaps+@@INDEX_0+(3*4)    ; "3"
    asm DECLE @@RightFont_bitmaps+@@INDEX_0+(4*4)    ; "4"
    asm DECLE @@RightFont_bitmaps+@@INDEX_0+(5*4)    ; "5"
    asm DECLE @@RightFont_bitmaps+@@INDEX_0+(6*4)    ; "6"
    asm DECLE @@RightFont_bitmaps+@@INDEX_0+(7*4)    ; "7"
    asm DECLE @@RightFont_bitmaps+@@INDEX_0+(8*4)    ; "8"
    asm DECLE @@RightFont_bitmaps+@@INDEX_0+(9*4)    ; "9"
    asm DECLE @@RightFont_bitmaps+@@INDEX_COLON      ; ":"
    asm DECLE @@RightFont_bitmaps+@@INDEX_SEMICOLON  ; ";"
    asm DECLE @@RightFont_bitmaps+@@INDEX_LESSTHAN   ; "<"
    asm DECLE @@RightFont_bitmaps+@@INDEX_EQUALS     ; "="
    asm DECLE @@RightFont_bitmaps+@@INDEX_MORETHAN   ; ">"
    asm DECLE @@RightFont_bitmaps+@@INDEX_QUESTION   ; "?"
    asm DECLE @@RightFont_bitmaps+@@INDEX_QUESTION   ; "@"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+( 0*4)   ; "A"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+( 1*4)   ; "B"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+( 2*4)   ; "C"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+( 3*4)   ; "D"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+( 4*4)   ; "E"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+( 5*4)   ; "F"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+( 6*4)   ; "G"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+( 7*4)   ; "H"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+( 8*4)   ; "I"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+( 9*4)   ; "J"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+(10*4)   ; "K"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+(11*4)   ; "L"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+(12*4)   ; "M"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+(13*4)   ; "N"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+(14*4)   ; "O"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+(15*4)   ; "P"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+(16*4)   ; "Q"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+(17*4)   ; "R"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+(18*4)   ; "S"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+(19*4)   ; "T"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+(20*4)   ; "U"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+(21*4)   ; "V"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+(22*4)   ; "W"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+(23*4)   ; "X"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+(24*4)   ; "Y"
    asm DECLE @@RightFont_bitmaps+@@INDEX_A+(25*4)   ; "Z"
    asm DECLE @@RightFont_bitmaps+@@INDEX_LSBRACKET  ; "["
    asm DECLE @@RightFont_bitmaps+@@INDEX_BSLASH     ; "\"
    asm DECLE @@RightFont_bitmaps+@@INDEX_RSBRACKET  ; "]"

    asm ; Just a space.
    asm @@FontSpaceData:                     
    asm DECLE 0,0,0,0
    
    asm ; Left side font character bitmap data                    
    asm @@LeftFont_bitmaps:
	asm DECLE $4000,$A0A0,$A0E0,$00A0
	asm DECLE $C000,$C0A0,$A0A0,$00C0
	asm DECLE $4000,$80A0,$A080,$0040
	asm DECLE $C000,$A0A0,$A0A0,$00C0
	asm DECLE $E000,$C080,$8080,$00E0
	asm DECLE $E000,$C080,$8080,$0080
	asm DECLE $4000,$80A0,$A0A0,$0060
	asm DECLE $A000,$E0A0,$A0A0,$00A0
	asm DECLE $E000,$4040,$4040,$00E0
	asm DECLE $E000,$2020,$A020,$0040
	asm DECLE $A000,$C0A0,$A0A0,$00A0
	asm DECLE $8000,$8080,$8080,$00E0
	asm DECLE $A000,$E0E0,$A0A0,$00A0
	asm DECLE $A000,$E0E0,$E0E0,$00A0
	asm DECLE $4000,$A0A0,$A0A0,$0040
	asm DECLE $C000,$A0A0,$80C0,$0080
	asm DECLE $4000,$A0A0,$A0E0,$0060
	asm DECLE $C000,$A0A0,$A0C0,$00A0
	asm DECLE $6000,$4080,$A020,$0040
	asm DECLE $E000,$4040,$4040,$0040
	asm DECLE $A000,$A0A0,$A0A0,$0060
	asm DECLE $A000,$A0A0,$A0A0,$0040
	asm DECLE $A000,$A0A0,$E0E0,$00A0
	asm DECLE $A000,$40A0,$A0A0,$00A0
	asm DECLE $A000,$A0A0,$4040,$0040
	asm DECLE $E000,$4020,$8040,$00E0
	asm DECLE $4000,$E0A0,$A0E0,$0040
	asm DECLE $4000,$40C0,$4040,$00E0
	asm DECLE $4000,$20A0,$8040,$00E0
	asm DECLE $E000,$4020,$A020,$0040
	asm DECLE $8000,$A080,$E0A0,$0020
	asm DECLE $E000,$C080,$A020,$0040
	asm DECLE $6000,$C080,$A0A0,$0040
	asm DECLE $E000,$2020,$4040,$0040
	asm DECLE $4000,$40A0,$A0A0,$0040
	asm DECLE $6000,$A0A0,$2060,$0020
	asm DECLE $0000,$0000,$0000,$0040
	asm DECLE $0000,$0000,$2000,$4020
	asm DECLE $0000,$2000,$2000,$4020
	asm DECLE $0000,$8000,$0000,$0080
	asm DECLE $0000,$E000,$0000,$0000
	asm DECLE $4000,$E040,$4040,$0000
	asm DECLE $A000,$E040,$A040,$0000
	asm DECLE $2000,$4020,$8040,$0080
	asm DECLE $4000,$4040,$0040,$0040
	asm DECLE $4000,$8040,$0000,$0000
	asm DECLE $A000,$4020,$8040,$00A0
	asm DECLE $E000,$E020,$0080,$0080
	asm DECLE $0000,$E000,$E000,$0000
	asm DECLE $2000,$8040,$2040,$0000
	asm DECLE $8000,$2040,$8040,$0000
	asm DECLE $4000,$2020,$2020,$0040
	asm DECLE $4000,$8080,$8080,$0040
	asm DECLE $C000,$8080,$8080,$00C0
	asm DECLE $6000,$2020,$2020,$0060
	asm DECLE $8000,$4080,$2040,$0020
	asm DECLE $4000,$C060,$60E0,$00C0

    asm ; Right side font character bitmap data                    
    asm @@RightFont_bitmaps:
	asm DECLE $0400,$0A0A,$0A0E,$000A
	asm DECLE $0C00,$0C0A,$0A0A,$000C
	asm DECLE $0400,$080A,$0A08,$0004
	asm DECLE $0C00,$0A0A,$0A0A,$000C
	asm DECLE $0E00,$0C08,$0808,$000E
	asm DECLE $0E00,$0C08,$0808,$0008
	asm DECLE $0400,$080A,$0A0A,$0006
	asm DECLE $0A00,$0E0A,$0A0A,$000A
	asm DECLE $0E00,$0404,$0404,$000E
	asm DECLE $0E00,$0202,$0A02,$0004
	asm DECLE $0A00,$0C0A,$0A0A,$000A
	asm DECLE $0800,$0808,$0808,$000E
	asm DECLE $0A00,$0E0E,$0A0A,$000A
	asm DECLE $0A00,$0E0E,$0E0E,$000A
	asm DECLE $0400,$0A0A,$0A0A,$0004
	asm DECLE $0C00,$0A0A,$080C,$0008
	asm DECLE $0400,$0A0A,$0A0E,$0006
	asm DECLE $0C00,$0A0A,$0A0C,$000A
	asm DECLE $0600,$0408,$0A02,$0004
	asm DECLE $0E00,$0404,$0404,$0004
	asm DECLE $0A00,$0A0A,$0A0A,$0006
	asm DECLE $0A00,$0A0A,$0A0A,$0004
	asm DECLE $0A00,$0A0A,$0E0E,$000A
	asm DECLE $0A00,$040A,$0A0A,$000A
	asm DECLE $0A00,$0A0A,$0404,$0004
	asm DECLE $0E00,$0402,$0804,$000E
	asm DECLE $0400,$0E0A,$0A0E,$0004
	asm DECLE $0400,$040C,$0404,$000E
	asm DECLE $0400,$020A,$0804,$000E
	asm DECLE $0E00,$0402,$0A02,$0004
	asm DECLE $0800,$0A08,$0E0A,$0002
	asm DECLE $0E00,$0C08,$0A02,$0004
	asm DECLE $0600,$0C08,$0A0A,$0004
	asm DECLE $0E00,$0202,$0404,$0004
	asm DECLE $0400,$040A,$0A0A,$0004
	asm DECLE $0600,$0A0A,$0206,$0002
	asm DECLE $0000,$0000,$0000,$0004
	asm DECLE $0000,$0000,$0200,$0402
	asm DECLE $0000,$0200,$0200,$0402
	asm DECLE $0000,$0800,$0000,$0008
	asm DECLE $0000,$0E00,$0000,$0000
	asm DECLE $0400,$0E04,$0404,$0000
	asm DECLE $0A00,$0E04,$0A04,$0000
	asm DECLE $0200,$0402,$0804,$0008
	asm DECLE $0400,$0404,$0004,$0004
	asm DECLE $0400,$0804,$0000,$0000
	asm DECLE $0A00,$0402,$0804,$000A
	asm DECLE $0E00,$0E02,$0008,$0008
	asm DECLE $0000,$0E00,$0E00,$0000
	asm DECLE $0200,$0804,$0204,$0000
	asm DECLE $0800,$0204,$0804,$0000
	asm DECLE $0400,$0202,$0202,$0004
	asm DECLE $0400,$0808,$0808,$0004
	asm DECLE $0C00,$0808,$0808,$000C
	asm DECLE $0600,$0202,$0202,$0006
	asm DECLE $0800,$0408,$0204,$0002
	asm DECLE $0400,$0C06,$060E,$000C

    asm ENDP                                 
REM -------------------------------------------------------------------------
REM TINYFONT - END
REM -------------------------------------------------------------------------
