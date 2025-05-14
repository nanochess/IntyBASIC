REM Module:			Grom2GramFont.bas
REM
REM Description:	An example of using the GROM font when its been copied to GRAM.
REM Author(s):		Mark Ball, carlsson, nanochess (from AtariAge)
REM Date:			16/02/16
REM Version:		1.01F
REM
REM HISTORY
REM -------
REM 1.00F 15/02/16 - First release.
REM 1.01F 16/02/16 - Added support for different font styles.
REM
REM -------------------------------------------------------------------------

	' We need some important constants.
    include "constants.bas"

REM -------------------------------------------------------------------------
REM Constants.
REM -------------------------------------------------------------------------

' Font styles.
CONST FONT_IS_NORMAL	=0
CONST FONT_IS_ITALIC	=1
CONST FONT_IS_THIN		=2
CONST FONT_IS_3D        =3

' BACKTAB location of the digits.
CONST DIGITS_X			=3
CONST DIGITS_Y			=7

REM -------------------------------------------------------------------------
REM Initialisation.
REM -------------------------------------------------------------------------
	
	' Prepare the first set of font characters we want.
	gosub PrepareTheNormalFont
	
	' Put the GRAM font on screen in light blue.
    for c=0 to 36
        #BACKTAB(c)=GRAM+CS_LIGHTBLUE+(c*8)
    next c

	' Put a 2 digit number on screen and convert its colour and font to map to
	' the one we've loaded into GRAM.
	wait						' Ensure that no flickering happens.
	#junk=37
	print at SCREENPOS(DIGITS_X,DIGITS_Y) COLOR CS_BLACK, <2> #junk
	' Change 1st digit to the colour orange.
	#BACKTAB(SCREENPOS(DIGITS_X,DIGITS_Y))=#BACKTAB(SCREENPOS(DIGITS_X,DIGITS_Y))-(("0"-" ")*8)+CS_ORANGE+GRAM+(DEF26*8)
	' change 2nd digit to the colour cyan.
	#BACKTAB(SCREENPOS(DIGITS_X+1,DIGITS_Y))=#BACKTAB(SCREENPOS(DIGITS_X+1,DIGITS_Y))-(("0"-" ")*8)+CS_CYAN+GRAM+(DEF26*8)

	' Wait for a key press before continuing.
	gosub WaitForKeyDownThenUp	
	
REM -------------------------------------------------------------------------
REM Main loop
REM -------------------------------------------------------------------------	
loop:

	' Display the test data in the Italic font and wait for a key.
	gosub PrepareTheItalicFont
	gosub WaitForKeyDownThenUp

	' Display the test data in the thin font and wait for a key.
	gosub PrepareTheThinFont
	gosub WaitForKeyDownThenUp
	
	' Display the test data in the thin font and wait for a key.
	gosub PrepareThe3DFont
	gosub WaitForKeyDownThenUp
	
	' Display the test data in the normal font and wait for a key.
	gosub PrepareTheNormalFont
	gosub WaitForKeyDownThenUp
	
    goto loop

REM -------------------------------------------------------------------------
REM WaitForKeyDownThenUp - Wait for any button to and then its release.
REM -------------------------------------------------------------------------
REM
REM Input:
REM	Nothing!
REM
REM Trashes:
REM Noting!
REM
REM -------------------------------------------------------------------------
WaitForKeyDownThenUp: procedure

	' Wait for a key to be pressed.
	do while cont=0
		wait
	loop
	
	' Wait for a key to be released.
	do while cont<>0
		wait
	loop
	
	end
REM -------------------------------------------------------------------------
REM WaitForKeyDownThenUp - END
REM -------------------------------------------------------------------------
	
REM -------------------------------------------------------------------------
REM PrepareTheNormalFont - Copy the GROM cards of interest into position in GRAM.
REM -------------------------------------------------------------------------
REM
REM Notes:
REM - In IntyBASIC 1.2.4 this function generates 6 warnings due to forward
REM   references at the assembly language phase. *These are safe*, but always
REM  check the project's *.lst file (just in case).
REM - Takes 3 VBLANK ticks to execute.
REM
REM Input:
REM	Nothing!
REM
REM -------------------------------------------------------------------------
PrepareTheNormalFont: procedure

	' Copy "A" to "M" into GRAM at index DEF00.
	call COPYFONT("A",DEF00,13,FONT_IS_NORMAL)
	
	' Copy "M" to "Z" into GRAM at index DEF13.
	call COPYFONT("N",DEF13,13,FONT_IS_NORMAL)
	
	' Copy "0" to "9" into GRAM at index DEF26.
	call COPYFONT("0",DEF26,10,FONT_IS_NORMAL)
	
	end
REM -------------------------------------------------------------------------
REM PrepareTheNormalFont - END
REM -------------------------------------------------------------------------

REM -------------------------------------------------------------------------
REM PrepareTheItalicFont - Copy the GROM cards of interest into position in
REM                        GRAM.and apply an italic algorithm to them.
REM -------------------------------------------------------------------------
REM
REM Notes:
REM - In IntyBASIC 1.2.4 this function generates 8 warnings due to forward
REM   references at the assembly language phase. *These are safe*, but always
REM   check the project's *.lst file (just in case).
REM - Takes 4 VBLANK ticks to execute.
REM
REM Input:
REM	Nothing!
REM
REM -------------------------------------------------------------------------
PrepareTheItalicFont: procedure

	' Copy "A" to "L" into GRAM at index DEF00.
	call COPYFONT("A",DEF00,12,FONT_IS_ITALIC)
	
	' Copy "M" to "X" into GRAM at index DEF12.
	call COPYFONT("M",DEF12,12,FONT_IS_ITALIC)

	' Copy "Y" to "Z" into GRAM at index DEF24.
	call COPYFONT("Y",DEF24,2,FONT_IS_ITALIC)
	
	' Copy "0" to "9" into GRAM at index DEF26.
	call COPYFONT("0",DEF26,10,FONT_IS_ITALIC)
	
	end
REM -------------------------------------------------------------------------
REM PrepareTheItalicFont - END
REM -------------------------------------------------------------------------

REM -------------------------------------------------------------------------
REM PrepareTheThinFont - Copy the GROM cards of interest into position in
REM                      GRAM.and apply a "thinning" algorithm to them.
REM -------------------------------------------------------------------------
REM
REM Notes:
REM - In IntyBASIC 1.2.4 this function generates 12 warnings due to forward
REM   references at the assembly language phase. *These are safe*, but always
REM   check the project's *.lst file (just in case).
REM - Takes 6 VBLANK ticks to execute.
REM
REM Input:
REM	Nothing!
REM
REM -------------------------------------------------------------------------
PrepareTheThinFont: procedure

	' Copy "A" to "G" into GRAM at index DEF00.
	call COPYFONT("A",DEF00,7,FONT_IS_THIN)
	
	' Copy "H" to "N" into GRAM at index DEF07.
	call COPYFONT("H",DEF07,7,FONT_IS_THIN)

	' Copy "O" to "U" into GRAM at index DEF14.
	call COPYFONT("O",DEF14,7,FONT_IS_THIN)
	
	' Copy "V" to "Z" into GRAM at index DEF21.
	call COPYFONT("V",DEF21,5,FONT_IS_THIN)
	
	' Copy "0" to "6" into GRAM at index DEF26.
	call COPYFONT("0",DEF26,7,FONT_IS_THIN)

	' Copy "7" to "9" into GRAM at index DEF33.
	call COPYFONT("7",DEF33,3,FONT_IS_THIN)
	
	end
REM -------------------------------------------------------------------------
REM PrepareTheThinFont - END
REM -------------------------------------------------------------------------

REM -------------------------------------------------------------------------
REM PrepareThe3DFont - Copy the GROM cards of interest into position in
REM                    GRAM.and apply a "3D" algorithm to them.
REM -------------------------------------------------------------------------
REM
REM Notes:
REM - In IntyBASIC 1.2.4 this function generates 12 warnings due to forward
REM   references at the assembly language phase. *These are safe*, but always
REM   check the project's *.lst file (just in case).
REM - Takes 6 VBLANK ticks to execute.
REM
REM Input:
REM	Nothing!
REM
REM -------------------------------------------------------------------------
PrepareThe3DFont: procedure

	for c = 0 to 25 step 5

	call COPYFONT("A" + c, DEF00 + c, 5, FONT_IS_3D)
	
	NEXT c

	' Copy "Z" into GRAM at index DEF25.
	call COPYFONT("Z",DEF25,1,FONT_IS_3D)

	' Copy "0" to "4" into GRAM at index DEF26.
	call COPYFONT("0",DEF26,5,FONT_IS_3D)
	
	' Copy "5" to "9" into GRAM at index DEF31.
	call COPYFONT("5",DEF31,5,FONT_IS_3D)
	
	end
REM -------------------------------------------------------------------------
REM PrepareThe3DFont - END
REM -------------------------------------------------------------------------

REM -------------------------------------------------------------------------
REM COPYFONT - Copy GROM cards to GRAM.
REM -------------------------------------------------------------------------
REM
REM Notes:
REM - Takes 1 VBLANK tick to execute.
REM
REM Usage:
REM USR COPYFONT(A,B,C,D)
REM
REM Input:
REM		A - Starting card in GROM to copy data from (source).
REM 	B - Starting index card in GRAM to copy data to (destination).
REM 	C - Number of consecutive cards to copy.
REM     D - Font style (0 - normal, 1 - Italics, 2 - thin).
REM
REM Returns:
REM Nothing!
REM
REM Trashes:
REM r0-r5
REM
REM -------------------------------------------------------------------------
    asm COPYFONT: PROC
	asm pshr r5

	asm ; Find out which font data handler we need.
	asm addi #@@JumpTable, r3	; Index into the jump table.
	asm mvi@ r3, r3				; Get the pointer to the routine to execute later.
	asm pshr r3					; Stack it!
	
	asm ; Convert 1st parameter into an offset in GROM.
	asm sll r0, 2
	asm sll r0, 1			; A=A*8
	asm addi #$3000, r0		; A=$3000+(A*8)
	asm pshr r0				; Stack it!
	
	asm ; Convert 2nd parameter into an offset in GRAM.
	asm sll r1, 2
	asm sll r1, 1			; B=B*8
	asm addi #$3800, r1		; B=$3800+(B*8)
	asm pshr r1				; Stack it!
	
	asm ; Save the 3rd parameter (length) for later.
	asm pshr r2				; Stack it!
		
	asm ; Wait for VBLANK to occur.
    asm call _wait
	
	asm ; Get the card's GROM/GRAM start addresses and the number of cards
	asm ; to copy from the stack.
	asm pulr r1				; No of cards to copy.
	asm pulr r5				; Destination data pointer in GRAM.
	asm pulr r4				; Source data pointer in GROM.
	asm pulr pc				; Execute the handler!

	asm ; Jump table.
	asm @@JumpTable:
	asm     DECLE @@HandleNormal
	asm		DECLE @@HandleItalic
	asm     DECLE @@HandleThin
        asm	DECLE @@Handle3D
	
	asm ; NORMAL
	asm ; ======
	asm ; Copy the data directly from GROM to GRAM (a cards worth at a time).
	asm @@HandleNormal:
    asm @@CopyLoop1:
    asm repeat 8
    asm mvi@ r4, r0
    asm mvo@ r0, r5
    asm endr
    asm decr r1
    asm bne @@CopyLoop1
	asm pulr pc

	asm ; ITALIC
	asm ; ======
	asm ; Copy the data from GROM, apply italics and store to GRAM (a cards worth
	asm ; at a time).
	asm @@HandleItalic:
    asm @@CopyLoop2:
    asm repeat 3
    asm mvi@ r4, r0
    asm slr r0, 1
    asm mvo@ r0, r5
    asm endr
    asm repeat 2
    asm mvi@ r4, r0
    asm mvo@ r0, r5
    asm endr
    asm repeat 3
    asm mvi@ r4, r0
    asm sll r0, 1
    asm mvo@ r0, r5
    asm endr
    asm decr r1
    asm bne @@CopyLoop2	
	asm pulr pc

	asm ; THIN
	asm ; ====
	asm ; Copy the data from GROM, apply "thinning" and store to GRAM (a cards worth
	asm ; at a time).
	asm @@HandleThin:	
    asm @@CopyLoop3:
	asm repeat 8
    asm mvi@ r4, r0
    asm movr r0, r2
    asm slr r0, 1
    asm andr r2, r0
    asm mvo@ r0, r5
    asm endr	
    asm decr r1
    asm bne @@CopyLoop3	
	asm pulr pc
	
	asm ; 3D
	asm ; ====
	asm ; Copy the data from GROM, apply "3D" and store to GRAM (a cards worth
	asm ; at a time).
	asm @@Handle3D:
	asm clrr r2	
    asm @@CopyLoop4:
	asm repeat 8
    asm mvi@ r4, r0
	asm movr r0, r3
	asm andi #$f0, r0
	asm subr r0, r3
	asm xorr r2, r0
	asm movr r3, r2
    asm mvo@ r0, r5
    asm endr	
    asm decr r1
    asm bne @@CopyLoop4	
	asm pulr pc
	
	asm ENDP
REM -------------------------------------------------------------------------
REM COPYFONT - END
REM -------------------------------------------------------------------------
