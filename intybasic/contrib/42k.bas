REM Module:			42K.BAS
REM
REM Description:	A template for creating the largest ROM possible (42K DECLES)
REM                 without using bank switching.
REM Author:			Mark Ball
REM Date:			20/08/15
REM Version:		1.00F
REM
REM HISTORY
REM -------
REM 1.00F 20/08/15 - First release
REM 1.01F 20/08/15 - Moved ROM segments around so that IntyBASIC epilogue
REM                  code will be appended in a segment with only compiled
REM                  IntyBASIC code in it.
REM 1.02F 21/08/15 - Fixed procedure name typo.
REM

REM -------------------------------------------------------------------------
REM 42K ROM memory map
REM -------------------------------------------------------------------------
REM SEGMENT ADDRESS RANGE  SIZE  NOTES
REM ======= ============== ===== =====
REM    0    $5000 to $6FFF $2000 - Default segment
REM    1    $A000 to $BFFF $2000
REM    2    $C040 to $FFFF $3FC0 - Avoid STIC aliasing.
REM    3    $2100 to $2FFF $0F00 
REM    4    $7100 to $7FFF $0F00 - Avoid EXEC's ECS ROM probe.
REM    5    $4810 to $4FFF $0800 - Account for game's ECS ROM mapping out code.
REM -------------------------------------------------------------------------

	' We need some important constants.
	include "CONSTANTS.BAS"

REM -------------------------------------------------------------------------
REM START OF ROM SEGMENT 0 ($5000 to $6FFF) - Default
REM -------------------------------------------------------------------------
	sound 5,0		' Ensure ECS ROMs are mapped out.
	
	cls		' Clear the screen.
	
	gosub PROC0	' This procedure is located in segment 0.	
	gosub PROC1	' This procedure is located in segment 1.
	gosub PROC2	' This procedure is located in segment 2.
	gosub PROC3	' This procedure is located in segment 3.
	gosub PROC4	' This procedure is located in segment 4.
	gosub PROC5	' This procedure is located in segment 5.
	
loop:
	wait
	goto loop

PROC0: PROCEDURE
	print at screenpos(0,0), "This is segment 0"
	return
	END	
	
REM -------------------------------------------------------------------------
REM END OF ROM SEGMENT 0
REM -------------------------------------------------------------------------
	
REM -------------------------------------------------------------------------
REM START OF ROM SEGMENT 1 ($A000 to $BFFF).
REM -------------------------------------------------------------------------
	asm org $A000
	
PROC1: PROCEDURE
	print at screenpos(0,1), "This is segment 1"
	return
	END

REM -------------------------------------------------------------------------
REM END OF ROM SEGMENT 1
REM -------------------------------------------------------------------------
	
REM -------------------------------------------------------------------------
REM START OF ROM SEGMENT 2 ($C040 to $FFFF).
REM -------------------------------------------------------------------------
	asm org $C040
	
PROC2: PROCEDURE
	print at screenpos(0,2), "This is segment 2"
	return
	END

REM -------------------------------------------------------------------------
REM END OF ROM SEGMENT 2
REM -------------------------------------------------------------------------
	
REM -------------------------------------------------------------------------
REM START OF ROM SEGMENT 3 ($2100 to $2FFF).
REM -------------------------------------------------------------------------
	asm org $2100
	
PROC3: PROCEDURE
	print at screenpos(0,3), "This is segment 3"
	return
	END

REM -------------------------------------------------------------------------
REM END OF ROM SEGMENT 3
REM -------------------------------------------------------------------------
	
REM -------------------------------------------------------------------------
REM START OF ROM SEGMENT 5 ($4810 to $4FFF).
REM -------------------------------------------------------------------------
	asm org $4810
	
PROC5: PROCEDURE
	print at screenpos(0,5), "This is segment 5"
	return
	END
	
REM -------------------------------------------------------------------------
REM END OF ROM SEGMENT 5
REM -------------------------------------------------------------------------

REM -------------------------------------------------------------------------
REM START OF ROM SEGMENT 4 ($7100 to $7FFF).
REM Notes
REM -----
REM 1) Keep this as the last segment so that the IntyBASIC epilogue code is
REM appended to the end of this segment. 
REM -------------------------------------------------------------------------
	asm org $7100
	
PROC4: PROCEDURE
	print at screenpos(0,4), "This is segment 4"
	return
	END

REM -------------------------------------------------------------------------
REM END OF ROM SEGMENT 4
REM -------------------------------------------------------------------------
