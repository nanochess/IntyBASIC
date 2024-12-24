	REM
	REM Frame counting
	REM Demo for IntyBASIC
	REM by Oscar Toledo G. http://nanochess.org
	REM Jan/25/2015.
	REM

	REM Include useful predefined constants
	INCLUDE "constants.bas"

	ON FRAME GOSUB clock

main:
	GOTO main

clock:	PROCEDURE
	PRINT AT SCREENPOS(0, 0),<5>frame
	RETURN
	END
