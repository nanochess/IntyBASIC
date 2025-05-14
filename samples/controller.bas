	REM
	REM Controllers test
	REM Demo for IntyBASIC.
	REM by Oscar Toledo G.  http://nanochess.org/
	REM Apr/02/2014
	REM

	REM Include useful predefined constants
	INCLUDE "constants.bas"

	CLS

loop:	WAIT
	IF cont1.up     THEN PRINT AT SCREENPOS(4, 1),"1" ELSE PRINT AT SCREENPOS(4, 1),"0"
	IF cont1.left   THEN PRINT AT SCREENPOS(3, 2),"1" ELSE PRINT AT SCREENPOS(3, 2),"0"
	IF cont1.right  THEN PRINT AT SCREENPOS(5, 2),"1" ELSE PRINT AT SCREENPOS(5, 2),"0"
	IF cont1.down   THEN PRINT AT SCREENPOS(4, 3),"1" ELSE PRINT AT SCREENPOS(4, 3),"0"
	IF cont1.b0     THEN PRINT AT SCREENPOS(3, 5),"1" ELSE PRINT AT SCREENPOS(3, 5),"0"
	IF cont1.b1     THEN PRINT AT SCREENPOS(4, 5),"1" ELSE PRINT AT SCREENPOS(4, 5),"0"
	IF cont1.b2     THEN PRINT AT SCREENPOS(5, 5),"1" ELSE PRINT AT SCREENPOS(5, 5),"0"

	a=cont1

	PRINT AT 183
	b=a/16
	GOSUB hex
	b=a
	GOSUB hex

	a=cont1.key
	PRINT AT 141,"key "
	b=a/16
	GOSUB hex
	b=a
	GOSUB hex

	IF cont2.up     THEN PRINT AT SCREENPOS(14, 1),"1" ELSE PRINT AT SCREENPOS(14, 1),"0"
	IF cont2.left   THEN PRINT AT SCREENPOS(13, 2),"1" ELSE PRINT AT SCREENPOS(13, 2),"0"
	IF cont2.right  THEN PRINT AT SCREENPOS(15, 2),"1" ELSE PRINT AT SCREENPOS(15, 2),"0"
	IF cont2.down   THEN PRINT AT SCREENPOS(14, 3),"1" ELSE PRINT AT SCREENPOS(14, 3),"0"
	IF cont2.b0     THEN PRINT AT SCREENPOS(13, 5),"1" ELSE PRINT AT SCREENPOS(13, 5),"0"
	IF cont2.b1     THEN PRINT AT SCREENPOS(14, 2),"1" ELSE PRINT AT SCREENPOS(14, 2),"0"
	IF cont2.b2     THEN PRINT AT SCREENPOS(15, 2),"1" ELSE PRINT AT SCREENPOS(15, 2),"0"

	a=cont2

	PRINT AT SCREENPOS(13, 9)
	b=a/16
	GOSUB hex
	b=a
	GOSUB hex

	a=cont2.key
	PRINT AT SCREENPOS(11, 7),"key "
	b=a/16
	GOSUB hex
	b=a
	GOSUB hex

	GOTO loop


hex:	PROCEDURE

	b = b and 15
	b = b + "0"
	if b > "9" then b=b+7
	PRINT b*8+7
	RETURN

	END
