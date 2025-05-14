	REM
	REM Scrolling technical test
	REM Demo for IntyBASIC
	REM by Oscar Toledo G. http://nanochess.org/
	REM Feb/26/2014
	REM Mar/02/2014. Added sprite sample with offset correction.
	REM

	REM Include useful predefined constants
	INCLUDE "constants.bas"

	WAIT
	DEFINE DEF00,3,graphics
	WAIT
	BORDER BORDER_BLACK, 3
	FOR ROW=0 TO 11
		GOSUB clear_row
	NEXT

	offset_x = 0
	offset_y = 0
	offset_d = 0
	WAIT

repeat:
	REM >>>> Scroll upward
	FOR time=1 to 240
		IF offset_y=7 THEN offset_d=3:offset_y=0 ELSE offset_y=offset_y+1
		SPRITE 0, 64 + VISIBLE + ZOOMX2 - offset_x, 48 + ZOOMY2 - offset_y, SPR02 + SPR_YELLOW
		SCROLL offset_x,offset_y,offset_d
		WAIT
		offset_d=0
		IF offset_y=0 THEN ROW=0:GOSUB clear_row:PRINT AT RAND%BACKGROUND_COLUMNS,"\257"
	NEXT time

	REM >>>> Scroll to left
	FOR time=1 to 240
		IF offset_x=7 THEN offset_d=1:offset_x=0 ELSE offset_x=offset_x+1
		SPRITE 0,64 + VISIBLE + ZOOMX2 - offset_x, 48 + ZOOMY2 - offset_y, SPR02 + SPR_YELLOW
		SCROLL offset_x,offset_y,offset_d
		WAIT
		offset_d=0
		IF offset_x=0 THEN COL=0:GOSUB clear_column:PRINT AT RAND%BACKGROUND_ROWS*BACKGROUND_COLUMNS,"\257"
	NEXT time

	REM >>>> Scroll downward
	FOR time=1 to 240
		IF offset_y=0 THEN offset_d=4:offset_y=7 ELSE offset_y=offset_y-1
		SPRITE 0, 64 + VISIBLE + ZOOMX2 - offset_x, 48 + ZOOMY2 - offset_y, SPR02 + SPR_YELLOW
		SCROLL offset_x,offset_y,offset_d
		WAIT
		offset_d=0
		IF offset_y=7 THEN ROW=11:GOSUB clear_row:PRINT AT SCREENPOS(RAND%BACKGROUND_COLUMNS, 10),"\257"
	NEXT time

	REM >>>> Scroll to right
	FOR time=1 to 240
		IF offset_x=0 THEN offset_d=2:offset_x=7 ELSE offset_x=offset_x-1
		SPRITE 0, 64 + VISIBLE + ZOOMX2 - offset_x, 48 + ZOOMY2 - offset_y, SPR02 + SPR_YELLOW
		SCROLL offset_x,offset_y,offset_d
		WAIT
		offset_d=0
		IF offset_x=7 THEN COL=19:GOSUB clear_column:PRINT AT RAND%BACKGROUND_ROWS*BACKGROUND_COLUMNS+19,"\257"
	NEXT time

	GOTO repeat

clear_row:	PROCEDURE
	PRINT AT ROW*BACKGROUND_COLUMNS
	PRINT "\256\256\256\256\256\256\256\256\256\256"
	PRINT "\256\256\256\256\256\256\256\256\256\256"
	RETURN
	END

clear_column:	PROCEDURE
	FOR ROW=0 TO 11
	PRINT AT SCREENPOS(COL, ROW),"\256"
	NEXT
	RETURN
	END

graphics:
	BITMAP "00001000"
	BITMAP "00001000"
	BITMAP "00001000"
	BITMAP "11111111"
	BITMAP "00001000"
	BITMAP "00001000"
	BITMAP "00001000"
	BITMAP "00001000"

	BITMAP "11111111"
	BITMAP "10101011"
	BITMAP "11010101"
	BITMAP "10101011"
	BITMAP "11010101"
	BITMAP "10101011"
	BITMAP "11010101"
	BITMAP "11111111"

	BITMAP "00111100"
	BITMAP "01111110"
	BITMAP "11111111"
	BITMAP "11111111"
	BITMAP "11111111"
	BITMAP "11111111"
	BITMAP "01111110"
	BITMAP "00111100"

