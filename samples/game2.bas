	REM
	REM Barzack
	REM Demo for IntyBASIC
	REM by Oscar Toledo G. http://nanochess.org
	REM Jan/28/2014.
	REM

	REM Include useful predefined constants
	INCLUDE "constants.bas"

	WAIT
	DEFINE DEF00,4,drawings
restart:
	WAIT
	room=0
	#score=0
load_room:
	IF room = 0 THEN RESTORE room0 : #monster = SPR03 + SPR_GREEN
	IF room = 1 THEN RESTORE room1 : #monster = SPR00 + SPR_GREEN

	' Deactivate sprites
	SPRITE 0,0,0
	SPRITE 1,0,0
	SPRITE 2,0,0
	SPRITE 3,0,0
	SPRITE 4,0,0
	WAIT    ' Wait to reset collision bits

	'
	' Clean screen
	'
	CLS

	'
	' Draw current score in vertical
	'
	PRINT AT (SCREENPOS(0, 0)),(#score/100%10+16)*8+6
	PRINT AT (SCREENPOS(0, 1)),(#score/10%10+16)*8+6
	PRINT AT (SCREENPOS(0, 2)),(#score%10+16)*8+6
	PRINT COLOR CS_BLUE

	'
	' Draw room line per line
	' Extract bit per bit of value to signal labyrinth value
	'
	FOR y = 0 TO 11
		READ #line
		FOR x = 0 TO 15
			IF #line AND $8000 THEN PRINT AT y*20+x+2,"\95"
			#line = #line * 2
		NEXT x
	NEXT y

	'
	' Setup some random barzacks
	'
	y =  1: GOSUB barzack: y2 = y * 8 + 8: x2 = result
	y =  4: GOSUB barzack: y3 = y * 8 + 8: x3 = result
	y =  7: GOSUB barzack: y4 = y * 8 + 8: x4 = result
	y = 10: GOSUB barzack: y5 = y * 8 + 8: x5 = result
	
	'
	' Player start
	'
	x1 = 32
	y1 = 28

	room = room + 1
	IF room = 2 THEN room = 0

loop:
	WAIT

	'
	' Update MOBs
	'
	SPRITE 0,X1+HIT+VISIBLE,Y1+ZOOMY2,SPR02 + SPR_RED ' Our hero
	SPRITE 1,X2+HIT+VISIBLE,Y2+ZOOMY2,#monster        ' Mutant barzack
	SPRITE 2,X3+HIT+VISIBLE,Y3+ZOOMY2,#monster        ' Mutant barzack
	SPRITE 3,X4+HIT+VISIBLE,Y4+ZOOMY2,#monster        ' Mutant barzack
	SPRITE 4,X5+HIT+VISIBLE,Y5+ZOOMY2,#monster        ' Mutant barzack

	'
	' Check for collision
	'
	IF COL0 AND (HIT_SPRITE1+HIT_SPRITE2+HIT_SPRITE3+HIT_SPRITE4+HIT_SPRITE5+HIT_BACKGROUND) THEN GOTO touched
	IF COL1 AND HIT_BACKGROUND THEN Y2=0:BLINK=1
	IF COL2 AND HIT_BACKGROUND THEN Y3=0:BLINK=1
	IF COL3 AND HIT_BACKGROUND THEN Y4=0:BLINK=1
	IF COL4 AND HIT_BACKGROUND THEN Y5=0:BLINK=1

	' Check for movement
	IF CONT1.UP THEN IF Y1>0 THEN Y1=Y1-1
	IF CONT1.DOWN THEN IF Y1<104 THEN Y1=Y1+1
	IF CONT1.LEFT THEN IF X1>0 THEN X1=X1-1
	IF CONT1.RIGHT THEN IF X1<168 THEN X1=X1+1

	' Check for change of room
	IF Y1<8 OR X1<24 OR X1>144 OR Y1>96 THEN #score=#score+1:GOTO load_room

	' Background sound
	SOUND 0,2000,FRAME AND 15
	IF BLINK THEN SOUND 1,500,15:BLINK=0 ELSE SOUND 1,,0

	' Barzacks displacement
	IF FRAME AND 7 THEN GOTO loop
	IF Y2=0 THEN GOTO avoid1
	IF X1<>X2 THEN IF X1<X2 THEN X2=X2-1 ELSE X2=X2+1
	IF Y1<>Y2 THEN IF Y1<Y2 THEN Y2=Y2-1 ELSE Y2=Y2+1
avoid1:
	IF Y3=0 THEN GOTO avoid2
	IF X1<>X3 THEN IF X1<X3 THEN X3=X3-1 ELSE X3=X3+1
	IF Y1<>Y3 THEN IF Y1<Y3 THEN Y3=Y3-1 ELSE Y3=Y3+1
avoid2:
	IF Y4=0 THEN GOTO avoid3
	IF X1<>X4 THEN IF X1<X4 THEN X4=X4-1 ELSE X4=X4+1
	IF Y1<>Y4 THEN IF Y1<Y4 THEN Y4=Y4-1 ELSE Y4=Y4+1
avoid3:
	IF Y5=0 THEN GOTO avoid4
	IF X1<>X5 THEN IF X1<X5 THEN X5=X5-1 ELSE X5=X5+1
	IF Y1<>Y5 THEN IF Y1<Y5 THEN Y5=Y5-1 ELSE Y5=Y5+1
avoid4:
	GOTO loop

touched:

	FOR color = 0 TO 31
		WAIT
		SPRITE 0,X1+HIT+VISIBLE,Y1+ZOOMY2,SPR02+(color AND SPR_WHITE) ' Our hero
		SOUND 0,(color and 7)*32+32,15
		SOUND 1,(color and 7)*36+32,15
		SOUND 2,(color and 7)*40+32,15
	NEXT color
	SOUND 0,,0
	SOUND 1,,0
	SOUND 2,,0

	FOR loop = 0 to 100
		WAIT
	NEXT loop

	' Deactivate sprites
	SPRITE 0,0,0
	SPRITE 1,0,0
	SPRITE 2,0,0
	SPRITE 3,0,0
	SPRITE 4,0,0

	GOTO restart

	'
	' Locate a free space for a barzack
	' Input: y = Screen row
	'
barzack:	PROCEDURE

	DO
		WAIT
		X = RAND % 16
	LOOP WHILE #BACKTAB(Y * BACKGROUND_COLUMNS + X + 2)

	result = (x + 2) * 8 + 8

	END

	'
	' Definition of rooms
	'
	' Uses binary numbers to form a 16-bit value
	' (the maximum fitting a DATA value)
	'
room0:
	DATA &1111111111111111
	DATA &1000000000000001
	DATA &1000000000000001
	DATA &1000011111100001
	DATA &1000000000100001
	DATA &1000000000100000
	DATA &1000000000100000
	DATA &1000000000100001
	DATA &1000011111100001
	DATA &1000000000000001
	DATA &1000000000000001
	DATA &1111111111111111

room1:
	DATA &1111111111111111
	DATA &1000000000000001
	DATA &1000000000000001
	DATA &1000010000100001
	DATA &1000010000100001
	DATA &1000010000100001
	DATA &1000010000100001
	DATA &1000010000100001
	DATA &1000010000100001
	DATA &1000000000000001
	DATA &1000000000000001
	DATA &1111110000111111

	'
	' Bitmaps used in the game
	'
drawings:
	BITMAP "#..##..#"
	BITMAP "########"
	BITMAP ".#.##.#."
	BITMAP ".######."
	BITMAP "..####.."
	BITMAP "..#..#.."
	BITMAP "..#..#.."
	BITMAP ".##..##."

	BITMAP "...##..."
	BITMAP "...##..."
	BITMAP "..####.."
	BITMAP "..#..#.."
	BITMAP "..####.."
	BITMAP "..####.."
	BITMAP ".######."
	BITMAP "##.##.##"

	BITMAP "...##..."
	BITMAP "...##..."
	BITMAP ".######."
	BITMAP "...##..."
	BITMAP "...##..."
	BITMAP "..####.."
	BITMAP "..#..#.."
	BITMAP ".##..##."

	BITMAP ".##....."
	BITMAP "###....."
	BITMAP ".##....."
	BITMAP ".#######"
	BITMAP ".######."
	BITMAP ".##..##."
	BITMAP ".##..##."
	BITMAP "###.###."
