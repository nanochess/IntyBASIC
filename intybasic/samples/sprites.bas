	REM
	REM Example MOBs (sprites)
	REM Demo for IntyBASIC
	REM by Oscar Toledo G. http://nanochess.org
	REM Jan/27/2014
	REM Jul/12/2015. Added IntyBASIC logo (DZ-Jay) and spaceship now visible (hidden before)
	REM Jul/13/2015. Enhanced spaceship graphics.
	REM

	REM Include useful predefined constants
	INCLUDE "constants.bas"

	WAIT
	DEFINE DEF00,5,drawings
	WAIT
	x1=80
	y1=56

loop:
	FOR X=1 TO 80
		WAIT
		SPRITE 0, ( 40+X) + VISIBLE + ZOOMX2,     20 + ZOOMY2, SPR00 + SPR_WHITE
		SPRITE 1, (120-X) + VISIBLE + ZOOMX2,     60 + ZOOMY2, SPR01 + SPR_BLUE
		SPRITE 2,      20 + VISIBLE + ZOOMX2, ( 8+X) + ZOOMY2, SPR02 + SPR_RED
		SPRITE 3,     140 + VISIBLE + ZOOMX2, (96-X) + ZOOMY2, SPR03 + SPR_TAN
		SPRITE 4,      X1 + VISIBLE + ZOOMX2, (Y1-8) + ZOOMY4, SPR04 + SPR_GREEN
		GOSUB test
	NEXT X

	FOR X=80 TO 1 STEP -1
		WAIT
		SPRITE 0, ( 40+X) + VISIBLE + ZOOMX2,     20 + ZOOMY2, SPR00 + SPR_WHITE
		SPRITE 1, (120-X) + VISIBLE + ZOOMX2,     60 + ZOOMY2, SPR01 + SPR_BLUE
		SPRITE 2,      20 + VISIBLE + ZOOMX2, ( 8+X) + ZOOMY2, SPR02 + SPR_RED
		SPRITE 3,     140 + VISIBLE + ZOOMX2, (96-X) + ZOOMY2, SPR03 + SPR_TAN
		SPRITE 4,      X1 + VISIBLE + ZOOMX2, (Y1-8) + ZOOMY4, SPR04 + SPR_GREEN
		GOSUB test
	NEXT X

	GOTO loop

test:	PROCEDURE
	IF CONT1.UP THEN IF Y1>0 THEN Y1=Y1-1
	IF CONT1.DOWN THEN IF Y1<104 THEN Y1=Y1+1
	IF CONT1.LEFT THEN IF X1>0 THEN X1=X1-1
	IF CONT1.RIGHT THEN IF X1<168 THEN X1=X1+1
	IF CONT1.BUTTON THEN SOUND 0,100,15 ELSE SOUND 0,,0 ' beeper
	END

drawings:
	REM Invader
	BITMAP "#..##..#"
	BITMAP "########"
	BITMAP ".#.##.#."
	BITMAP ".######."
	BITMAP "..####.."
	BITMAP "..#..#.."
	BITMAP "..#..#.."
	BITMAP ".##..##."

	REM Spaceship
	BITMAP "...##..."
	BITMAP "...##..."
	BITMAP "..####.."
	BITMAP "..#..#.."
	BITMAP "..####.."
	BITMAP "..####.."
	BITMAP ".######."
	BITMAP "##.##.##"

	REM Stickman
	BITMAP "...##..."
	BITMAP "...##..."
	BITMAP ".######."
	BITMAP "...##..."
	BITMAP "...##..."
	BITMAP "..####.."
	BITMAP "..#..#.."
	BITMAP ".##..##."

	REM Lama
	BITMAP ".##....."
	BITMAP "###....."
	BITMAP ".##....."
	BITMAP ".#######"
	BITMAP ".######."
	BITMAP ".##..##."
	BITMAP ".##..##."
	BITMAP "###.###."

	REM SDK Logo
	BITMAP "..#.#..."
	BITMAP ".#####.."
	BITMAP ".#.##.#."
	BITMAP ".####.#."
	BITMAP "###.##.#"
	BITMAP "###.##.#"
	BITMAP ".#.#####"
	BITMAP "...#####"
