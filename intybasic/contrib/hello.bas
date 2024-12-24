REM *************************************************************************
REM IntyBASIC Project: HELLO
REM
REM     This is an IntyBASIC version of the classic "Hello World" program.
REM     All it does is clear the screen and display a simple message.  You
REM     can use this program as a "baseline" to make sure your tools and
REM     workflows are working as expected.
REM *************************************************************************
REM     Programmer: DZ-Jay
REM     Created:    Tue 07/07/2015
REM     Updated:    Tue 07/07/2015
REM
REM     Project automatically generated by INTYNEW.
REM *************************************************************************

	REM Include useful predefined constants
	INCLUDE "constants.bas"

REM **********************
REM INITIALIZATION
REM **********************
	REM Initialize the screen to use Color Stack
	REM Set the stack colors and load the graphic
	MODE SCREEN_CS, STACK_WHITE, STACK_WHITE, STACK_WHITE, STACK_WHITE
	DEFINE DEF00,1,graphics
	WAIT

REM **********************
REM MAIN
REM **********************
	REM Clear the screen a display a message
	CLS
	PRINT AT SCREENPOS(4, 5) COLOR CS_BLUE, "Hello World!"

	REM Display the IntyBASIC SDK logo icon
	PRINT AT SCREENPOS(9, 7), SPR00 + CS_BLACK

loop:
	REM Loop forever!
	GOTO loop

graphics:
	REM SDK Logo
	BITMAP "..#.#..."
	BITMAP ".#####.."
	BITMAP ".#.##.#."
	BITMAP ".####.#."
	BITMAP "###.##.#"
	BITMAP "###.##.#"
	BITMAP ".#.#####"
	BITMAP "...#####"