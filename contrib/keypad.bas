REM Module:			KEYPAD.BAS
REM
REM Description:	Raw keypad demonstration - A demo for IntyBASIC.
REM Author:			Mark Ball
REM Date:			01/01/16
REM Version:		1.03F
REM
REM HISTORY
REM -------
REM 1.00F 13/07/15 - First release
REM 1.01F 13/07/15 - Modified instruction text.
REM 1.02F 13/07/15 - Added side buttons.
REM 1.03F 01/01/16 - Added pause buttons.
REM

	' We need some important constants.
	include "constants.bas"

	cls		' Clear the screen.	

	' Display some instructions.
	print at screenpos(0,0), "Press buttons on a"
	print at screenpos(0,1), "controller's keypad"
	print at screenpos(0,4), "Button ..."
	print at screenpos(0,5), "Value ...."
	
loop:
	wait				' Wait for the vertical blank.
	keypad=cont			' Get the raw hand controller value at this instant.
	
	print at screenpos(11,5), <3>keypad		' Display the controller response.

	' Check if a keypad button is down, 
	if keypad=0 then print at screenpos(11,4), "     ": goto loop
	
	' A keypad button is down, so display what it is.
	if keypad=KEYPAD_0 then print at screenpos(11,4), "0"
	if keypad=KEYPAD_1 then print at screenpos(11,4), "1"
	if keypad=KEYPAD_2 then print at screenpos(11,4), "2"
	if keypad=KEYPAD_3 then print at screenpos(11,4), "3"
	if keypad=KEYPAD_4 then print at screenpos(11,4), "4"
	if keypad=KEYPAD_5 then print at screenpos(11,4), "5"
	if keypad=KEYPAD_6 then print at screenpos(11,4), "6"
	if keypad=KEYPAD_7 then print at screenpos(11,4), "7"
	if keypad=KEYPAD_8 then print at screenpos(11,4), "8"
	if keypad=KEYPAD_9 then print at screenpos(11,4), "9"
	if keypad=KEYPAD_ENTER then print at screenpos(11,4), "enter"
	if keypad=KEYPAD_CLEAR then print at screenpos(11,4), "clear"
	if keypad=KEYPAD_PAUSE then print at screenpos(11,4), "pause"
	if keypad=BUTTON_1 then print at screenpos(11,4), "top"
	if keypad=BUTTON_2 then print at screenpos(11,4), "b-left"
	if keypad=BUTTON_3 then print at screenpos(11,4), "b-right"

	
	' Keep checking for keypad buttons.
	goto loop