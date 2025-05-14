	REM
	REM Voice with Intellivoice
	REM Demo for IntyBASIC
	REM by Oscar Toledo G. http://nanochess.org
	REM Nov/21/2014.
	REM Jul/11/2015. Voice improved by DZ-Jay
	REM Feb/05/2018. Shows how to stop voice.
	REM

	VOICE INIT
	
	FOR A=1 TO 100
		VOICE PLAY coins_detected
		FOR B=1 TO 100
			#backtab(0) = #backtab(0) + 1
			IF CONT.BUTTON THEN VOICE INIT
			WAIT
		NEXT B
		VOICE NUMBER A
	NEXT A

loop:	GOTO loop

coins_detected:
	VOICE KK1,OY,NN1,SS,PA1,PA1
	VOICE DD2,IH,TT1,EH,KK1,TT1,EH,DD1,PA1,PA2
	VOICE IH,NN1,PA1,PA1
	VOICE PP,AO,KK2,EH,TT1,PA1,PA2,0