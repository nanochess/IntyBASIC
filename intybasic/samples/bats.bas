	REM
	REM Bats
	REM Demo for IntyBASIC
	REM by Oscar Toledo G. http://nanochess.org/
	REM Sep/01/2015
	REM

	' Note there is no use of labels, thanks to the use of the structured loops

	include "constants.bas"

	DIM x(8),y(8),f(8),dx(8)

	' Put in random coordinate each bat
        FOR a = 0 TO 7
        x(a) = RANDOM(152) + 8
        y(a) = RANDOM(48) + 24
	f(a) = $2d		' Uses M and W letter ($2d and $37), V for attack ($36)
	dx(a) = 1
	NEXT a

	bats = 8
	
	' Main loop
	DO
		' Show bats
		FOR a = 0 TO 7
			SPRITE a,x(a)+VISIBLE,y(a),f(a)*8+(a AND 1)*3+2
		NEXT a
		WAIT
		' Move bats
		FOR a = 0 TO 7
			IF f(a) = $36 THEN	' Attacking?
				IF y(a) < 104 THEN y(a) = y(a) + 1:IF y(a) > 103 THEN bats = bats - 1
			ELSE
				' Bats goes left to right and back
				x(a) = x(a) + dx(a)
				IF x(a) = 8 OR x(a) = 160 THEN dx(a) = -dx(a)
				' Vertical movement is random (-2 to +2)
				y(a) = y(a) + RANDOM(5) - 2
				IF y(a) < 8 THEN y(a) = 8
				IF y(a) > 80 THEN y(a) = 80
				' Move wings each 4 frames
				IF (FRAME AND 3) = 0 THEN IF f(a) = $2D THEN f(a) = $37 ELSE f(a) = $2d
			END IF
		NEXT a
		IF (FRAME AND 255) = 0 THEN	' Each 256 frames, a bat attacks
			FOR a = 0 TO 7
				IF f(a) <> $36 THEN f(a) = $36:EXIT FOR
			NEXT a
		END IF
	LOOP UNTIL bats = 0

	WAIT

        FOR a = 0 TO 7
		SPRITE a,0
	NEXT a
