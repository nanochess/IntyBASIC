	'
	' Example of program in segments
	'
	' by Oscar Toledo G.
	' https://nanochess.org
	'
	' Creation date: Apr/27/2025.
	'

	OPTION MAP 1

	GOSUB subroutine_1
	GOSUB subroutine_2

	PRINT AT 21,"Main segment"

	WHILE 1: WEND

	SEGMENT 1

subroutine_1:	PROCEDURE
	PRINT AT 41,"Segment 1"
	END

	SEGMENT 2

subroutine_2:	PROCEDURE
	PRINT AT 61,"Segment 2"
	END

