	'
	' Example of program in banks
	'
	' by Oscar Toledo G.
	' https://nanochess.org/
	'
	' Creation date: Apr/27/2025.
	'
	
	'
	' Notice this program only can be generated as .bin+.cfg format,
	' because the ROM format doesn't support bank-switching
	' information.
	'
	OPTION MAP 3

	PRINT AT 21,"Main program"

	BANK SELECT 0

	GOSUB subroutine_0

	BANK SELECT 1

	GOSUB subroutine_1

	BANK SELECT 2

	GOSUB subroutine_2

	BANK SELECT 3

	GOSUB subroutine_3

	WHILE 1: WEND

	BANK 0

subroutine_0:	PROCEDURE
	PRINT AT 41,"Bank 0"
	END

	BANK 1

subroutine_1:	PROCEDURE
	PRINT AT 61,"Bank 1"
	END

	BANK 2

subroutine_2:	PROCEDURE
	PRINT AT 81,"Bank 2"
	END

	BANK 3

subroutine_3:	PROCEDURE
	PRINT AT 101,"Bank 3"
	END



