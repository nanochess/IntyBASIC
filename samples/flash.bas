	REM
	REM Use of JLP Flash memory
	REM Demo for IntyBASIC
	REM by Oscar Toledo G. http://nanochess.org/
	REM Aug/21/2015
	REM

	DIM #row(96)

	FLASH INIT

	PRINT AT 0, "First row: ",<>FLASH.FIRST
	PRINT AT 20, "Last row: ",<>FLASH.LAST

	PRINT AT 60,"Operations:"
	PRINT AT 80,"1> Read row"
	PRINT AT 100,"2> Write row"
	PRINT AT 120,"3> Erase sector"

main:	WAIT
	A = CONT.key
	IF A = 1 THEN GOSUB read_row
	IF A = 2 THEN GOSUB write_row
	IF A = 3 THEN GOSUB erase_sector
	IF A = 12 THEN GOTO main

debounce:
	WAIT
	A = CONT.key
	IF A <> 12 THEN GOTO debounce

	GOTO main

read_row:	PROCEDURE
	
	FLASH READ FLASH.FIRST,VARPTR #row(0)
	PRINT AT 140 COLOR 7
	FOR c = 0 TO 15
	PRINT <>#row(c),","
	NEXT c

	END

write_row:	PROCEDURE

	FOR c = 0 TO 15
	#row(c) = RANDOM(10)
	NEXT c
	PRINT AT 140 COLOR 5
	FOR c = 0 TO 15
	PRINT <>#row(c),","
	NEXT c

	FLASH WRITE FLASH.FIRST,VARPTR #row(0)
	END

erase_sector:	PROCEDURE

	PRINT AT 140
	FOR c = 0 to 99
	PRINT " "
	NEXT c

	FLASH ERASE FLASH.FIRST

	END

	
