	REM
	REM Shooting letters game
	REM Demo for IntyBASIC
	REM by Oscar Toledo G. http://nanochess.org
	REM Jan/27/2014.
	REM

	REM Include useful predefined constants
	INCLUDE "constants.bas"

	X = 10
	S = 0
	PRINT AT SCREENPOS(0, 11),"--------------------"
	GOSUB update_score

	WAVE = 1
	STATE = 0

loop:	WAIT
	IF BY THEN SOUND 0,200-BY*10,15 ELSE SOUND 0,,0
	PRINT AT SCREENPOS(X, 10)," "
	IF BY THEN PRINT AT SCREENPOS(BX, BY)," ":BY=BY-1
	IF EY THEN PRINT AT SCREENPOS(EX, EY)," "
	IF FRAME AND 1 THEN GOSUB move_player
	IF FRAME AND 1 THEN GOSUB enemy
	PRINT AT SCREENPOS(X, 10) COLOR CS_GREEN,"A"
	IF EY THEN PRINT AT SCREENPOS(EX, EY) COLOR CS_TAN,"E"
	IF BY THEN PRINT AT SCREENPOS(BX, BY) COLOR CS_WHITE,"^"
	IF BY=EY AND BX=EX AND BY<>0 THEN GOSUB add_points
	GOTO loop

update_score:	PROCEDURE
	PRINT AT SCREENPOS(0, 0),"Score: ",(S/100%10+16)*8+6,(S/10%10+16)*8+6,(S%10+16)*8+6
	END

move_player:	PROCEDURE
	IF cont1.left THEN IF X>0 THEN X=X-1
	IF cont1.right THEN IF X<19 THEN X=X+1
	IF cont1.button THEN IF BY=0 THEN BY=10:BX=X
	RETURN
	END

enemy:	PROCEDURE
	IF STATE=1 GOTO move_enemy
	EY = (WAVE AND 7) + 1
	IF WAVE AND 1 THEN EX=19 ELSE EX=0
	STATE=1
	RETURN

move_enemy:
	IF WAVE AND 1 THEN IF EX=0 THEN STATE=0:EY=0:WAVE=WAVE+1 ELSE EX=EX-1
	IF NOT WAVE AND 1 THEN IF EX=19 THEN STATE=0:EY=0:WAVE=WAVE+1 ELSE EX=EX+1
	END

add_points:	PROCEDURE
	EY=0:STATE=0:S=S+1:GOSUB update_score
	SOUND 1,400,14
	WAIT
	SOUND 1,300,14
	WAIT
	SOUND 1,500,14
	WAIT
	SOUND 1,,0 ' Turn volume to zero
	END

