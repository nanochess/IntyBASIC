	REM
	REM IntyLander
	REM Game demo for IntyBASIC
	REM by Oscar Toledo G. http://nanochess.org
	REM May/21/2015.
	REM

	REM Include useful predefined constants
	INCLUDE "constants.bas"

	DIM #xe(8)
	DIM #ye(8)
	DIM #vx(8)
	DIM #vy(8)

	'
	' Title screen
	'
title_screen:
	CLS
	MODE SCREEN_FOREGROUND_BACKGROUND

	FOR C = 0 TO 63
		WAIT
		PRINT AT SCREENPOS(5, 2) COLOR C AND FG_WHITE,"INTYLANDER"
	NEXT C

	PRINT AT SCREENPOS(2, 4) COLOR FG_TAN,"A MOON ADVENTURE"
	FOR C = 0 TO 20
		WAIT
	NEXT C

	PRINT AT SCREENPOS(1, 10) COLOR FG_YELLOW,"DIFFICULTY (1-4) ?"

	#score = 0

wait_title_1:
	WAIT
	IF cont.key <> 12 GOTO wait_title_1

wait_title_2:
	WAIT
	IF cont.key < 1 OR cont.key > 4 GOTO wait_title_2

	' Acceleration increases per difficulty
	accel = cont.key * 3 + 5
	level = 0

	' Bring in the moon graphics
	WAIT
	WAIT
	DEFINE DEF00,16,screen_bitmaps_0
	WAIT
	DEFINE DEF16,16,screen_bitmaps_1
	WAIT
	DEFINE DEF32,13,screen_bitmaps_2
	WAIT
	SCREEN screen_cards
	' Define some sprites
	DEFINE DEF58,6,sprites

	' New level
next_level:
	level = level + 1
	X = RAND % 152 + 8	' Random location for player
	#Y = 8 * 256
	#DY = 5
	start = 50
	#fuel = 1000 - accel * 10
	' Random landing pod
	c = RAND % 6
	IF c = 0 THEN LX = 80:LY=63
	IF c = 1 THEN LX = 88:LY=63
	IF c = 2 THEN LX = 54:LY=54
	IF c = 3 THEN LX = 32:LY=60
	IF c = 4 THEN LX = 106:LY=56
	IF c = 5 THEN LX = 148:LY=54
	SPRITE 2,LX+8 + VISIBLE + HIT,LY+8 + ZOOMY2,SPR61 + SPR_RED
	GOSUB update_score
	PLAY SIMPLE
	PLAY music_level
	IF (level AND 1)=0 THEN AY=32:IF (level AND 2)=0 THEN AX=8:AZ=1 ELSE AX=152:AZ=-1
	'
	' Main loop
	'
main_loop:
	PRINT AT SCREENPOS(0, 0) COLOR FG_GREEN,"FUEL:",<>#fuel,"  "
	SPRITE 0,X+8 + VISIBLE + HIT,#Y/256+8 + ZOOMY2,SPR63 + SPR_CYAN
	SPRITE 3,0:IF (level AND 1)=0 THEN SPRITE 3,AX+8 + VISIBLE + HIT,AY+8 + ZOOMY2,(SPR58+((FRAME AND 4)/4)*8) + SPR_GREEN:IF start=0 THEN AX=AX+AZ:IF AX=8 OR AX=152 THEN AZ=-AZ
	WAIT
	' Check for crashing
	IF PEEK(SCREENADDR((X-8+4)/8, (#Y/256)/8)) <> 0 THEN GOTO end_of_mission
	c = cont.button
	IF (c=$20)+(c=$40)+(c=$80) THEN GOTO avoid_controller
	IF cont.left THEN IF X > 0 THEN X = X - 1
	IF cont.right THEN IF X < 152 THEN X = X + 1
	' Check for using fuel
	SPRITE 1,0:IF c<>0 AND #fuel > 0 AND #DY > -200 THEN #DY = #DY - 35:#fuel = #fuel - 2:SPRITE 1,X+8 + VISIBLE + HIT,#Y/256+8+6 + ZOOMY2,SPR60 + (RAND AND 4) + SPR_RED
avoid_controller:
	IF start THEN start = start - 1: GOTO main_loop
	' Landing allowed only if falling speed isn't too high!
	IF COL0 AND HIT_SPRITE3 THEN GOTO end_of_mission
	IF (COL0 AND HIT_SPRITE2) <> 0 AND #dy < 512 THEN GOTO winning
	#Y = #Y + #DY
	if #Y < 8*256 THEN #Y = 8*256
	#DY = #DY + accel	' Gravity
	IF #fuel > 0 THEN #fuel = #fuel - 1
	GOTO main_loop

	'
	' Succesful landing!
	'
winning:
	#score = #score + 1
	SPRITE 1,0
	WHILE COL0 AND $0004
		#Y = #Y - 256
		SPRITE 0,X+8 + VISIBLE + HIT,#Y/256+8 + ZOOMY2,SPR63 + SPR_CYAN
		WAIT
	WEND
	GOSUB update_score
	#Y = #Y + 256
	SPRITE 0,X+8 + VISIBLE + HIT,#Y/256+8 + ZOOMY2,SPR63 + SPR_CYAN
	PLAY music_victory
	FOR c = 0 to 100
		WAIT
	NEXT c
	GOTO next_level

	'
	' End of mission, lander explosion!
	'
end_of_mission:
	PLAY NONE
	FOR c = 0 TO 7
	#xe(c) = X * 256
	#ye(c) = #Y + 8 * 256
	#vx(c) = (c - 4) * 256 + 64
	IF c = 3 OR c = 4 THEN #vy(c) = -512 ELSE IF c = 2 OR c = 5 THEN #vy(c) = - 440 ELSE IF c = 1 OR c = 6 THEN #vy(c) = - 380 ELSE #vy(c) = - 340
	NEXT c
	FOR d = 0 to 128
		FOR c = 0 to 7
			IF #ye(c) THEN #xe(c) = #xe(c) + #vx(c):#ye(c) = #ye(c) + #vy(c):#vy(c) = #vy(c) + 15
			IF #vx(c) < 0 THEN #vx(c) = -(-#vx(c) * 12 / 16) ELSE #vx(c) = #vx(c) * 12 / 16
			IF #ye(c) > 96 * 256 THEN #xe(c) = -8:#ye(c) = 0
		NEXT c
		SPRITE 0,#xe(0)/256+8+VISIBLE+HIT,#ye(0)/256+8+ZOOMY2,SPR62+SPR_RED
		SPRITE 1,#xe(1)/256+8+VISIBLE+HIT,#ye(1)/256+8+ZOOMY2,SPR62+SPR_RED
		SPRITE 2,#xe(2)/256+8+VISIBLE+HIT,#ye(2)/256+8+ZOOMY2,SPR62+SPR_RED
		SPRITE 3,#xe(3)/256+8+VISIBLE+HIT,#ye(3)/256+8+ZOOMY2,SPR62+SPR_RED
		SPRITE 4,#xe(4)/256+8+VISIBLE+HIT,#ye(4)/256+8+ZOOMY2,SPR62+SPR_RED
		SPRITE 5,#xe(5)/256+8+VISIBLE+HIT,#ye(5)/256+8+ZOOMY2,SPR62+SPR_RED
		SPRITE 6,#xe(6)/256+8+VISIBLE+HIT,#ye(6)/256+8+ZOOMY2,SPR62+SPR_RED
		SPRITE 7,#xe(7)/256+8+VISIBLE+HIT,#ye(7)/256+8+ZOOMY2,SPR62+SPR_RED
		SOUND 2,d*16,15-d/16
		SOUND 4,d/16+4,$18
		WAIT
	NEXT d
	SOUND 2,1,0
	SOUND 4,1,$38
	SPRITE 0,0
	SPRITE 1,0
	SPRITE 2,0
	SPRITE 3,0
	SPRITE 4,0
	SPRITE 5,0
	SPRITE 6,0
	SPRITE 7,0
end_of_mission_1:
	WAIT
	IF cont. button <> 0 THEN GOTO end_of_mission_1
end_of_mission_2:
	WAIT
	IF cont. button = 0 THEN GOTO end_of_mission_2
	GOTO title_screen

	'
	' Update score
	'
update_score:	PROCEDURE
	PRINT AT SCREENPOS(11, 0) COLOR FG_GREEN,"SCORE:",<>#score
	END

sprites:
	' Alien 1
	BITMAP "..####.."
	BITMAP "..####.."
	BITMAP "##.##.##"
	BITMAP ".######."
	BITMAP ".##..##."
	BITMAP "..####.."
	BITMAP ".##..##."
	BITMAP ".######."
	' Alien 2
	BITMAP "..####.."
	BITMAP "#.####.#"
	BITMAP ".#.##.#."
	BITMAP ".######."
	BITMAP ".#....#."
	BITMAP "..####.."
	BITMAP ".##..##."
	BITMAP "###..###"
	' Fire
	BITMAP "..####.."
	BITMAP "...###.."
	BITMAP "....#..."
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	' Landing platform
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "..####.."
	BITMAP ".#....#."
	BITMAP "###..###"
	' Debris
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "...#...."
	BITMAP "..#.#..."
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	' Lunar lander
	BITMAP "..####.."
	BITMAP ".####.##"
	BITMAP ".####.##"
	BITMAP ".#.#.#.#"
	BITMAP ".####.##"
	BITMAP ".####.##"
	BITMAP "..#..#.."
	BITMAP "..#..#.."

	' 45 bitmaps
screen_bitmaps_0:
	DATA $0000,$0000,$0000,$0300
	DATA $0000,$0700,$3F1F,$FFFF
	DATA $0000,$0000,$8000,$E0C0
	DATA $0000,$0000,$0000,$7F1F
	DATA $0000,$0000,$0000,$FFFC
	DATA $0000,$0000,$0000,$8000
	DATA $0000,$0000,$0000,$0100
	DATA $0000,$0000,$0000,$FFFF
	DATA $0000,$0101,$0F07,$7F3F
	DATA $7F1F,$FFFF,$FFFF,$FFFF
	DATA $F8F0,$FFFE,$FFFF,$FFFF
	DATA $0000,$8000,$C7BF,$FCF1
	DATA $1F0F,$FF3F,$FFFF,$7FFF
	DATA $F0E0,$FEFC,$FFFF,$FFFF
	DATA $0000,$0000,$F0C0,$FFFE
	DATA $0000,$0000,$0000,$FF00
screen_bitmaps_1:
	DATA $0000,$0100,$0F03,$FF1F
	DATA $7F3F,$FFFF,$FFFF,$FFFF
	DATA $F8E0,$FCF8,$FFFE,$FFFF
	DATA $0000,$0000,$0000,$C080
	DATA $0000,$0000,$0100,$0F07
	DATA $0F03,$7F1F,$FFFF,$FFFF
	DATA $CF3F,$FCE3,$FFFF,$FFFF
	DATA $FFFF,$FFFF,$E31F,$FFFC
	DATA $FFFF,$FFFF,$FFFF,$C33F
	DATA $FFFF,$FFFF,$FEFF,$C0E0
	DATA $FFFF,$FFFF,$00FF,$7F00
	DATA $FFFF,$FFFF,$07FF,$8000
	DATA $E0E0,$FBF1,$FCF3,$FFFE
	DATA $7F1F,$FFFF,$FFFF,$E01F
	DATA $FCFF,$FCFC,$FBF8,$FBFB
	DATA $0FF0,$FFFF,$007F,$FEFC
screen_bitmaps_2:
	DATA $F30F,$FDF9,$19E1,$7EFC
	DATA $FFFF,$FEFF,$F9FC,$FFF3
	DATA $91C7,$7F3C,$FFFF,$FFFF
	DATA $FFFF,$C03F,$BFBF,$7F3F
	DATA $F0FC,$3FC7,$CF9F,$E7EF
	DATA $7FFF,$9F3F,$F1E7,$FFFD
	DATA $FFFE,$FFFF,$FFFF,$FFFF
	DATA $FC03,$FFFF,$FFFF,$FFFF
	DATA $3FFF,$FFFF,$FFFF,$FFFF
	DATA $F7F3,$FFFF,$FFFF,$FFFF
	DATA $3F7F,$FFFF,$FFFF,$FFFF
	DATA $FF3F,$FFFF,$FFFF,$FFFF
	DATA $FF7F,$FFFF,$FFFF,$FFFF

	REM 20x12 cards
screen_cards:
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0807,$080F,$0817,$0000,$0000,$081F,$0827,$082F,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0837,$083F,$083F
	DATA $0847,$084F,$2600,$0857,$085F,$0867,$2600,$2600,$086F,$0877
	DATA $087F,$087F,$0887,$088F,$0897,$089F,$08A7,$08AF,$2600,$2600
	DATA $2600,$2600,$2600,$2600,$2600,$08B7,$08BF,$08C7,$2600,$2600
	DATA $2600,$08CF,$08D7,$08DF,$2600,$08E7,$08EF,$2600,$2600,$2600
	DATA $2600,$2600,$08F7,$08FF,$0907,$2E28,$2600,$2600,$2600,$2600
	DATA $090F,$0917,$091F,$0927,$092F,$2600,$0937,$093F,$0947,$2600
	DATA $2600,$2600,$094F,$2600,$0957,$095F,$2600,$2600,$2600,$2600
	DATA $2600,$2600,$0967,$2600,$2600,$2600,$2600,$2600,$2600,$2600

music_level:
	DATA 5
	MUSIC C4X,-,-,-
	MUSIC E4X,-,-,-
	MUSIC G4X,-,-,-
	MUSIC S,-,-,-
	MUSIC S,-,-,-
	MUSIC S,-,-,-
	MUSIC STOP

music_victory:
	DATA 5
	MUSIC F4W,-,-,-
	MUSIC F4W,-,-,-
	MUSIC F4W,-,-,-
	MUSIC S,-,-,-
	MUSIC S,-,-,-
	MUSIC S,-,-,-
	MUSIC STOP

