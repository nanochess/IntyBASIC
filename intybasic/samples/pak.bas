	REM
	REM IntyPak
	REM Game demo for IntyBASIC
	REM by Oscar Toledo G. http://nanochess.org
	REM Jun/03/2015.
	REM

	REM Include useful predefined constants
	INCLUDE "constants.bas"

	CONST max_ghosts = 2
	MODE SCREEN_FOREGROUND_BACKGROUND
	#record = 50

	'
	' The awesome title screen
	'
title_screen:
	CLS
	WAIT
	DEFINE DEF00,16,screen_bitmaps_0
	WAIT
	DEFINE DEF16,16,screen_bitmaps_1
	WAIT
	DEFINE DEF32,6,screen_bitmaps_2
	WAIT
	SCREEN screen_cards

	PRINT AT SCREENPOS(3, 6) COLOR CS_GREEN,"RECORD: "
	PRINT COLOR CS_TAN,<5>#record,"0"
	PRINT AT SCREENPOS(4, 9) COLOR CS_GREEN,"PRESS BUTTON"
title_screen_loop_1:
	WAIT
	IF cont.button THEN GOTO title_screen_loop_1
title_screen_loop_2:
	WAIT
	IF cont.button = 0 THEN GOTO title_screen_loop_2

	FOR c = 0 TO 10
		WAIT
		WAIT
		PRINT AT SCREENPOS(4, 9) COLOR CS_GREEN,"PRESS BUTTON"
		WAIT
		WAIT
		PRINT AT SCREENPOS(4, 9) COLOR CS_GREEN,"            "
	NEXT c

	'
	' Load graphics for level
	'
	CLS
	DEFINE DEF00,16,bitmaps
	WAIT
	DEFINE DEF16,3,bitmaps_2
	WAIT
	level = 0
	#score = 0
	lives = 2

	DIM x(max_ghosts),y(max_ghosts),d(max_ghosts)

	'
	' Start next level
	'
next_level:
	level = level + 1
	SCREEN level1
	GOSUB update_lives
	GOSUB update_score
	PRINT AT SCREENPOS(19, 10) COLOR CS_DARKGREEN,(level/10%10+16)*8+$0003
	PRINT AT SCREENPOS(19, 11) COLOR CS_DARKGREEN,(level%10+16)*8+$0003

	'
	' Count points in screen
	'
	points = 0
	FOR c = SCREENPOS(0, 0) to SCREENPOS(19, 11)
		IF #BACKTAB(c) = BG06 + FG_YELLOW THEN points = points + 1
	NEXT c

restart_level:
	'
	' Changing starting position for ghosts
	'
	ON level AND 3 GOTO start_0,start_1,start_2,start_3

start_0:
	x(0) = 8
	y(0) = 8
	d(0) = 0
	x(1) = 136
	y(1) = 8
	d(1) = 1
	GOTO start_4

start_1:
	x(0) = 136
	y(0) = 8
	d(0) = 1
	x(1) = 136
	y(1) = 80
	d(1) = 1
	GOTO start_4

start_2:
	x(0) = 136
	y(0) = 80
	d(0) = 1
	x(1) = 8
	y(1) = 80
	d(1) = 0
	GOTO start_4

start_3:
	x(0) = 8
	y(0) = 80
	d(0) = 0
	x(1) = 8
	y(1) = 8
	d(1) = 0
start_4:
	X1 = 64
	Y1 = 56
	face = 0
	dir = 0	' 0=right, 1=left, 2=upwards, 3=downwards
	next_dir = 0
	fine_speed = 0
	ping = 0
	SPRITE 0, VISIBLE+HIT+8+X1,ZOOMY2+8+Y1,SPR09+SPR_GREEN
	SPRITE 1,0
	SPRITE 2,0
	SPRITE 3,0
	SPRITE 4,0
	FOR c = 0 TO 63
		WAIT
	NEXT c
	'
	' Main game loop
	'
loop:
	WAIT
	' Eating pill
	IF ping THEN SOUND 1,120+ping*4,6+ping/2:ping=ping-1 ELSE SOUND 1,1,0
	' Background arcade-style sound
	SOUND 2,wave((frame AND 15)/2),10
	' Collision detection
	IF COL0 AND (HIT_SPRITE1+HIT_SPRITE2) THEN GOTO player_touched
	WAIT
	' Player
	SPRITE 0,HIT+VISIBLE+8+X1,ZOOMY2+8+Y1,GRAM+(8+face*2+(FRAME AND 8)/8)*8+SPR_GREEN
	' Ghost 1
	SPRITE 1,HIT+VISIBLE+8+X(0),ZOOMY2+8+Y(0),GRAM+(16+(FRAME AND 8)/8)*8+SPR_RED
	' Ghost 2
	SPRITE 2,HIT+VISIBLE+8+X(1),ZOOMY2+8+Y(1),GRAM+(16+(FRAME AND 8)/8)*8+SPR_BLUE
	' Player speed reduces with each level
	IF level > 7 THEN c = $49 - 8 ELSE c = $49 - level
	' Fine movement for player
	fine_speed = fine_speed + c
	WHILE fine_speed >= $40
	fine_speed = fine_speed - $40
	c = cont.button
	IF (c = $20)+(c = $40)+(c = $80) THEN GOTO avoid_controller
	IF cont.right THEN next_dir = 0
	IF cont.left THEN next_dir = 1
	IF cont.up THEN next_dir = 2
	IF cont.down THEN next_dir = 3
avoid_controller:
	' Only change direction in card boundary
	IF (X1 AND 7)+(Y1 AND 7) THEN GOTO loop_moving
	' Check if pill eaten
	IF PEEK(SCREENADDR(X1/8, Y1/8)) = (BG06+FG_YELLOW) THEN ping = 7:PRINT AT (SCREENPOS(X1/8, Y1/8)),0:#score = #score + 1:GOSUB update_score:points = points - 1:IF points = 0 THEN GOTO level_won
	dir = next_dir
	face = next_dir
	' Check if can move in desired direction
	IF dir = 0 THEN #c = PEEK($0201+X1/8+Y1/8*20):IF #c <> (BG06+FG_YELLOW) AND #c <> 0 THEN dir = 4
	IF dir = 1 THEN #c = PEEK($01FF+X1/8+Y1/8*20):IF #c <> (BG06+FG_YELLOW) AND #c <> 0 THEN dir = 4
	IF dir = 2 THEN #c = PEEK($01EC+X1/8+Y1/8*20):IF #c <> (BG06+FG_YELLOW) AND #c <> 0 THEN dir = 4
	IF dir = 3 THEN #c = PEEK($0214+X1/8+Y1/8*20):IF #c <> (BG06+FG_YELLOW) AND #c <> 0 THEN dir = 4
loop_moving:
	IF dir = 0 THEN X1 = X1 + 1
	IF dir = 1 THEN X1 = X1 - 1
	IF dir = 2 THEN Y1 = Y1 - 1
	IF dir = 3 THEN Y1 = Y1 + 1
	WEND
	' Ghosts movement
	FOR c = 0 TO max_ghosts-1
	' Only change direction in card boundary
	IF (x(c) AND 7)+(y(c) AND 7) THEN GOTO loop_moving2
	' Check available directions
	#offset = x(c)/8+y(c)/8*20
	move = 0
	#c = PEEK($0201+#offset)
	IF #c <> (BG06+FG_YELLOW) AND #c <> 0 THEN move = move + 1
	#c = PEEK($01ff+#offset)
	IF #c <> (BG06+FG_YELLOW) AND #c <> 0 THEN move = move + 2
	#c = PEEK($01EC+#offset)
	IF #c <> (BG06+FG_YELLOW) AND #c <> 0 THEN move = move + 4
	#c = PEEK($0214+#offset)
	IF #c <> (BG06+FG_YELLOW) AND #c <> 0 THEN move = move + 8
	ON d(c) GOTO loop_moving_horiz,loop_moving_horiz,loop_moving_vert,loop_moving_vert

	' If it was moving horizontally, go now for vertical
loop_moving_horiz:
	IF y1 = y(c) AND (move AND 3) = 0 THEN GOTO loop_moving2
	IF y1 < y(c) THEN GOTO loop_moving_horiz_1
	IF (move AND 8) = 0 THEN d(c) = 3:GOTO loop_moving2
loop_moving_horiz_1:
	IF (move AND 4) = 0 THEN d(c) = 2:GOTO loop_moving2
	IF (move AND 8) = 0 THEN d(c) = 3
	GOTO loop_moving2

	' If it was moving vertically, go now for horizontal
loop_moving_vert:
	IF x1 = x(c) AND (move AND 12) = 0 THEN GOTO loop_moving2
	IF x1 < x(c) THEN GOTO loop_moving_vert_1
	IF (move AND 1) = 0 THEN d(c) = 0:GOTO loop_moving2
loop_moving_vert_1:
	IF (move AND 2) = 0 THEN d(c) = 1:GOTO loop_moving2
	IF (move AND 1) = 0 THEN d(c) = 0
	GOTO loop_moving2

	' Displace ghost
loop_moving2:
	IF d(c) = 0 THEN x(c) = x(c) + 1
	IF d(c) = 1 THEN x(c) = x(c) - 1
	IF d(c) = 2 THEN y(c) = y(c) - 1
	IF d(c) = 3 THEN y(c) = y(c) + 1
	NEXT c
	GOTO loop

	'
	' Level won
	'
level_won:
	SOUND 1,1,0
	SOUND 2,1,0
	SPRITE 0,0
	SPRITE 1,0
	SPRITE 2,0
	SPRITE 3,0
	SPRITE 4,0
	' Classic-style flashing
	FOR d = 0 to 9
		WAIT
		WAIT
		WAIT
		WAIT
		WAIT
		FOR c = SCREENPOS(0, 0) to SCREENPOS(19, 11)
			#BACKTAB(c) = #BACKTAB(c) XOR FG_YELLOW
		NEXT c
	NEXT d
	GOTO next_level

	'
	' Player touched by ghosts
	'
player_touched:
	SOUND 1,1,0
	FOR c = 0 to 63
		WAIT
		SOUND 2,128-c,10	' Sound effect
		SPRITE 0,VISIBLE+HIT+8+X1,ZOOMY2+8+Y1,(10+(c AND 8)/8)*8+(c AND 7)
		SPRITE 1,0
		SPRITE 2,0
		SPRITE 3,0
		SPRITE 4,0
	NEXT c
	SOUND 2,1,0
	SPRITE 0,0
	FOR c = 0 TO 63
		WAIT
	NEXT c
	IF lives = 0 THEN GOTO game_over
	lives = lives - 1
	GOSUB update_lives
	GOTO restart_level

game_over:
	PRINT AT SCREENPOS(4, 5) COLOR FG_WHITE," GAME OVER "
	FOR c = 0 to 200
		WAIT
	NEXT c
	GOTO title_screen

update_lives:	PROCEDURE
	c = 0
	WHILE c < lives
		PRINT AT c * 40 + 19,18*8+$0805
		c = c + 1
	WEND
	WHILE c < 5
	PRINT AT c * 40 + 19,0
	c = c + 1
	WEND
	END

update_score:	PROCEDURE
	IF #score > #record THEN #record = #score
	PRINT AT SCREENPOS(0, 11) COLOR FG_TAN,<5>#score,"0"
	END

	' Background bass sound
wave:
	DATA 800,1000,1200,1400,1400,1200,1000,800

	' Bitmaps for labyrinth
bitmaps:
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP ".....###"
	BITMAP "....#..."
	BITMAP "...#...."
	BITMAP "...#...."
	BITMAP "...#...."

	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "########"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"

	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "##......"
	BITMAP "..#....."
	BITMAP "...#...."
	BITMAP "...#...."
	BITMAP "...#...."

	BITMAP "...#...."
	BITMAP "...#...."
	BITMAP "...#...."
	BITMAP "...#...."
	BITMAP "...#...."
	BITMAP "...#...."
	BITMAP "...#...."
	BITMAP "...#...."

	BITMAP "...#...."
	BITMAP "...#...."
	BITMAP "....#..."
	BITMAP ".....###"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"

	BITMAP "...#...."
	BITMAP "...#...."
	BITMAP "..#....."
	BITMAP "##......"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"

	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "...##..."
	BITMAP "...##..."
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"

	BITMAP "........"
	BITMAP "........"
	BITMAP "...##..."
	BITMAP "..####.."
	BITMAP "..####.."
	BITMAP "...##..."
	BITMAP "........"
	BITMAP "........"

	' Card 8 player bitmaps

	BITMAP "..####.."
	BITMAP ".######."
	BITMAP "####..##"
	BITMAP "########"
	BITMAP "########"
	BITMAP "########"
	BITMAP ".######."
	BITMAP "..####.."

	BITMAP "..####.."
	BITMAP ".######."
	BITMAP "####..##"
	BITMAP "######.."
	BITMAP "####...."
	BITMAP "######.."
	BITMAP ".######."
	BITMAP "..####.."

	BITMAP "..####.."
	BITMAP ".######."
	BITMAP "##..####"
	BITMAP "########"
	BITMAP "########"
	BITMAP "########"
	BITMAP ".######."
	BITMAP "..####.."

	BITMAP "..####.."
	BITMAP ".######."
	BITMAP "##..####"
	BITMAP "..######"
	BITMAP "....####"
	BITMAP "..######"
	BITMAP ".######."
	BITMAP "..####.."

	BITMAP "..####.."
	BITMAP ".######."
	BITMAP "##.#####"
	BITMAP "##.#####"
	BITMAP "########"
	BITMAP "########"
	BITMAP ".######."
	BITMAP "..####.."

	BITMAP "..#....."
	BITMAP ".##...#."
	BITMAP "##.#.###"
	BITMAP "##.#.###"
	BITMAP "########"
	BITMAP "########"
	BITMAP ".######."
	BITMAP "..####.."

	BITMAP "..####.."
	BITMAP ".######."
	BITMAP "########"
	BITMAP "########"
	BITMAP "#####.##"
	BITMAP "#####.##"
	BITMAP ".######."
	BITMAP "..####.."

	BITMAP "..####.."
	BITMAP ".######."
	BITMAP "########"
	BITMAP "########"
	BITMAP "###.#.##"
	BITMAP "###.#.##"
	BITMAP ".#...##."
	BITMAP ".....#.."

	' Card #6. Ghosts bitmap
bitmaps_2:
	BITMAP "..####.."
	BITMAP ".######."
	BITMAP "##.##.##"
	BITMAP "########"
	BITMAP "########"
	BITMAP "########"
	BITMAP "########"
	BITMAP "#.#.#.#."

	BITMAP "..####.."
	BITMAP ".######."
	BITMAP "##.##.##"
	BITMAP "########"
	BITMAP "########"
	BITMAP "########"
	BITMAP "########"
	BITMAP ".#.#.#.#"

	BITMAP ".#####.."
	BITMAP "#######."
	BITMAP "#..#..#."
	BITMAP "#######."
	BITMAP "#.###.#."
	BITMAP ".#.#.#.."
	BITMAP "..###..."
	BITMAP "........"

	' Labyrinth
level1:
	DATA $0801,$0809,$0809,$0809,$0809,$0811,$0801,$0809,$0809,$0809
	DATA $0809,$0809,$0811,$0801,$0809,$0809,$0809,$0809,$0811,$0000

	DATA $0819,$0836,$0836,$0836,$0836,$0821,$0829,$0836,$0836,$0836
	DATA $0836,$0836,$0821,$0829,$0836,$0836,$0836,$0836,$0819,$0000

	DATA $0819,$0836,$0801,$0811,$0836,$0836,$0836,$0836,$0801,$0809
	DATA $0811,$0836,$0836,$0836,$0836,$0801,$0811,$0836,$0819,$0000

	DATA $0819,$0836,$0821,$0829,$0836,$0801,$0811,$0836,$0821,$0809
	DATA $0829,$0836,$0801,$0811,$0836,$0821,$0829,$0836,$0819,$0000

	DATA $0819,$0836,$0836,$0836,$0836,$0821,$0829,$0836,$0836,$0836
	DATA $0836,$0836,$0821,$0829,$0836,$0836,$0836,$0836,$0819,$0000

	DATA $0821,$0809,$0809,$0811,$0836,$0836,$0836,$0836,$0801,$0809
	DATA $0811,$0836,$0836,$0836,$0836,$0801,$0809,$0809,$0829,$0000

	DATA $0801,$0809,$0809,$0829,$0836,$0801,$0811,$0836,$0821,$0809
	DATA $0829,$0836,$0801,$0811,$0836,$0821,$0809,$0809,$0811,$0000

	DATA $0819,$0836,$0836,$0836,$0836,$0821,$0829,$0836,$0836,$0836
	DATA $0836,$0836,$0821,$0829,$0836,$0836,$0836,$0836,$0819,$0000

	DATA $0819,$0836,$0801,$0811,$0836,$0836,$0836,$0836,$0801,$0809
	DATA $0811,$0836,$0836,$0836,$0836,$0801,$0811,$0836,$0819,$0000

	DATA $0819,$0836,$0821,$0829,$0836,$0801,$0811,$0836,$0821,$0809
	DATA $0829,$0836,$0801,$0811,$0836,$0821,$0829,$0836,$0819,$0000

	DATA $0819,$0836,$0836,$0836,$0836,$0819,$0819,$0836,$0836,$0836
	DATA $0836,$0836,$0819,$0819,$0836,$0836,$0836,$0836,$0819,$0000

	DATA $0821,$0809,$0809,$0809,$0809,$0829,$0821,$0809,$0809,$0809
	DATA $0809,$0809,$0829,$0821,$0809,$0809,$0809,$0809,$0829,$0000

	' Logo

	' 38 bitmaps
screen_bitmaps_0:
	DATA $FFFF,$FFFF,$FFFF,$E0FF
	DATA $FFFF,$FFFF,$FFFF,$0FFF
	DATA $FFFF,$FFFF,$FCFF,$FCFC
	DATA $FFFF,$FFFF,$1F9F,$0F0F
	DATA $FFFF,$FCFF,$F0FC,$FFF7
	DATA $EFFF,$00E0,$0300,$F3F3
	DATA $FFFF,$1F1F,$FF7F,$FBFF
	DATA $FFFF,$FFFF,$F3FB,$E3E3
	DATA $FEFE,$F9F8,$F3F3,$F3F3
	DATA $0000,$FF0F,$FFFF,$FEFF
	DATA $070F,$E7E7,$C7E7,$0F87
	DATA $FEFE,$FFFE,$FFFF,$FFFF
	DATA $0707,$0303,$8103,$8181
	DATA $FFFF,$CFCF,$C1C3,$E0C0
	DATA $FCF8,$FCFC,$FCFC,$1C7C
	DATA $F3F3,$7971,$7979,$7879
screen_bitmaps_1:
	DATA $F1F1,$F0F0,$F8F0,$F8F8
	DATA $C7E3,$CFC7,$8F8F,$9F8F
	DATA $E0F0,$E6E4,$E7E7,$CFCF
	DATA $70F8,$0300,$FFFF,$FFFF
	DATA $7C3C,$F1F8,$E0F1,$C7E4
	DATA $070F,$E387,$23E3,$0303
	DATA $C7FF,$C2C3,$C0C0,$C1C7
	DATA $FFFF,$1F3F,$FF7F,$FFFF
	DATA $C181,$C1C1,$C0C0,$C0C0
	DATA $E3E0,$E1E3,$F1F1,$F1F1
	DATA $8004,$E0C0,$F0E0,$FFFF
	DATA $7878,$7C7C,$FCFC,$FFFF
	DATA $FCFC,$FFFE,$FEFF,$FCFE
	DATA $3F1F,$3F3F,$7F7F,$7F7F
	DATA $CFCF,$CFCF,$FFFF,$FFFF
	DATA $C7C7,$8F8F,$DF9F,$FFFF
screen_bitmaps_2:
	DATA $E3E3,$E3E3,$EFE7,$FFFF
	DATA $8C80,$8F8E,$FF1F,$FFFF
	DATA $7F7F,$3F3F,$FFFF,$FFFF
	DATA $CFC0,$FFFF,$FFFF,$FFFF
	DATA $FFF1,$FFFF,$FFFF,$FFFF
	DATA $F8FC,$FFFF,$FFFF,$FFFF

	REM 20x12 cards
screen_cards:
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$1C00,$1C08,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$1C10,$1C18,$0000,$1C20,$1C28,$1C30,$1C38,$0000
	DATA $1C40,$1C48,$1C50,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$1C58,$1C60,$1C68,$1C70,$1C78,$1C80,$1C88,$0000
	DATA $1C90,$1C98,$1CA0,$1CA8,$1CB0,$1CB8,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$1CC0,$1CC8,$1CD0,$1CD8,$1CE0,$1CE8,$0000
	DATA $1CF0,$0000,$1CF8,$1D00,$1D08,$1D10,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$1D18,$1D20,$0000,$0000,$1D28,$0000,$0000
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
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
