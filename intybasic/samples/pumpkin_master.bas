	'
	' Pumpkin master
	'
	' Demo for IntyBASIC
	'
	' by Oscar Toledo G. (nanochess)
	' http://nanochess.org
	'
	' Creation date: Oct/28/2018.
	' Revision date: Oct/29/2018. Added the pumpkin master and story.
	' Revision date: Oct/31/2018. Story can be accelerated pressing button.
	'                             Added 3 more waves.
	'

	ON FRAME GOSUB play_sound

	' Number of pumpkins at same time, maximum 6 because SPRITE 6 is bullet
	' and SPRITE 7 is player
	CONST PUMPKINS = 6

	CONST VOLUME_TITLE = 12	' Volume of music during title
	CONST VOLUME_GAME = 10		' Volume of music inside the game
	CONST VOLUME_BOSS = 14		' Volume of music during boss

	UNSIGNED #score, #record

	DIM x(PUMPKINS)		' X-coordinate of pumpkin or boss bullet
	DIM y(PUMPKINS)		' Y-coordinate of pumpkin or boss bullet
	DIM s(PUMPKINS)		' State of pumpkin (in current wave)
	DIM z(PUMPKINS)		' Timing of pumpkin explosion
	DIM b(PUMPKINS)		' Pumpkin bullet

	#record = 25		' Setup a default record

	FOR c = 0 TO 60
		WAIT
	NEXT c

	'
	' For the good guys :)
	'
	PRINT AT 61 COLOR 5,"For all my friends"
	PRINT AT 81 COLOR 5,"   at Atariage!"
	GOSUB wait_and_clean

	PRINT AT 102 COLOR 5,"Is it already"
	PRINT AT 122 COLOR 2,"Halloween 2018?"
	PRINT AT 216 COLOR 3,";)"
	GOSUB wait_and_clean

	'
	' Show my logo
	'
	CLS
	MODE 1
	WAIT

	PLAY SIMPLE

	'
	' Title screen
	'
title_screen:
	PLAY music_game
	PLAY VOLUME VOLUME_TITLE

	CLS
	MODE 0,0,1,0,0
	WAIT
	FOR c = 0 TO 7
		SPRITE c, 0
	NEXT c
	WAIT
	DEFINE 0,16,pumpkin_bitmaps_0
	WAIT
	DEFINE 16,16,pumpkin_bitmaps_1
	WAIT
	DEFINE 32,16,pumpkin_bitmaps_2
	WAIT
	DEFINE 48,11,pumpkin_bitmaps_3
	DEFINE ALTERNATE 60,4,game_bitmaps_0
	WAIT
	' Show the Pumpkin Master and the title letters
	SCREEN pumpkin_cards,20,0,20,11

	' Add some houses
	SPRITE 0,$0308 + 8, $0088 + 64, $1801 + 62 * 8
	SPRITE 1,$0308 + 24, $0088 + 64, $1801 + 62 * 8
	SPRITE 2,$0308 + 40, $0088 + 64, $1801 + 62 * 8
	SPRITE 3,$0308 + 104, $0088 + 64, $1801 + 62 * 8
	SPRITE 4,$0308 + 120, $0088 + 64, $1801 + 62 * 8
	SPRITE 5,$0308 + 136, $0088 + 64, $1801 + 62 * 8

	' Add a pair of pumpkins
	SPRITE 6,$0308 + 64, $0088 + 8, $1802 + 60 * 8
	SPRITE 7,$0308 + 92, $0088 + 22, $1802 + 60 * 8

	' Show the record in a color bar (as ground)
	#backtab(180) = $2000
	PRINT AT 183 COLOR 5,"Record: ",<5>#record,"0"
	#backtab(200) = $2000

	PRINT AT 224 COLOR 6,"Press button"

	' Wait for controller to be "free"
	DO
		WAIT
		c = CONT
	LOOP WHILE c

	' Now wait for controller press
	DO
		WAIT
		c = CONT
	LOOP WHILE c = 0

	'
	' Prepare for starting game
	'
	CLS
	MODE 0,0,0,0,0
	FOR c = 0 TO 7
		SPRITE c, 0
	NEXT c
	WAIT
	DEFINE 0,16,game_bitmaps_0
	WAIT

	first_time_ever = 1	' In order to show story
	lives = 4		' Default lives
	level = 1		' Start level
	sublevel = 0		' Sublevel (or wave number)
	#score = 0		' Reset score
	FOR c = 0 TO PUMPKINS - 1
		x(c) = 0
		y(c) = 0
		s(c) = 0
		b(c) = 0
	NEXT c
	GOSUB start_wave	' Start a pumpkins wave

	'
	' Restart game after losing a life
	'
restart_game:
	PLAY VOLUME VOLUME_GAME
	by = 0			' No player bullet
	px = 84			' Setup player at bottom center
	py = 96

	GOSUB update_score
	GOSUB update_lives
	GOSUB update_level

	' If restarting inside boss level, go to boss game loop
	IF sublevel = 10 THEN PLAY VOLUME VOLUME_BOSS: GOTO boss_loop

	' Is it the first time the game restarts? show story.
	IF first_time_ever = 1 THEN
		first_time_ever = 0

		' Re-use pumpkin arrays for tiny houses
		FOR c = 0 TO 2
			x(c) = px - 12 * (c + 1)
		NEXT c
		FOR c = 3 TO 5
			x(c) = px + 12 * (c - 2)
		NEXT c 
		SPRITE 7, $0300 + px, $0080 + py, $1801 + 2 * 8
		FOR c = 0 TO 6
			SPRITE c, $0300 + x(c), $0080 + py, $1801 + 2 * 8
		NEXT c

		PRINT AT 65 COLOR 6,"Hey guys!"
		PRINT AT 85,"Thanks for"
		PRINT AT 105,"helping me."
		GOSUB wait_and_clean

		PRINT AT 64 COLOR 2,"Of course, we"
		PRINT AT 84,"like to eat"
		PRINT AT 104,"pumpkin."
		GOSUB wait_and_clean

		PRINT AT 62 COLOR 6,"Wait, we aren't"
		PRINT AT 82,"eating pumpkin"
		PRINT AT 102,"but killing them."
		GOSUB wait_and_clean

		PRINT AT 65 COLOR 2,"Killing?"
		GOSUB wait_and_clean

		PRINT AT 61 COLOR 6,"These are witched"
		PRINT AT 81,"pumpkins!"
		GOSUB wait_and_clean

		PRINT AT 65 COLOR 2,"Witched?"
		GOSUB wait_and_clean

		PRINT AT 63 COLOR 2,"Err... we have"
		PRINT AT 83,"things to do..."
		PRINT AT 103,"See you later!"

		' Tiny houses run to the sides
		FOR d = 0 TO 120
			' Houses at left side
			FOR c = 0 TO 2
				IF x(c) THEN x(c) = x(c) - 1
			NEXT c
			' House at right side
			FOR c = 3 TO 5
				IF x(c) < 168 THEN x(c) = x(c) + 1
			NEXT c
			' Update sprites
			FOR c = 0 TO 6
				SPRITE c, $0300 + x(c), $0080 + py, $1801 + 2 * 8
			NEXT c
			WAIT
		NEXT d
		GOSUB wait_and_clean

		PRINT AT 63 COLOR 6,"Cowards! I'll"
		PRINT AT 83,"use my homebrew"
		PRINT AT 103,"proton cannon."
		GOSUB wait_and_clean

		' Clean pumpkins array
		FOR c = 0 TO PUMPKINS - 1
			x(c) = 0
			y(c) = 0
			s(c) = 0
		NEXT c
	END IF

	'
	' Game loop for pumpkins waves
	'
game_loop:

	'
	' Update pumpkins and drop bullets
	'
	IF drop_bullet THEN	' Not yet time for a bullet?
		drop_bullet = drop_bullet - 1
		d = PUMPKINS
	ELSE
		c = level
		IF c > 10 THEN c = 10
		drop_bullet = 15 - c + RAND(16)	' Time for next bullet
		d = RAND(PUMPKINS)	' Choose a random pumpkin that will shot
	END IF

	'
	' Check also if no pumpkins are shown (in order to trigger another wave)
	'
	valid = 0
	FOR c = 0 TO PUMPKINS - 1
		IF y(c) THEN
			IF s(c) THEN
				SPRITE c, $0300 + x(c), $0080 + y(c), $1802 + 0 * 8
			ELSE
				SPRITE c, $0300 + x(c), $0080 + y(c), $1802 + 6 * 8
				z(c) = z(c) - 1
				IF z(c) = 0 THEN y(c) = 0
			END IF
			valid = 1
			' Shot if possible
			IF c = d THEN	' Pumpkin can shot
				IF b(c) = 0 THEN	' Free space for shot
					IF x(c) > 7 THEN	' Pumpkin inside visible screen
						IF x(c) < 168 THEN
							b(c) = x(c) / 8 + (y(c) / 8) * 20 - 21
							IF sound_effect < 3 THEN sound_effect = 2: sound_state = 0
						END IF
					END IF
				END IF
			END IF
		ELSE
			SPRITE c, 0
		END IF
	NEXT c

	'
	' Update player house and bullet
	'
	GOSUB update_player
	MODE 0,0,0,0,0:BORDER 0

	WAIT
	' Check if player is touched by background (bullet)
	IF COL7 AND $0100 THEN	GOTO player_touched
	' Check if player bullet touches pumpkin
	IF COL6 AND $003F THEN
		' Hardware saves us of tedious collision checking
		c = 255
		IF COL6 AND $0001 THEN c = 0
		IF COL6 AND $0002 THEN c = 1
		IF COL6 AND $0004 THEN c = 2
		IF COL6 AND $0008 THEN c = 3
		IF COL6 AND $0010 THEN c = 4
		IF COL6 AND $0020 THEN c = 5
		IF c < 6 THEN	' Pumpkin touched?
			IF y(c) THEN	' Pumpkin alive?
				IF s(c) THEN	' Pumpkin moving?
					s(c) = 0	' Cease movement
					z(c) = 8	' Start explosion timing
					by = 0
					#score = #score + 1
					GOSUB update_score
					sound_effect = 3: sound_state = 0
				END IF
			END IF
		END IF
	END IF

	'
	' Move pumpkins bullets in 4px steps.
	'
	next_bullet = (next_bullet + 1) AND 3
	IF next_bullet = 0 THEN
		FOR c = 0 TO PUMPKINS - 1
			d = b(c)
			IF d THEN
				#backtab(d) = $0802 + 5 * 8
				b(c) = d
			END IF
		NEXT c
	ELSEIF next_bullet = 2 THEN
		FOR c = 0 TO PUMPKINS - 1
			d = b(c)
			IF d THEN
				#backtab(d) = 0
				IF d >= 220 THEN
					d = 0
				ELSE
					d = d + 20
					#backtab(d) = $0802 + 4 * 8
				END IF
				b(c) = d
			END IF
		NEXT c
	END IF

	'
	' Start a new wave of pumpkins, pumpkin boss or move pumpkins
	'
	IF next_wave THEN	' Waiting for next wave
		next_wave = next_wave - 1
		IF next_wave = 0 THEN	' Start it?
			ON wave GOSUB start_0, start_1, start_2, start_3, start_4, start_5, start_6, start_7, start_8, start_9
		END IF
	ELSEIF valid = 0 THEN	' No pumpkins alive?
		IF sublevel = 9 THEN	' Last wave?

			'
			' Start boss wave
			'
			sublevel = 10

			' Remove pumpkin bullets
			FOR c = 0 TO PUMPKINS - 1	
				b(c) = 0
			NEXT c

			' Bring in the Pumpkin Master and clean screen
			SCREEN pumpkin_cards, 60, 20, 20, 7, 20
			FOR c = 160 TO 239
				#backtab(c) = 0
			NEXT c

			' Remove all pumpkins
			FOR c = 0 TO PUMPKINS - 1
				y(c) = 0
				s(c) = 0
				SPRITE c, 0
			NEXT c

			' Count of boss blocks
			blocks = 7 * 7 - 7

			' Start boss music
			PLAY VOLUME VOLUME_BOSS
			PLAY music_beat

			' Go to boss game loop
			GOTO boss_loop
		ELSE
			GOSUB start_wave
		END IF
	ELSE
		ON wave GOSUB move_0, move_1, move_2, move_3, move_4, move_5, move_6, move_7, move_8, move_9
	END IF

	GOSUB move_player
	GOTO game_loop

	'
	' Boss loop
	'
boss_loop:

	'
	' Show boss' bullets and make them to descend
	'
	FOR c = 0 TO PUMPKINS - 1
		IF y(c) THEN
			SPRITE c, $0300 + x(c), $0000 + y(c), $0802 + 4 * 8
			y(c) = y(c) + 1
			IF y(c) >= 104 THEN y(c) = 0
		ELSE
			SPRITE c, 0
			x(c) = RANDOM(152) + 8
			y(c) = 8 + RANDOM(16)
		END IF
	NEXT c

	'
	' Update player house and bullet
	'
	GOSUB update_player

	'
	' If boss hit then flash screen
	'
	IF gronk THEN MODE 0,7,7,7,7:BORDER 7:gronk = 0 ELSE MODE 0,0,0,0,0:BORDER 0
	WAIT

	' Player touched by bullet
	IF COL7 AND $003f THEN	GOTO player_touched

	' If bullet is moving check if touches boss
	IF by THEN
		c = (bx + 1) / 8 + (by / 8) * 20 - 21
		IF c >= 20 AND #backtab(c) <> 0 THEN	' Crashes against it?
			#backtab(c) = 0		' Remove block
			gronk = 1		' Signal flash requested
			by = 0			' Remove bullet
			#score = #score + 2
			sound_effect = 3: sound_state = 0
			blocks = blocks - 1	' One block less
			IF blocks = 0 THEN	' All blocks completed?
				#score = #score + 50	' Bonus
			END IF
			GOSUB update_score
			IF blocks = 0 THEN	' All blocks completed?
				'
				' Return to pumpkins waves
				'
				sound_effect = 4:sound_state = 0
				PLAY VOLUME VOLUME_GAME
				PLAY music_game
				FOR c = 0 TO PUMPKINS - 1
					y(c) = 0
					s(c) = 0
					SPRITE c, 0
				NEXT c
				GOSUB start_wave
				GOTO game_loop
			END IF
		END IF
	END IF

	GOSUB move_player
	GOTO boss_loop

	'
	' Player explosion
	'
player_touched:
	PLAY VOLUME 0	
	sound_effect = 4: sound_state = 0

	'
	' Screen cleaning changes if it's pumpkins or boss wave.
	'
	IF sublevel = 10 THEN
		FOR c = 0 TO PUMPKINS - 1
			SPRITE c, 0
			y(c) = 0
			s(c) = 0
		NEXT c
	ELSE
		FOR c = 20 TO 239
			#backtab(c) = 0
		NEXT c
		FOR c = 0 TO PUMPKINS - 1
			b(c) = 0
		NEXT c
	END IF

	'
	' Player nuclear explosion
	' (not really, the pumpkin bullet hit the freezer and
	' it was filled with beer :P)
	'
	SPRITE 6,0
	FOR c = 0 TO 127
		IF c = 0 THEN
			SPRITE 7, $0300 + px, $0080 + py
		END IF
		IF c = 32 THEN
			SPRITE 7, $0300 + px, $0180 + py - 8
		END IF
		IF c = 64 THEN
			SPRITE 7, $0700 + px - 4, $0180 + py - 8
		END IF
		IF c = 96 THEN
			SPRITE 7, $0700 + px - 4, $0280 + py - 24
		END IF
		IF C AND 2 THEN
			SPRITE 7,,,$0807 + 10 * 8
		ELSE
			SPRITE 7,,,$0807 + 8 * 8
		END IF
		d = c AND 15
		MODE 0,d,d,d,d
		BORDER d
		WAIT
	NEXT c
	SPRITE 7,0
	MODE 0,0,0,0,0
	BORDER 0
	FOR c = 0 TO 10
		WAIT
	NEXT c

	' No more lives = Game over
	IF lives = 0 THEN
		FOR c = 0 TO 255
			PRINT AT 105 COLOR C AND 7,"GAME  OVER"
			WAIT
		NEXT c
		IF #score > #record THEN #record = #score
		GOTO title_screen
	END IF

	' One life less, restart game
	lives = lives - 1
	GOTO restart_game

	'
	' Move player and bullet
	'
move_player:	PROCEDURE
	IF by THEN	' Active bullet?
		by = by - 4	' Move towards top
		IF by < 16 THEN by = 0	' Disappears if touches score bar
	END IF	

	c = CONT
	d = c AND $E0
	IF (d = $80) + (d = $40) + (d = $20) THEN	' Ignore keypad
	ELSE
		IF (d = $a0) + (d = $c0) + (d = $60) THEN	' Side-button
			IF by = 0 THEN	' Only if no active bullet?
				bx = px + 2	' Start a bullet
				by = 96
				IF sound_effect < 3 THEN sound_effect = 1: sound_state = 0
			END IF
		END IF
		c = controller_direction(c AND $1F)
		' Move to right
		IF c = 2 THEN IF px < 160 THEN px = px + 2
		' Move to left
		IF c = 4 THEN IF px > 8 THEN px = px - 2 
	END IF
	END

	'
	' Update player house and bullet
	'
update_player:	PROCEDURE
	IF by THEN
		SPRITE 6, $0300 + bx, $0000 + by, $0805 + 12 * 8
	ELSE
		SPRITE 6, 0
	END IF
	SPRITE 7, $0300 + px, $0080 + py, $1801 + 2 * 8
	END

	'
	' Update current score
	'
update_score:	PROCEDURE
	PRINT AT 0 COLOR 4,"1UP "
	PRINT COLOR 5,<5>#score,"0"
	END

	'
	' Update level
	'
update_level:	PROCEDURE
	PRINT AT 12 COLOR 4,"L"
	PRINT COLOR 5,<2>level
	END

	'
	' Update number of lives
	'
update_lives:	PROCEDURE
	PRINT AT 18 COLOR 4,"H"
	PRINT COLOR 5,<>lives
	END

	'
	' Wait and clean story
	'
wait_and_clean:		PROCEDURE
	FOR c = 0 TO 180
		IF c >= 60 THEN d = CONT: IF d THEN EXIT FOR
		WAIT
	NEXT c
	FOR c = 60 TO 119
		#backtab(c) = 0
	NEXT c
	END

	'
	' Start a new attack wave
	'
start_wave:	PROCEDURE
	wave = cucu_wave
	cucu_wave = cucu_wave + 1
	IF cucu_wave = 10 THEN cucu_wave = 0
'	DO
'		c = RAND(10)
'	LOOP WHILE wave = c
'	wave = c
	sublevel = sublevel + 1
	next_wave = 30 + RAND(30)
	IF sublevel = 11 THEN
		sublevel = 0
		level = level + 1
		GOSUB update_level
	END IF
	END

	'
	' Start wave 0
	'
start_0:	PROCEDURE
	FOR c = 0 TO PUMPKINS - 1
		x(c) = 168 + c * 12
		y(c) = 48
		s(c) = 1
	NEXT c
	END

	'
	' Move wave 0
	'
move_0:	PROCEDURE
	FOR c = 0 TO PUMPKINS - 1
		ON s(c) GOTO move_00, move_01, move_02, move_03

move_01:
		x(c) = x(c) - 2
		IF x(c) = 8 THEN s(c) = 2
		GOTO move_00

move_02:
		x(c) = x(c) + 1
		y(c) = y(c) - 1
		IF y(c) = 24 THEN s(c) = 3
		GOTO move_00

move_03:	x(c) = x(c) + 2
		IF x(c) = 168 THEN y(c) = 0: s(c) = 0
		GOTO move_00

move_00:
	NEXT c
	END

	'
	' Start wave 1
	'
start_1:	PROCEDURE
	FOR c = 0 TO PUMPKINS - 1
		x(c) = 0 - c * 12
		y(c) = 48
		s(c) = 1
	NEXT c
	END

	'
	' Move wave 1
	'
move_1:	PROCEDURE
	FOR c = 0 TO PUMPKINS - 1
		ON s(c) GOTO move_010, move_011, move_012, move_013

move_011:
		x(c) = x(c) + 2
		IF x(c) = 160 THEN s(c) = 2
		GOTO move_010

move_012:
		x(c) = x(c) - 1
		y(c) = y(c) - 1
		IF y(c) = 24 THEN s(c) = 3
		GOTO move_010

move_013:	x(c) = x(c) - 2
		IF x(c) = 0 THEN y(c) = 0: s(c) = 0
		GOTO move_010

move_010:
	NEXT c
	END

	'
	' Start wave 2
	'
start_2:	PROCEDURE
	FOR c = 0 TO PUMPKINS - 1
		x(c) = 168 + c * 12
		y(c) = 48
		s(c) = 1
	NEXT c
	END

	'
	' Move wave 2
	'
move_2:	PROCEDURE
	FOR c = 0 TO PUMPKINS - 1
		ON s(c) GOTO move_020, move_021

move_021:
		x(c) = x(c) - 2
		y(c) = 24 + sin24((x(c) AND $3e) / 2)
		IF x(c) = 0 THEN y(c) = 0: s(c) = 0
		GOTO move_020

move_020:
	NEXT c
	END

	'
	' Start wave 3
	'
start_3:	PROCEDURE
	FOR c = 0 TO PUMPKINS - 1
		x(c) = 0 - c * 12
		y(c) = 48
		s(c) = 1
	NEXT c
	END

	'
	' Move wave 3
	'
move_3:	PROCEDURE
	FOR c = 0 TO PUMPKINS - 1
		ON s(c) GOTO move_030, move_031

move_031:
		x(c) = x(c) + 2
		y(c) = 24 + sin24((x(c) AND $3e) / 2)
		IF x(c) = 168 THEN y(c) = 0: s(c) = 0
		GOTO move_030

move_030:
	NEXT c
	END

	'
	' Start wave 4
	'
start_4:	PROCEDURE
	FOR c = 0 TO PUMPKINS - 1
		x(c) = 0 - c * 12
		y(c) = 24
		s(c) = 1
	NEXT c
	END

	'
	' Move wave 4
	'
move_4:	PROCEDURE
	FOR c = 0 TO PUMPKINS - 1
		ON s(c) GOTO move_040, move_041, move_042, move_043

move_041:
		x(c) = x(c) + 2
		IF x(c) = 90 THEN s(c) = 2
		GOTO move_040

move_042:
		y(c) = y(c) + 1
		x(c) = 72 + sin24(y(c) AND $1f)
		IF y(c) = 80 THEN s(c) = 3
		GOTO move_040

move_043:
		x(c) = (x(c) AND $fe) + 2
		IF x(c) = 168 THEN y(c) = 0: s(c) = 0
		GOTO move_040

move_040:
	NEXT c
	END

	'
	' Start wave 5
	'
start_5:	PROCEDURE
	FOR c = 0 TO PUMPKINS - 1
		x(c) = 168 + RANDOM(32) * 2
		y(c) = 24 + c * 8
		s(c) = 1
	NEXT c
	END

	'
	' Move wave 5
	'
move_5:	PROCEDURE
	FOR c = 0 TO PUMPKINS - 1
		ON s(c) GOTO move_050, move_051

move_051:
		x(c) = x(c) - 2
		IF x(c) = 0 THEN y(c) = 0: s(c) = 0
		GOTO move_050

move_050:
	NEXT c
	END

	'
	' Start wave 6
	'
start_6:	PROCEDURE
	FOR c = 0 TO PUMPKINS - 1
		IF c AND 1 THEN
			x(c) = 168 + RANDOM(32) * 2
			s(c) = 1
		ELSE
			x(c) = 0 - RANDOM(32) * 2
			s(c) = 2
		END IF
		y(c) = 24 + c * 8
	NEXT c
	END

	'
	' Move wave 6
	'
move_6:	PROCEDURE
	FOR c = 0 TO PUMPKINS - 1
		ON s(c) GOTO move_060, move_061, move_062

move_061:
		x(c) = x(c) - 2
		IF x(c) = 0 THEN y(c) = 0: s(c) = 0
		GOTO move_060

move_062:
		x(c) = x(c) + 2
		IF x(c) = 168 THEN y(c) = 0: s(c) = 0
		GOTO move_060

move_060:
	NEXT c
	END

	'
	' Start wave 7
	'
start_7:	PROCEDURE
	FOR c = 0 TO PUMPKINS - 1
		d = c % 3
		IF d = 0 THEN x(c) = 0 - c / 3 * 32:y(c) = 24
		IF d = 1 THEN x(c) = 0 - c / 3 * 32:y(c) = 40
		IF d = 2 THEN x(c) = -16 - c / 3 * 32: y(c) = 32
		s(c) = 1
	NEXT c
	END

	'
	' Move wave 7
	'
move_7:	PROCEDURE
	FOR c = 0 TO PUMPKINS - 1
		ON s(c) GOTO move_70, move_71

move_71:
		x(c) = x(c) + 2
		IF x(c) = 168 THEN y(c) = 0: s(c) = 0
		GOTO move_70

move_70:
	NEXT c
	END

	'
	' Start wave 8
	'
start_8:	PROCEDURE
	FOR c = 0 TO PUMPKINS - 1
		x(c) = 0 - c * 12
		y(c) = 24
		s(c) = 1
	NEXT c
	END

	'
	' Move wave 8
	'
move_8:	PROCEDURE
	FOR c = 0 TO PUMPKINS - 1
		ON s(c) GOTO move_080, move_081, move_082, move_083, move_084, move_085

move_081:
		x(c) = x(c) + 2
		IF x(c) = 160 THEN s(c) = 2
		GOTO move_080

move_082:
		x(c) = x(c) - 1
		y(c) = y(c) + 1
		IF (y(c) AND 7) = 0 THEN IF y(c) = 80 THEN s(c) = 5 ELSE s(c) = 3
		GOTO move_080

move_083:	x(c) = x(c) - 2
		IF x(c) = 8 THEN s(c) = 4
		GOTO move_080

move_084:
		x(c) = x(c) + 1
		y(c) = y(c) + 1
		IF (y(c) AND 7) = 0 THEN s(c) = 1
		GOTO move_080

move_085:
		x(c) = x(c) - 2
		IF x(c) = 0 THEN s(c) = 0: y(c) = 0
		GOTO move_080

move_080:
	NEXT c
	END

	'
	' Start wave 9
	'
start_9:	PROCEDURE
	FOR c = 0 TO PUMPKINS - 1
		IF c AND 1 THEN
			x(c) = 0 - c / 2 * 24
			y(c) = 24
		ELSE
			x(c) = 0 - c / 2 * 24
			y(c) = 40
		END IF
		s(c) = 1
		z(c) = 54
	NEXT c
	END

	'
	' Move wave 9
	'
move_9:	PROCEDURE
	FOR c = 0 TO PUMPKINS - 1
		ON s(c) GOTO move_090, move_091, move_092, move_093

move_091:
		x(c) = x(c) + 2
		z(c) = z(c) - 1
		IF z(c) = 0 THEN s(c) = 2
		GOTO move_090

move_092:
		IF y(c) AND 8 THEN
			x(c) = x(c) - 1
			y(c) = y(c) + 1
		ELSE
			x(c) = x(c) + 1
			y(c) = y(c) + 1
		END IF
		IF y(c) = 80 THEN s(c) = 3
		GOTO move_090

move_093:
		x(c) = x(c) + 2
		IF x(c) >= 168 THEN s(c) = 0: y(c) = 0
		GOTO move_090

move_090:
	NEXT c
	END

	'
	' Sine table for curvy movement
	'
sin24:
	DATA 0,2,5,7,9,11,13,15
	DATA 17,19,20,21,22,23,24,24
	DATA 24,24,24,23,22,21,20,19
	DATA 17,15,13,11,9,7,5,2

	'
	' Table for converting disc direction to 4-way direction
	'
controller_direction:
	DATA 0,3,2,3,1,0,2,0
	DATA 4,4,0,0,1,0,0,0
	DATA 0,3,2,0,0,0,0,0
	DATA 4,0,0,0,1,0,0,0

	'
	' Bitmaps used for the game
	'
game_bitmaps_0:
	BITMAP "........"	' 0 Pumpkin
	BITMAP "........"
	BITMAP "........"
	BITMAP ".XX....."
	BITMAP "..X....."
	BITMAP "...X...."
	BITMAP ".XX.XXX."
	BITMAP "XX.XX.XX"
	BITMAP "X..XX..X"
	BITMAP "XXXX.XXX"
	BITMAP "XXXXXXXX"
	BITMAP "X.XXXX.X"
	BITMAP "X..X...X"
	BITMAP "XX...XXX"
	BITMAP ".XXXXXX."
	BITMAP "..XXXX.."

	BITMAP "...XX..."	' 2 House
	BITMAP "...XX..."
	BITMAP "...XX..."
	BITMAP "..XXX..."
	BITMAP ".X..X..."
	BITMAP "X.XX.X.."
	BITMAP ".X.XX.X."
	BITMAP "XX.XXX.X"
	BITMAP "XXXXXXX."
	BITMAP "X.XXX.XX"
	BITMAP "X.XXX.XX"
	BITMAP "XXXXXXXX"
	BITMAP "X.XXX.XX"
	BITMAP "X.XXX.XX"
	BITMAP "XXX.XXXX"
	BITMAP "XXX.XXXX"

	BITMAP "...X...."	' 4 Pumpkin bullet 1
	BITMAP "...XX..."
	BITMAP "...XX..."
	BITMAP "...XX..."
	BITMAP "....X..."
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"

	BITMAP "........"	' 5 Pumpkin bullet 2
	BITMAP "........"
	BITMAP "........"
	BITMAP "....X..."
	BITMAP "...XX..."
	BITMAP "...XX..."
	BITMAP "...XX..."
	BITMAP "...X...."

	BITMAP "X......."	' 6 Pumpkin explosion
	BITMAP "X......X"
	BITMAP ".XX....."
	BITMAP "XX....XX"
	BITMAP ".X....X."
	BITMAP ".X..X.X."
	BITMAP ".X.X...."
	BITMAP "........"
	BITMAP "XX....XX"
	BITMAP ".X...X.."
	BITMAP "........"
	BITMAP "..X..X.."
	BITMAP ".XX..XX."
	BITMAP "X.....X."
	BITMAP ".......X"
	BITMAP ".......X"

	BITMAP "..XXXX.."	' 8 Nuclear mushroom 1
	BITMAP ".XXXXXX."
	BITMAP "XXXXXXXX"
	BITMAP "XXXXXXXX"
	BITMAP "X.X.XX.X"
	BITMAP "X.XXXX.X"
	BITMAP "..XX.X.."
	BITMAP "..XXXX.."
	BITMAP "..X.XX.."
	BITMAP "..XXXX.."
	BITMAP "..XX.X.."
	BITMAP "..XXXX.."
	BITMAP ".XXXXXX."
	BITMAP ".X.XXXX."
	BITMAP "XXXXX.XX"
	BITMAP "XXXXXXXX"

	BITMAP "..XXXX.."	' 10 Nuclear mushroom 2
	BITMAP ".XXXXXX."
	BITMAP "XXXXXXXX"
	BITMAP "XXXX.XXX"
	BITMAP "X.XXXX.X"
	BITMAP "X.X.XX.X"
	BITMAP "..XXXX.."
	BITMAP "..XX.X.."
	BITMAP "..XXXX.."
	BITMAP "..X.XX.."
	BITMAP "..XXXX.."
	BITMAP "..XXXX.."
	BITMAP ".XXXX.X."
	BITMAP ".X.XXXX."
	BITMAP "XXXX.XXX"
	BITMAP "XXXXXXXX"

	BITMAP ".X......"	' 12 Player bullet
	BITMAP "XX......"
	BITMAP "XX......"
	BITMAP "XX......"
	BITMAP "XXX....."
	BITMAP "XXX....."
	BITMAP ".XX....."
	BITMAP ".X......"

	' Based on https://pixabay.com/es/pumpkin-helloween-witch-bruja-3726795/
	REM IntyColor v1.1.5 Jul/25/2017
	REM Command: ./intycolor -b -n -s0000 pumpkin.bmp pumpkin_title.bas pumpkin 
	REM Created: Sun Oct 28 21:28:30 2018

	' 59 bitmaps
pumpkin_bitmaps_0:
	DATA $0000,$0000,$0000,$0600
	DATA $667C,$6666,$607C,$6060
	DATA $0000,$6666,$6666,$3E66
	DATA $0000,$667D,$6666,$6666
	DATA $0000,$66C7,$6666,$6766
	DATA $0606,$66C6,$6767,$C666
	DATA $0006,$C666,$8686,$66C6
	DATA $0000,$667C,$6666,$6666
	DATA $0706,$0507,$0404,$0404
	DATA $1808,$D8B8,$1998,$1819
	DATA $0000,$19F0,$98F9,$F998
	DATA $0303,$83F7,$F3E3,$E133
	DATA $0000,$19CF,$181F,$CF18
	DATA $0000,$9F1B,$1898,$1898
	DATA $0606,$0000,$0000,$0000
	DATA $0100,$0003,$0300,$0101
pumpkin_bitmaps_1:
	DATA $E000,$1810,$FC68,$B868
	DATA $60C0,$1830,$070C,$0303
	DATA $0000,$1800,$F8F0,$CEFB
	DATA $0000,$0000,$0000,$70F0
	DATA $0100,$0301,$0101,$0000
	DATA $83FE,$B0E0,$F0D0,$3F6F
	DATA $8000,$3CE0,$0007,$9FE7
	DATA $0706,$0F0F,$6100,$BFDE
	DATA $D18C,$6071,$C020,$7EF9
	DATA $F8F8,$00E0,$0000,$7CE0
	DATA $0100,$0303,$0707,$1F0F
	DATA $FEFF,$FBFD,$EFF7,$DFDF
	DATA $FE7F,$FEFE,$FDFD,$FBFD
	DATA $FF7F,$FFFF,$FFFF,$FFFF
	DATA $BFBF,$DFDF,$EFEF,$F7EF
	DATA $DFBF,$F7EF,$FDFB,$FEFE
pumpkin_bitmaps_2:
	DATA $C000,$F0E0,$F8F0,$FCFC
	DATA $1F1F,$3F3F,$3F3F,$3E3E
	DATA $BFBF,$7F7F,$7F7F,$FFFF
	DATA $9BBB,$838B,$8081,$E0C0
	DATA $FFFF,$FFFF,$7FFF,$FF3F
	DATA $F6F7,$F0F4,$00C0,$C080
	DATA $3FFF,$3F3F,$7F7F,$FF7F
	DATA $7E7E,$BEBE,$DEBE,$DEDE
	DATA $3E3E,$1E3E,$1E1E,$0F1E
	DATA $FFFF,$FFFF,$FFFF,$78F9
	DATA $F7F7,$F7F7,$F7F7,$F7F7
	DATA $FFFF,$F7F7,$E3E3,$FFFF
	DATA $FBFB,$FBFB,$FBFB,$FBFB
	DATA $FFFF,$FFFF,$FFFF,$CFEF
	DATA $DEDE,$DEDE,$DCDC,$BCDC
	DATA $070F,$0707,$0103,$0001
pumpkin_bitmaps_3:
	DATA $7C7C,$BEBC,$DFDE,$F7EF
	DATA $3737,$3839,$0018,$E080
	DATA $FFFF,$00FF,$0000,$1E00
	DATA $F8FB,$00C0,$0000,$6F28
	DATA $0F0F,$1F1F,$7E3E,$FBFD
	DATA $B8B8,$7070,$E0E0,$80C0
	DATA $3D7B,$0F1E,$0003,$0000
	DATA $FDFC,$7EFE,$E79F,$0118
	DATA $9F1E,$FFFF,$BF7F,$00DE
	DATA $DF6F,$DFDF,$79BE,$20C6
	DATA $EEF7,$B8DC,$8060,$0000

	REM 20x12 cards
pumpkin_cards:
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$1806,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$180E,$1816,$181E,$1826,$182E,$1836,$183E,$1846,$184E,$1856,$185E,$1866,$186E,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$1876,$0000,$087D,$0885,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$088D,$0895,$089D,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$08A5,$08AD,$08B5,$08BD,$08C5,$08CD,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$18D2,$18DA,$18E2,$18EA,$18F2,$18FA,$1902,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$190A,$1912,$191A,$1922,$192A,$1932,$193A,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$1942,$194A,$1952,$195A,$1962,$196A,$1972,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$197A,$1982,$198A,$1992,$199A,$19A2,$19AA,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$19B2,$19BA,$19C2,$19CA,$19D2,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000

	'
	' Play sound effects
	'
play_sound:	PROCEDURE
	ON sound_effect GOSUB play_none, play_fire, play_drop, play_explosion_1, play_explosion_2
	END

play_none:	PROCEDURE
	SOUND 2,,0
	SOUND 4,,$38
	END

play_fire:	PROCEDURE
	SOUND 2,200 - sound_state * sound_state,12
	SOUND 4,,$38
	sound_state = sound_state + 1
	IF sound_state = 10 THEN sound_effect = 0
	END

play_drop:	PROCEDURE
	SOUND 2,100 + (RAND AND 1) + sound_state * sound_state,12
	SOUND 4,,$38
	sound_state = sound_state + 1
	IF sound_state = 10 THEN sound_effect = 0
	END

play_explosion_1:	PROCEDURE
	SOUND 2,1000+sound_state * 16,12 - sound_state / 4
	SOUND 4,,$38
	sound_state = sound_state + 1
	IF sound_state = 24 THEN sound_effect = 0
	END

play_explosion_2:	PROCEDURE
	SOUND 2,2000-sound_state*16,12 - sound_state / 8
	SOUND 4,31-sound_state/4,$18
	sound_state = sound_state + 1
	IF sound_state = 96 THEN sound_effect = 0
	END
	
	ASM ORG $F000

	'
	' Pumpkin Boogie
	' 
	' Automagically generated by boogie.c srand(1540785469)
music_game:
	DATA 6
	MUSIC C3W,C5W,-,-
	MUSIC C3,C4,-,-
	MUSIC S,S,-
	MUSIC D3#,A4,-,-
	MUSIC E3,F4#,-,-
	MUSIC S,S,-
	MUSIC G3,G4,-,-
	MUSIC C3,C4,-,-
	MUSIC S,S,-
	MUSIC A3,-,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC F3,F4,-,-
	MUSIC F3,C5,-,-
	MUSIC S,S,-
	MUSIC G3#,D5,-,-
	MUSIC A3,C5,-,-
	MUSIC S,S,-
	MUSIC C4,F4,-,-
	MUSIC F3,-,-,-
	MUSIC S,-,-
	MUSIC D4,-,-,-
	MUSIC C4,-,-,-
	MUSIC S,-,-
	MUSIC G3,B5,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC A3#,B5,-,-
	MUSIC B3,-,-,-
	MUSIC S,-,-
	MUSIC D4,B5,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC E4,B5,-,-
	MUSIC D4,-,-,-
	MUSIC S,-,-
	MUSIC F3,F5,-,-
	MUSIC F3,F4,-,-
	MUSIC S,S,-
	MUSIC G3#,D5,-,-
	MUSIC A3,B4,-,-
	MUSIC S,S,-
	MUSIC C4,C5,-,-
	MUSIC F3,F4,-,-
	MUSIC S,S,-
	MUSIC D4,-,-,-
	MUSIC C4,-,-,-
	MUSIC S,-,-
	MUSIC C3,C5,-,-
	MUSIC C3,C4,-,-
	MUSIC S,S,-
	MUSIC D3#,A4,-,-
	MUSIC E3,F4#,-,-
	MUSIC S,S,-
	MUSIC G3,G4,-,-
	MUSIC C3,C4,-,-
	MUSIC S,S,-
	MUSIC A3,-,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC C3,-,-
	MUSIC C3,-,-
	MUSIC S,-,-
	MUSIC D3#,-,-
	MUSIC E3,-,-
	MUSIC S,-,-
	MUSIC G3,-,-
	MUSIC C3,-,-
	MUSIC S,-,-
	MUSIC A3,-,-
	MUSIC G3,-,-
	MUSIC S,-,-
	MUSIC C3,E5,-,-
	MUSIC C3,-,-,-
	MUSIC S,-,-
	MUSIC D3#,E5,-,-
	MUSIC E3,-,-,-
	MUSIC S,-,-
	MUSIC G3,E5,-,-
	MUSIC C3,-,-,-
	MUSIC S,-,-
	MUSIC A3,E5,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC C3,-,-
	MUSIC C3,-,-
	MUSIC S,-,-
	MUSIC D3#,-,-
	MUSIC E3,-,-
	MUSIC S,-,-
	MUSIC G3,-,-
	MUSIC C3,-,-
	MUSIC S,-,-
	MUSIC A3,-,-
	MUSIC G3,-,-
	MUSIC S,-,-
	MUSIC C3,C5,-,-
	MUSIC C3,C4,-,-
	MUSIC S,S,-
	MUSIC D3#,A4,-,-
	MUSIC E3,F4#,-,-
	MUSIC S,S,-
	MUSIC G3,G4,-,-
	MUSIC C3,C4,-,-
	MUSIC S,S,-
	MUSIC A3,-,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC C3,E5,-,-
	MUSIC C3,-,-,-
	MUSIC S,-,-
	MUSIC D3#,-,-,-
	MUSIC E3,-,-,-
	MUSIC S,-,-
	MUSIC G3,E5,-,-
	MUSIC C3,-,-,-
	MUSIC S,-,-
	MUSIC A3,-,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC C3,C5,-,-
	MUSIC C3,C4,-,-
	MUSIC S,S,-
	MUSIC D3#,A4,-,-
	MUSIC E3,F4#,-,-
	MUSIC S,S,-
	MUSIC G3,G4,-,-
	MUSIC C3,C4,-,-
	MUSIC S,S,-
	MUSIC A3,-,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC C3,C4,-,-
	MUSIC C3,G4,-,-
	MUSIC S,S,-
	MUSIC D3#,C4,-,-
	MUSIC E3,A4,-,-
	MUSIC S,S,-
	MUSIC G3,C4,-,-
	MUSIC C3,C5,-,-
	MUSIC S,S,-
	MUSIC A3,C4,-,-
	MUSIC G3,A4,-,-
	MUSIC S,S,-
	MUSIC C3,C4,-,-
	MUSIC C3,G4,-,-
	MUSIC S,S,-
	MUSIC D3#,C4,-,-
	MUSIC E3,A4,-,-
	MUSIC S,S,-
	MUSIC G3,C4,-,-
	MUSIC C3,C5,-,-
	MUSIC S,S,-
	MUSIC A3,C4,-,-
	MUSIC G3,A4,-,-
	MUSIC S,S,-
	MUSIC F3,F5,-,-
	MUSIC F3,F4,-,-
	MUSIC S,S,-
	MUSIC G3#,D5,-,-
	MUSIC A3,B4,-,-
	MUSIC S,S,-
	MUSIC C4,C5,-,-
	MUSIC F3,F4,-,-
	MUSIC S,S,-
	MUSIC D4,-,-,-
	MUSIC C4,-,-,-
	MUSIC S,-,-
	MUSIC G3,B5,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC A3#,B5,-,-
	MUSIC B3,-,-,-
	MUSIC S,-,-
	MUSIC D4,B5,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC E4,B5,-,-
	MUSIC D4,-,-,-
	MUSIC S,-,-
	MUSIC F3,F4,-,-
	MUSIC F3,C5,-,-
	MUSIC S,S,-
	MUSIC G3#,D5,-,-
	MUSIC A3,C5,-,-
	MUSIC S,S,-
	MUSIC C4,F4,-,-
	MUSIC F3,-,-,-
	MUSIC S,-,-
	MUSIC D4,-,-,-
	MUSIC C4,-,-,-
	MUSIC S,-,-
	MUSIC C3,C4,-,-
	MUSIC C3,G4,-,-
	MUSIC S,S,-
	MUSIC D3#,C4,-,-
	MUSIC E3,A4,-,-
	MUSIC S,S,-
	MUSIC G3,C4,-,-
	MUSIC C3,C5,-,-
	MUSIC S,S,-
	MUSIC A3,C4,-,-
	MUSIC G3,A4,-,-
	MUSIC S,S,-
	MUSIC C3,E5,-,-
	MUSIC C3,-,-,-
	MUSIC S,-,-
	MUSIC D3#,-,-,-
	MUSIC E3,-,-,-
	MUSIC S,-,-
	MUSIC G3,E5,-,-
	MUSIC C3,-,-,-
	MUSIC S,-,-
	MUSIC A3,-,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC C3,C4,-,-
	MUSIC C3,G4,-,-
	MUSIC S,S,-
	MUSIC D3#,A4,-,-
	MUSIC E3,G4,-,-
	MUSIC S,S,-
	MUSIC G3,C4,-,-
	MUSIC C3,-,-,-
	MUSIC S,-,-
	MUSIC A3,-,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC C3,C4,-,-
	MUSIC C3,G4,-,-
	MUSIC S,S,-
	MUSIC D3#,C4,-,-
	MUSIC E3,A4,-,-
	MUSIC S,S,-
	MUSIC G3,C4,-,-
	MUSIC C3,C5,-,-
	MUSIC S,S,-
	MUSIC A3,C4,-,-
	MUSIC G3,A4,-,-
	MUSIC S,S,-
	MUSIC C3,C5,-,-
	MUSIC C3,C4,-,-
	MUSIC S,S,-
	MUSIC D3#,A4,-,-
	MUSIC E3,F4#,-,-
	MUSIC S,S,-
	MUSIC G3,G4,-,-
	MUSIC C3,C4,-,-
	MUSIC S,S,-
	MUSIC A3,-,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC C3,C5,-,-
	MUSIC C3,C4,-,-
	MUSIC S,S,-
	MUSIC D3#,A4,-,-
	MUSIC E3,F4#,-,-
	MUSIC S,S,-
	MUSIC G3,G4,-,-
	MUSIC C3,C4,-,-
	MUSIC S,S,-
	MUSIC A3,-,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC C3,C4,-,-
	MUSIC C3,G4,-,-
	MUSIC S,S,-
	MUSIC D3#,A4,-,-
	MUSIC E3,G4,-,-
	MUSIC S,S,-
	MUSIC G3,C4,-,-
	MUSIC C3,-,-,-
	MUSIC S,-,-
	MUSIC A3,-,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC C3,C5,-,-
	MUSIC C3,C4,-,-
	MUSIC S,S,-
	MUSIC D3#,A4,-,-
	MUSIC E3,F4#,-,-
	MUSIC S,S,-
	MUSIC G3,G4,-,-
	MUSIC C3,C4,-,-
	MUSIC S,S,-
	MUSIC A3,-,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC C3,E5,-,-
	MUSIC C3,E5,-,-
	MUSIC S,S,-
	MUSIC D3#,E5,-,-
	MUSIC E3,E5,-,-
	MUSIC S,S,-
	MUSIC G3,E5,-,-
	MUSIC C3,E5,-,-
	MUSIC S,S,-
	MUSIC A3,E5,-,-
	MUSIC G3,E5,-,-
	MUSIC S,S,-
	MUSIC C3,C5,-,-
	MUSIC C3,C4,-,-
	MUSIC S,S,-
	MUSIC D3#,A4,-,-
	MUSIC E3,F4#,-,-
	MUSIC S,S,-
	MUSIC G3,G4,-,-
	MUSIC C3,C4,-,-
	MUSIC S,S,-
	MUSIC A3,-,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC F3,A5,-,-
	MUSIC F3,-,-,-
	MUSIC S,-,-
	MUSIC G3#,-,-,-
	MUSIC A3,-,-,-
	MUSIC S,-,-
	MUSIC C4,A5,-,-
	MUSIC F3,-,-,-
	MUSIC S,-,-
	MUSIC D4,-,-,-
	MUSIC C4,-,-,-
	MUSIC S,-,-
	MUSIC F3,F5,-,-
	MUSIC F3,F4,-,-
	MUSIC S,S,-
	MUSIC G3#,D5,-,-
	MUSIC A3,B4,-,-
	MUSIC S,S,-
	MUSIC C4,C5,-,-
	MUSIC F3,F4,-,-
	MUSIC S,S,-
	MUSIC D4,-,-,-
	MUSIC C4,-,-,-
	MUSIC S,-,-
	MUSIC F3,A5,-,-
	MUSIC F3,-,-,-
	MUSIC S,-,-
	MUSIC G3#,A5,-,-
	MUSIC A3,-,-,-
	MUSIC S,-,-
	MUSIC C4,A5,-,-
	MUSIC F3,-,-,-
	MUSIC S,-,-
	MUSIC D4,A5,-,-
	MUSIC C4,-,-,-
	MUSIC S,-,-
	MUSIC F3,F5,-,-
	MUSIC F3,F4,-,-
	MUSIC S,S,-
	MUSIC G3#,D5,-,-
	MUSIC A3,B4,-,-
	MUSIC S,S,-
	MUSIC C4,C5,-,-
	MUSIC F3,F4,-,-
	MUSIC S,S,-
	MUSIC D4,-,-,-
	MUSIC C4,-,-,-
	MUSIC S,-,-
	MUSIC G3,G4,-,-
	MUSIC G3,D5,-,-
	MUSIC S,S,-
	MUSIC A3#,G4,-,-
	MUSIC B3,E5,-,-
	MUSIC S,S,-
	MUSIC D4,G4,-,-
	MUSIC G3,G5,-,-
	MUSIC S,S,-
	MUSIC E4,G4,-,-
	MUSIC D4,E5,-,-
	MUSIC S,S,-
	MUSIC G3,G5,-,-
	MUSIC G3,G4,-,-
	MUSIC S,S,-
	MUSIC A3#,E5,-,-
	MUSIC B3,C5#,-,-
	MUSIC S,S,-
	MUSIC D4,D5,-,-
	MUSIC G3,G4,-,-
	MUSIC S,S,-
	MUSIC E4,-,-,-
	MUSIC D4,-,-,-
	MUSIC S,-,-
	MUSIC G3,G4,-,-
	MUSIC G3,D5,-,-
	MUSIC S,S,-
	MUSIC A3#,E5,-,-
	MUSIC B3,D5,-,-
	MUSIC S,S,-
	MUSIC D4,G4,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC E4,-,-,-
	MUSIC D4,-,-,-
	MUSIC S,-,-
	MUSIC G3,B5,-,-
	MUSIC G3,B5,-,-
	MUSIC S,S,-
	MUSIC A3#,B5,-,-
	MUSIC B3,B5,-,-
	MUSIC S,S,-
	MUSIC D4,B5,-,-
	MUSIC G3,B5,-,-
	MUSIC S,S,-
	MUSIC E4,B5,-,-
	MUSIC D4,B5,-,-
	MUSIC S,S,-
	MUSIC C3,C4,-,-
	MUSIC C3,G4,-,-
	MUSIC S,S,-
	MUSIC D3#,C4,-,-
	MUSIC E3,A4,-,-
	MUSIC S,S,-
	MUSIC G3,C4,-,-
	MUSIC C3,C5,-,-
	MUSIC S,S,-
	MUSIC A3,C4,-,-
	MUSIC G3,A4,-,-
	MUSIC S,S,-
	MUSIC C3,-,-
	MUSIC C3,-,-
	MUSIC S,-,-
	MUSIC D3#,-,-
	MUSIC E3,-,-
	MUSIC S,-,-
	MUSIC G3,-,-
	MUSIC C3,-,-
	MUSIC S,-,-
	MUSIC A3,-,-
	MUSIC G3,-,-
	MUSIC S,-,-
	MUSIC C3,E5,-,-
	MUSIC C3,E5,-,-
	MUSIC S,S,-
	MUSIC D3#,E5,-,-
	MUSIC E3,E5,-,-
	MUSIC S,S,-
	MUSIC G3,E5,-,-
	MUSIC C3,E5,-,-
	MUSIC S,S,-
	MUSIC A3,E5,-,-
	MUSIC G3,E5,-,-
	MUSIC S,S,-
	MUSIC C3,-,-
	MUSIC C3,-,-
	MUSIC S,-,-
	MUSIC D3#,-,-
	MUSIC E3,-,-
	MUSIC S,-,-
	MUSIC G3,-,-
	MUSIC C3,-,-
	MUSIC S,-,-
	MUSIC A3,-,-
	MUSIC G3,-,-
	MUSIC S,-,-
	MUSIC C3,E5,-,-
	MUSIC C3,-,-,-
	MUSIC S,-,-
	MUSIC D3#,E5,-,-
	MUSIC E3,-,-,-
	MUSIC S,-,-
	MUSIC G3,E5,-,-
	MUSIC C3,-,-,-
	MUSIC S,-,-
	MUSIC A3,E5,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC C3,-,-
	MUSIC C3,-,-
	MUSIC S,-,-
	MUSIC D3#,-,-
	MUSIC E3,-,-
	MUSIC S,-,-
	MUSIC G3,-,-
	MUSIC C3,-,-
	MUSIC S,-,-
	MUSIC A3,-,-
	MUSIC G3,-,-
	MUSIC S,-,-
	MUSIC C3,E5,-,-
	MUSIC C3,E5,-,-
	MUSIC S,S,-
	MUSIC D3#,E5,-,-
	MUSIC E3,E5,-,-
	MUSIC S,S,-
	MUSIC G3,E5,-,-
	MUSIC C3,E5,-,-
	MUSIC S,S,-
	MUSIC A3,E5,-,-
	MUSIC G3,E5,-,-
	MUSIC S,S,-
	MUSIC C3,-,-
	MUSIC C3,-,-
	MUSIC S,-,-
	MUSIC D3#,-,-
	MUSIC E3,-,-
	MUSIC S,-,-
	MUSIC G3,-,-
	MUSIC C3,-,-
	MUSIC S,-,-
	MUSIC A3,-,-
	MUSIC G3,-,-
	MUSIC S,-,-
	MUSIC C3,E5,-,-
	MUSIC C3,E5,-,-
	MUSIC S,S,-
	MUSIC D3#,E5,-,-
	MUSIC E3,E5,-,-
	MUSIC S,S,-
	MUSIC G3,E5,-,-
	MUSIC C3,E5,-,-
	MUSIC S,S,-
	MUSIC A3,E5,-,-
	MUSIC G3,E5,-,-
	MUSIC S,S,-
	MUSIC C3,C4,-,-
	MUSIC C3,G4,-,-
	MUSIC S,S,-
	MUSIC D3#,C4,-,-
	MUSIC E3,A4,-,-
	MUSIC S,S,-
	MUSIC G3,C4,-,-
	MUSIC C3,C5,-,-
	MUSIC S,S,-
	MUSIC A3,C4,-,-
	MUSIC G3,A4,-,-
	MUSIC S,S,-
	MUSIC G3,G4,-,-
	MUSIC G3,D5,-,-
	MUSIC S,S,-
	MUSIC A3#,E5,-,-
	MUSIC B3,D5,-,-
	MUSIC S,S,-
	MUSIC D4,G4,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC E4,-,-,-
	MUSIC D4,-,-,-
	MUSIC S,-,-
	MUSIC G3,B5,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC A3#,B5,-,-
	MUSIC B3,-,-,-
	MUSIC S,-,-
	MUSIC D4,B5,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC E4,B5,-,-
	MUSIC D4,-,-,-
	MUSIC S,-,-
	MUSIC C3,C4,-,-
	MUSIC C3,G4,-,-
	MUSIC S,S,-
	MUSIC D3#,C4,-,-
	MUSIC E3,A4,-,-
	MUSIC S,S,-
	MUSIC G3,C4,-,-
	MUSIC C3,C5,-,-
	MUSIC S,S,-
	MUSIC A3,C4,-,-
	MUSIC G3,A4,-,-
	MUSIC S,S,-
	MUSIC C3,-,-
	MUSIC C3,-,-
	MUSIC S,-,-
	MUSIC D3#,-,-
	MUSIC E3,-,-
	MUSIC S,-,-
	MUSIC G3,-,-
	MUSIC C3,-,-
	MUSIC S,-,-
	MUSIC A3,-,-
	MUSIC G3,-,-
	MUSIC S,-,-
	MUSIC C3,E5,-,-
	MUSIC C3,-,-,-
	MUSIC S,-,-
	MUSIC D3#,-,-,-
	MUSIC E3,-,-,-
	MUSIC S,-,-
	MUSIC G3,E5,-,-
	MUSIC C3,-,-,-
	MUSIC S,-,-
	MUSIC A3,-,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC C3,-,-
	MUSIC C3,-,-
	MUSIC S,-,-
	MUSIC D3#,-,-
	MUSIC E3,-,-
	MUSIC S,-,-
	MUSIC G3,-,-
	MUSIC C3,-,-
	MUSIC S,-,-
	MUSIC A3,-,-
	MUSIC G3,-,-
	MUSIC S,-,-
	MUSIC C3,E5,-,-
	MUSIC C3,E5,-,-
	MUSIC S,S,-
	MUSIC D3#,E5,-,-
	MUSIC E3,E5,-,-
	MUSIC S,S,-
	MUSIC G3,E5,-,-
	MUSIC C3,E5,-,-
	MUSIC S,S,-
	MUSIC A3,E5,-,-
	MUSIC G3,E5,-,-
	MUSIC S,S,-
	MUSIC C3,C4,-,-
	MUSIC C3,G4,-,-
	MUSIC S,S,-
	MUSIC D3#,A4,-,-
	MUSIC E3,G4,-,-
	MUSIC S,S,-
	MUSIC G3,C4,-,-
	MUSIC C3,-,-,-
	MUSIC S,-,-
	MUSIC A3,-,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC F3,F5,-,-
	MUSIC F3,F4,-,-
	MUSIC S,S,-
	MUSIC G3#,D5,-,-
	MUSIC A3,B4,-,-
	MUSIC S,S,-
	MUSIC C4,C5,-,-
	MUSIC F3,F4,-,-
	MUSIC S,S,-
	MUSIC D4,-,-,-
	MUSIC C4,-,-,-
	MUSIC S,-,-
	MUSIC F3,F5,-,-
	MUSIC F3,F4,-,-
	MUSIC S,S,-
	MUSIC G3#,D5,-,-
	MUSIC A3,B4,-,-
	MUSIC S,S,-
	MUSIC C4,C5,-,-
	MUSIC F3,F4,-,-
	MUSIC S,S,-
	MUSIC D4,-,-,-
	MUSIC C4,-,-,-
	MUSIC S,-,-
	MUSIC F3,A5,-,-
	MUSIC F3,-,-,-
	MUSIC S,-,-
	MUSIC G3#,-,-,-
	MUSIC A3,-,-,-
	MUSIC S,-,-
	MUSIC C4,A5,-,-
	MUSIC F3,-,-,-
	MUSIC S,-,-
	MUSIC D4,-,-,-
	MUSIC C4,-,-,-
	MUSIC S,-,-
	MUSIC F3,F4,-,-
	MUSIC F3,C5,-,-
	MUSIC S,S,-
	MUSIC G3#,D5,-,-
	MUSIC A3,C5,-,-
	MUSIC S,S,-
	MUSIC C4,F4,-,-
	MUSIC F3,-,-,-
	MUSIC S,-,-
	MUSIC D4,-,-,-
	MUSIC C4,-,-,-
	MUSIC S,-,-
	MUSIC G3,G4,-,-
	MUSIC G3,D5,-,-
	MUSIC S,S,-
	MUSIC A3#,G4,-,-
	MUSIC B3,E5,-,-
	MUSIC S,S,-
	MUSIC D4,G4,-,-
	MUSIC G3,G5,-,-
	MUSIC S,S,-
	MUSIC E4,G4,-,-
	MUSIC D4,E5,-,-
	MUSIC S,S,-
	MUSIC G3,B5,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC A3#,-,-,-
	MUSIC B3,-,-,-
	MUSIC S,-,-
	MUSIC D4,B5,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC E4,-,-,-
	MUSIC D4,-,-,-
	MUSIC S,-,-
	MUSIC G3,G4,-,-
	MUSIC G3,D5,-,-
	MUSIC S,S,-
	MUSIC A3#,G4,-,-
	MUSIC B3,E5,-,-
	MUSIC S,S,-
	MUSIC D4,G4,-,-
	MUSIC G3,G5,-,-
	MUSIC S,S,-
	MUSIC E4,G4,-,-
	MUSIC D4,E5,-,-
	MUSIC S,S,-
	MUSIC G3,B5,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC A3#,B5,-,-
	MUSIC B3,-,-,-
	MUSIC S,-,-
	MUSIC D4,B5,-,-
	MUSIC G3,-,-,-
	MUSIC S,-,-
	MUSIC E4,B5,-,-
	MUSIC D4,-,-,-
	MUSIC S,-,-
	MUSIC REPEAT

	'
	' Boss "music"
	'
music_beat:
	DATA 6
	MUSIC C2W,-,-
	MUSIC S,-,-
	MUSIC S,-,-
	MUSIC -,-,-
	MUSIC C2#W,-,-
	MUSIC S,-,-
	MUSIC S,-,-
	MUSIC -,-,-
	MUSIC REPEAT
