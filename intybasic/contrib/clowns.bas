	REM
	REM Clowns and Balloons game
	REM Created in IntyBASIC
	REM by Catsfolly
	REM Feb 2014.
	REM

	REM Include useful predefined constants
	INCLUDE "constants.bas"

	CONST heart_round_index	= 10
	CONST ghost_round_index	= 11
	CONST round_data_count  = 12

	DEF FN fix7_to_Int(afix) = (afix / 128)
	DEF FN Int_to_fix7(aInt) = (aInt * 128)

REM *****************************************************************
REM
REM Title routine - displays the title page
REM
REM *****************************************************************
title:
	cls		' clear the screen

	REM disable the sprites
	SPRITE 0,0,0,0
	SPRITE 1,0,0,0
	SPRITE 2,0,0,0
	SPRITE 3,0,0,0
	SPRITE 4,0,0,0
	WAIT

    REM the screen is 20 chars wide so position is 20 * row + col
	print at SCREENPOS(5, 3),"Clowns and"
	print at SCREENPOS(6, 4),"Balloons"
	print at SCREENPOS(8, 5),"Beta"

	print at SCREENPOS(3,  9) COLOR CS_BLUE,"Created with"
	print at SCREENPOS(3, 10) COLOR CS_RED, "IntyBasic 1.1"

	title_lp:
	REM loop until a button or the disc is pressed
	IF (CONT.button + CONT.left + CONT.right + CONT.up + CONT.down)=0 THEN GOTO title_lp

REM *****************************************************************
REM
REM RESET - start a new game
REM
REM *****************************************************************

RESET:




	round=1 ' start at round one

	speed_index = 0
restart:

	lives=3	' 3 lives at the start

	rem the score is stored in BCD (Binary coded decimal) in 3 bytes
	rem s1 is the most significant byte
	s1=0
	s2=0
	s3=0

new_round:	' go here to start a new round
	cls
	WAIT
	SPRITE 0,0,0,0
	SPRITE 1,0,0,0
	SPRITE 2,0,0,0
	SPRITE 3,0,0,0
	SPRITE 4,0,0,0

	WAIT

	round_data_index = (round-1) % round_data_count

	if round_data_index = ghost_round_index then goto load_ghosts

	DEFINE DEF10,10,gramchars10 ' load the custom character definitions
	WAIT
	DEFINE DEF20,10,gramchars20
	WAIT
	DEFINE DEF30,10,gramchars30 ' load the custom character definitions
	WAIT
	goto new_round_common

load_ghosts:
	DEFINE DEF10,10,ghostgrams10 ' load the custom character definitions
	WAIT
	DEFINE DEF20,10,ghostgrams20
	WAIT
	DEFINE DEF30,10,ghostgrams30 ' load the custom character definitions
	WAIT


new_round_common:
	DEFINE DEF00,10,gramchars ' load the custom character definitions
	WAIT
	DEFINE DEF40,10,gramchars40
	wait
	if (round_data_index = heart_round_index) then DEFINE DEF18,1,fushek_balloon : wait


	print at (screenpos(8,4)) COLOR CS_WHITE, "ROUND"
	print at (screenpos(9,5)),<3> round

	for a=0 to 60 ' wait about a second
		WAIT
	NEXT a
	cls

	gosub init_clown
	red1 = 0	' if nonzero, the stretcher carriers turn red
	red2 = 0
	bounces=0	' count the bounces
	speed_index = 0

	saf = 0		' stretcher animation freme
	X = 80		' stretcher x position
	ground_hit=0 ' if one, the clown has hit the ground

	GOSUB display_score		' draw the score and bounces and lives
	GOSUB init_balloons3		' init the balloons for this round
	for A=0 to BACKGROUND_COLUMNS	' draw a line across the screen dividing game and hud
		poke SCREENADDR(a, 10), BG49 + CS_DARKGREEN
	next a

REM *****************************************************************
REM
REM loop - main gameplay loop
REM
REM *****************************************************************
loop:
	WAIT
	GOSUB draw_stretcher

	REM SPRITE 4 is the clown
	IF round_data_index = ghost_round_index THEN #clown_color = SPR_WHITE ELSE #clown_color = SPR_YELLOW
	#af = SPR12	' chose a picture 12 is floating, 14 is rising, 16 is falling
	IF #cyv < -10 THEN #af = SPR14 ELSE if #cyv > 10 THEN #af = SPR16
	SPRITE 4,fix7_to_Int(#cx) + HIT + VISIBLE +8 ,fix7_to_Int(#cy)+  DOUBLEY, #clown_color + #af

	IF saf > 0 THEN saf = saf -1	' animate the stretcher
	GOSUB move_player		' move the stretcher
	GOSUB move_clown		' move the clown, collide with balloons, stretcher and ground
	GOSUB display_score		' draw the score and bounces and lives
	GOSUB update_balloons		' animate exploding balloons, count balloons

	rem if the balloon count is complete and the count is zero, go to round finished code
	if ((frame AND 3)=3) AND (balloon_count=0) GOTO did_it

	if ground_hit=0 GOTO loop	' if we didn't hit the ground, go back to loop
REM main gameplay loop end *****************************************************************


rem clown death sequence
	#bf = SPR38	' animation frame for flying dust sprite
	rem ground hit sound
	SOUND 0,$333,8 ' freq and vol
	SOUND 3,$73f,0 ' evelope duration
	POKE $01fB,$38 ' use envelope

ground_loop:

	WAIT
		if #af > SPR37 THEN SPRITE 5,0 ,0,0 : goto do_4
		SPRITE 5,fix7_to_Int(#cx) + HIT + VISIBLE +8,fix7_to_Int(#cy) + DOUBLEY , #clown_color + #af

do_4:
		SPRITE 4,fix7_to_Int(#cx) + HIT + VISIBLE +8,fix7_to_Int(#cy) + ZOOMY2,SPR_DARKGREEN + #bf
		if (FRAME AND 7) = 7 THEN #af=#af+16: #bf= #bf+8
		GOSUB draw_stretcher
		GOSUB move_player


		if(#af < SPR48) GOTO ground_loop
		for a=0 to 60
			WAIT
			SPRITE 4,0 ,0,0
			SPRITE 5,0 ,0,0
			GOSUB draw_stretcher
			GOSUB move_player

		NEXT a
	lives = lives-1
	if (lives < 1) THEN goto game_over
	GOSUB init_clown
	GOTO loop
REM *****************************************************************
REM
REM game_over - display game over message, give a chance to continue
REM
REM *****************************************************************
game_over:

	WAIT

	print at SCREENPOS(5, 6) COLOR CS_WHITE, "GAME OVER"
	for a=0 to 120
		WAIT
	NEXT a
	continue=9
	print at SCREENPOS(5, 7) COLOR CS_YELLOW, "CONTINUE?"
pre_loop:
	if (frame AND $3f)> 0 goto pre_loop

game_over_loop:
	WAIT
	print at (SCREENPOS(9, 8)),(continue+16)*8+CS_YELLOW
	if (frame AND $3f)=$3f THEN continue=continue-1

	IF (CONT.button + CONT.left + CONT.right + CONT.up + CONT.down)>0 THEN  GOTO restart
	if (continue>0) GOTO game_over_loop
	goto title
REM *****************************************************************
REM
REM did it - display finished round message
REM
REM *****************************************************************
did_it:
	cls
	WAIT
	SPRITE 0,0,0,0
	SPRITE 1,0,0,0
	SPRITE 2,0,0,0
	SPRITE 3,0,0,0
	SPRITE 4,0,0,0

	print at  SCREENPOS(8, 2) COLOR CS_WHITE, "ROUND"
	print at (SCREENPOS(9, 3)),<3> round
	print at  SCREENPOS(6, 4),"COMPLETE!"

	print at  SCREENPOS(7, 7) COLOR CS_RED, "BOUNCES"
	print at (SCREENPOS(9, 8)),<3> bounces
	if (bounces < 30) THEN poke SCREENADDR( 7, 9), BG48 + CS_YELLOW
	if (bounces < 23) THEN poke SCREENADDR(10, 9), BG48 + CS_YELLOW
	if (bounces < 15) THEN poke SCREENADDR(13, 9), BG48 + CS_YELLOW

	for a=0 to 120
		WAIT
	NEXT a

did_it_loop:
	IF (CONT.button + CONT.left + CONT.right + CONT.up + CONT.down)=0 THEN GOTO did_it_loop

	round= round+1: GOTO new_round


REM *****************************************************************
REM
REM add_points - adds point to the score, points must be in bcd format
REM
REM *****************************************************************

add_points: PROCEDURE
	rem temp has the points (must be less than 100
	temp2 = s3+ temp
	carry= 0
	gosub fix_bcd
	s3=temp2
	if (carry>0) THEN temp2=s2: gosub fix_bcd:s2=temp2
	if (carry>0) THEN temp2=s1: gosub fix_bcd:s1=temp2
	RETURN
	END

fix_bcd: PROCEDURE
	rem value in temp2, read/set carry
	if carry>0 THEN temp2=temp2+1: carry=0
	if ((temp2 AND $0f) > 9) THEN temp2= temp2+6
	if (temp2 > $99) THEN temp2 = (temp2+ $060) AND $00ff: carry=1
	RETURN
	END
REM *****************************************************************
REM
REM display score - draw the score and bounces and lifes
REM
REM *****************************************************************
display_score:	PROCEDURE

	rem print lives
	print at SCREENPOS(17, 11) COLOR CS_RED,"L:",(lives+15)*8+CS_YELLOW

	rem print bounces
	print at SCREENPOS(10, 11) COLOR CS_RED,"B:"
	IF bounces < 10 THEN PRINT <1>bounces : goto ds_done
	IF bounces < 100 THEN PRINT <2> bounces : GOTO ds_done
	PRINT <3> bounces
ds_done:

	rem print score
	print at SCREENPOS(0, 11) COLOR CS_RED,"S:"

	if (s1=0 ) GOTO ds_trys2
	if (s1> 9) THEN PRINT (s1/16 +16)*8+CS_YELLOW
	PRINT ((s1 AND $0f)+16) *8+CS_YELLOW
	PRINT (s2/16 +16)*8+6,((s2 AND $0f)+16) *8+CS_YELLOW
	PRINT (s3/16 +16)*8+6,((s3 AND $0f)+16) *8+CS_YELLOW
	RETURN

ds_trys2:
	if (s2= 0) GOTO ds_trys3
	if (s2> 9) THEN PRINT (s2/16 +16)*8+CS_YELLOW
	PRINT ((s2 AND $0f)+16) *8+CS_YELLOW
	PRINT (s3/16 +16)*8+6,((s3 AND $0f)+16) *8+CS_YELLOW
	RETURN
ds_trys3:
	if (s3> 9) THEN PRINT (s3/16 +16)*8+CS_YELLOW
	PRINT ((s3 AND $0f)+16) *8+CS_YELLOW


	RETURN
	END
REM *****************************************************************
REM
REM move_player - currently supports left control only
REM
REM *****************************************************************
move_player:	PROCEDURE
	IF CONT.left THEN  if (X > 8) THEN X=X-1
	IF CONT.right THEN IF (X<128) THEN X=X+1

	RETURN
	END

REM *****************************************************************
REM
REM draw_stretcher - draw the stretcher and the carriers
REM
REM *****************************************************************
draw_stretcher:	PROCEDURE

    REM SPRITE 0 is the left stretcher holder
	temp = 0
	if red1 > 0 THEN red1 = red1-1: temp = (4 * 8) + (SPR_RED - SPR_WHITE) ' add (4 *8) to choose pushed head animation, change color from white to red
	SPRITE 0,X + HIT + VISIBLE,84+ZOOMY2,GRAM + SPR_WHITE + ((X AND 3) * 8) + temp

	REM SPRITE 1 is the right stretcher holder
	temp = 0
	if red2 > 0 THEN red2 = red2-1: temp = (4 * 8) + (SPR_RED - SPR_WHITE) ' add (4 *8) to choose pushed head animation, -5 changes color from white to red

	SPRITE 1,((X + 32) AND $0ff) + HIT + VISIBLE,84+ZOOMY2+ FLIPX,GRAM + ((3-(X AND 3)) * 8) + SPR_WHITE + temp
	REM SPRITES 2 and 3 are the stretcher

	if saf = 0 THEN temp= (8 * 8) ELSE read temp

	SPRITE 2, ZOOMX2 + HIT + VISIBLE+((X +  8) AND $0ff)  ,84+ZOOMY2        + ((temp AND $80) * 16),GRAM + SPR_BLUE + (temp and $7f)
	SPRITE 3, ZOOMX2 + HIT + VISIBLE+((X + 16) AND $0ff)  ,84+ZOOMY2+ FLIPX + ((temp AND $80) * 16),GRAM + SPR_BLUE + (temp and $7f)
	RETURN

	REM if (X<9) AND (CONT.left>0) GOTO push_left
	REM if (X>127) AND (CONT.right>0) GOTO push_right


	END

	CONST STRETCHER_LEFT	=	( 4)
	CONST STRETCHER_RIGHT	=	( 8)
	CONST STRETCHER_CENTER	=	(12)

ymax:
	data 23 * 8
	data 23 * 8
	data 24 * 8
	data 24 * 8

	data 25 * 8
	data 25 * 8
	data 26 * 8
	data 26 * 8

	data 27 * 8
	data 27 * 8
	data 28 * 8
	data 28 * 8

	data 29 * 8
	data 29 * 8
	data 30 * 8
	data 30 * 8
bounce_boost_lr:
	data 2 * 8
	data 2 * 8
	data 2 * 8
	data 2 * 8

	data 3 * 8
	data 3 * 8
	data 3 * 8
	data 3 * 8

	data 4 * 8
	data 4 * 8
	data 4 * 8
	data 4 * 8

	data 5 * 8
	data 5 * 8
	data 5 * 8
	data 5 * 8

bounce_boost_center:
	data 4 * 8
	data 4 * 8
	data 4 * 8
	data 5 * 8

	data 5 * 8
	data 5 * 8
	data 6 * 8
	data 6 * 8

	data 6 * 8
	data 7 * 8
	data 7 * 8
	data 7 * 8

	data 7 * 8
	data 8 * 8
	data 8 * 8
	data 8 * 8

initial_bounce_v:
	data 2 * 8
	data 2 * 8
	data 2 * 8
	data 2 * 8

	data 2 * 8
	data 3 * 8
	data 3 * 8
	data 3 * 8

	data 3 * 8
	data 3 * 8
	data 4 * 8
	data 4 * 8

	data 4 * 8
	data 4 * 8
	data 4 * 8
	data 4 * 8

second_bounce_v:
	data 5 * 8
	data 5 * 8
	data 5 * 8
	data 6 * 8

	data 6 * 8
	data 6 * 8
	data 7 * 8
	data 7 * 8

	data 7 * 8
	data 8 * 8
	data 8 * 8
	data 8 * 8

	data 9 * 8
	data 9 * 8
	data 9 * 8
	data 10 * 8

gravity:
	data 2,2,2,2
	data 3,3,3,3
	data 3,3,3,3
	data 4,4,4,4

side_speed:
	data 10 * 8
	data 10 * 8
	data 11 * 8
	data 11 * 8

	data 12 * 8
	data 12 * 8
	data 13 * 8
	data 13 * 8

	data 14 * 8
	data 14 * 8
	data 15 * 8
	data 16 * 8

	data 17 * 8
	data 18 * 8
	data 19 * 8
	data 20 * 8







REM *****************************************************************
REM
REM move_clown - move the clown, collide with ground, stretcher and balloons
REM
REM *****************************************************************
move_clown:	PROCEDURE
    REM check direction of movement
    IF (#cyv < 0) GOTO up

	REM check for collision with stretcher
	test = COL4 AND (HIT_SPRITE2 + HIT_SPRITE3)
	IF test =0 GOTO staff
	REM if we get here we are moving down and hittng the stretcher

	if (X<9) AND (CONT.left>0) THEN test=STRETCHER_RIGHT     ' if player pushes  left, make the clown go right
	if (X>127) AND (CONT.right>0) THEN test= STRETCHER_LEFT  ' if player pushes right, make the clown go left

	if (#cyv > ymax(speed_index)) then #cyv = ymax(speed_index) 'limit y speed so we don't wrap around
	if test=STRETCHER_LEFT  THEN #cyv = 0 - #cyv - bounce_boost_lr(speed_index) : if #cxv<0 THEN #cxv = -second_bounce_v(speed_index) ELSE #cxv = -initial_bounce_v(speed_index)
	if test=STRETCHER_CENTER then  #cyv = 0 - #cyv -bounce_boost_center(speed_index) : #cxv =  0
	if test=STRETCHER_RIGHT  then #cyv = 0 - #cyv - bounce_boost_lr(speed_index) : if #cxv> 0 THEN #cxv = second_bounce_v(speed_index) ELSE #cxv =  initial_bounce_v(speed_index)

allx:
	saf = 14
	RESTORE stretcher

	SOUND 0,$540 +(#cyv *2) ,8 ' was %540
	SOUND 3,$53f,0
	POKE $01fB,$38 ' use envelope
	bounces=bounces+1
	if bounces > 30 then speed_index = 15 else speed_index = bounces /2


	GOTO move

staff:
	rem check collision with stretcher carriers
	test = COL4 AND  (HIT_SPRITE0 + HIT_SPRITE1)
	if (test=0) THEN GOTO up
	#cyv = 0 - ((#cyv * 3 )/4)

	if test=HIT_SPRITE0 THEN red1=30:#cxv=-side_speed(speed_index)
	if test=HIT_SPRITE1 THEN red2=30:#cxv= side_speed(speed_index)

	rem robot head bounce sound
	SOUND 0,$140,8
	SOUND 3,$33f,0
	POKE $01fB,$38 ' use envelope
	if (bounces < 255) THEN bounces=bounces+1



	GOTO move

up:
	rem going up so check ballon collisions
	if (COL4 AND HIT_BACKGROUND)=0 THEN GOTO move
	REM looks like a collision

	#bg_row = (fix7_to_Int(#cy) -4)/8
	#bg_col = (fix7_to_int(#cx))/8

	#temp = screenaddr(#bg_col,#bg_row)

	#value  = PEEK(#temp)
	if (#value AND CS_CARD_DATA_MASK) = (BG18 AND CS_CARD_DATA_MASK)  THEN GOTO DING
	#temp = #temp+1
	#value  = PEEK(#temp)
	if (#value AND CS_CARD_DATA_MASK) = (BG18 AND CS_CARD_DATA_MASK) THEN  GOTO DING
	GOTO move
DING:
	POKE #temp,#value+8 ' start the balloon animation
	temp = 5
	GOSUB add_points

	SOUND 0,$060,8
	SOUND 3,$43f,0
	POKE $01fB,$38 ' use envelope


move:
	rem move the clown
    ground_hit=0
	#cyv = #cyv + gravity(speed_index)
	#cy  = #cy + #cyv
	#cx  = #cx + #cxv
	IF #cy > Int_to_fix7(88) THEN ground_hit=1:#af= SPR28 ' was 86
	IF (#cx < 0) THEN #cx=0:if(#cxv<0) THEN #cxv = 0 - #cxv : GOTO wall_sound
	IF (#cx > Int_to_fix7(152)) THEN #cx=Int_to_fix7(152): IF (#cxv > 0) THEN #cxv = 0 - #cxv : goto wall_sound

	RETURN
wall_sound:

	rem wall bounce sound
	SOUND 0,$a80,8
	SOUND 3,$33f,0
	POKE $01fB,$38 ' use envelope

	RETURN
	END

REM *****************************************************************
REM
REM init_clown - put the clown in the center of the screen, no velocity
REM
REM *****************************************************************
init_clown: PROCEDURE
	#cx = Int_to_fix7(80)
	#cy = Int_to_fix7(48)
	#cxv = 0
	#cyv = 0
	RETURN
	END


REM *****************************************************************
REM
REM init_balloons3 - draw balloons for this round
REM
REM *****************************************************************
init_balloons3: PROCEDURE

	for a=0 to 24
		temp = (round-1) % round_data_count
		#temp = #bt_3( ((temp) * 25) + a)
		gosub do_a_word
	next a
		balloon_count = 1 ' init to nonzero value
	RETURN
	END

rem take one word from the table and draw 4 balloons based on the values in the word

do_a_word: procedure
	for nibble = 4 to 1 step -1
	    balloon_color = #temp AND $000f
	    if balloon_color = 0 then goto daw_skip
	    #value = BG18 + balloon_color

	    if (balloon_color > 7) then #value = #value + (CS_GREY - 8)
	    poke BACKTAB_ADDR + (a *4) + (nibble-1),#value
daw_skip:
	    #temp = #temp /16
	next nibble
	END



REM *****************************************************************
REM
REM update_balloons - advance balloon animations, count balloons
REM the work is spread out over 4 frames
REM
REM *****************************************************************

update_balloons: PROCEDURE
  test= frame AND 3
  if (test) = 0 THEN balloon_count = 0
  start = (test * 16) + (test * 8) +test ' test * 25
  FOR A=start TO start+24 ' Loop
  #temp = PEEK(BACKTAB_ADDR +A)		' read the screen location
  if #temp=0 THEN GOTO ub_next	' if zero, advance
  balloon_count = balloon_count+1	' if not zero, count it as a balloon
  if (#temp AND CS_CARD_DATA_MASK) =(18*8) GOTO ub_next	' if not exploding frame, go to next location
  if (#temp AND CS_CARD_DATA_MASK) > (25*8) THEN POKE (BACKTAB_ADDR+A), 0: GOTO ub_next ' if animation is done, clear the location
  POKE (BACKTAB_ADDR + A), #temp + 8 ' advance to next animation frame


  ub_next:
  NEXT A
	RETURN
	END


rem stretcher animation - gram number * 8 plus $80 if we y flip the picture
stretcher:
data	10 * 8
data	10 * 8
data     9 * 8
data     9 * 8
data     8 * 8
data     8 * 8
data     9 * 8 + $80
data     9 * 8 + $80
data    10 * 8 + $80
data    10 * 8 + $80
data     9 * 8 + $80
data     9 * 8 + $80
data     8 * 8
data     8 * 8
data     9 * 8
data     8 * 8
data     8 * 8
data     8 * 8

rem balloon tables - each digit is a color from the above tablez


#bt_3:
	REM round 1 (Nano)
	DATA $0011,$1022,$2011,$1033,$3000
	DATA $0010,$1020,$2010,$1030,$3000
	DATA $0010,$1022,$2010,$1030,$3000
	DATA $0010,$1020,$2010,$1033,$3000
	DATA $0000,$0000,$0000,$0000,$0000

	REM round 2 Curtains
	DATA $0511,$0000,$0000,$0000,$1150
	DATA $0451,$1111,$1111,$1111,$1540
	DATA $0045,$5555,$5555,$5555,$5400
	DATA $0004,$4444,$4444,$4444,$4000
	DATA $0000,$0000,$0000,$0000,$0000

	REM round 3 dz
	DATA $0009,$0777,$7000,$0999,$9900
	DATA $0009,$0000,$7000,$9777,$7790
	DATA $0999,$0007,$0000,$9797,$9790
	DATA $0909,$0070,$0000,$9777,$7790
	DATA $0999,$0777,$7000,$9777,$7790

	REM round 4 pattern
	DATA $00ff,$ffff,$ffff,$ffff,$fff0
	DATA $0011,$f111,$11f1,$1111,$f110
	DATA $00f1,$f1f1,$f1f1,$f1f1,$f1f0
	DATA $00ff,$fff1,$ffff,$f1ff,$fff0
	DATA $0011,$1111,$1111,$1111,$1110

	REM round 5 cmart
	DATA $6602,$0002,$0ccc,$0999,$0aaa
	DATA $6002,$2022,$0c0c,$0909,$00a0
	DATA $6002,$0202,$0ccc,$0990,$00a0
	DATA $6602,$0002,$0c0c,$0909,$00a0
	DATA $0000,$0000,$0000,$0000,$0000

	REM round 6 rev
	DATA $0009,$9909,$9909,$0909,$0000
	DATA $0009,$0909,$0009,$0909,$0000
	DATA $0009,$9009,$9909,$0909,$0000
	DATA $0009,$0909,$0009,$0900,$0000
	DATA $0009,$0909,$9900,$9009,$0000

	REM round 7 inferno
	DATA $0000,$000a,$cccc,$a000,$0000
	DATA $0000,$00ac,$eeee,$ca00,$0000
	DATA $0000,$0ace,$FFFF,$eca0,$0000
	DATA $0000,$00ac,$eeee,$ca00,$0000
	DATA $0000,$000a,$cccc,$a000,$0000

	REM round 8 wave
	DATA $0000,$1111,$1111,$0000,$0000
	DATA $0001,$dddd,$dddd,$1000,$0000
	DATA $001d,$ffff,$ffff,$d100,$0000
	DATA $01df,$7777,$7777,$fd10,$0000
	DATA $0000,$0000,$0000,$0000,$0000

	REM round 9 fish
	DATA $04e0,$00e4,$eeee,$e000,$00d0
	DATA $04ee,$0ee4,$ee0e,$ee00,$00d0
	DATA $00ee,$4ee4,$ee4e,$0000,$00d0
	DATA $04ee,$0ee4,$ee4e,$0000,$d0d0
	DATA $04e0,$00e4,$ee4e,$e000,$d220

	REM round 10 rocket
	DATA $0000,$0005,$1100,$0000,$0000
	DATA $02a0,$0805,$1111,$0000,$0000
	DATA $0002,$0885,$1111,$1100,$0000
	DATA $02a0,$0805,$1111,$1150,$0202
	DATA $0000,$0005,$5555,$5550,$0000

	REM round 11 I heart you (Fushek)
	DATA $0CCC,$0020,$2000,$c00C,$0C00
	DATA $00C0,$02c2,$c200,$c00c,$0C00
	DATA $00C0,$02cc,$c200,$c00c,$0C00
	DATA $00C0,$002c,$2000,$c00c,$0000
	DATA $0CCC,$0002,$0000,$cccc,$0C00

	REM round 12 ghost
	DATA $0272,$0000,$6000,$0052,$5000
	DATA $2272,$20DD,$6DD0,$5552,$5550
	DATA $7777,$7066,$6660,$2222,$2220
	DATA $2272,$20DD,$6DD0,$5552,$5550
	DATA $2272,$20DD,$6DD0,$5552,$5550





	REM blank
	DATA $0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000


	REM blank
	DATA $0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000

	rem wip levels
	REM fish
	DATA $0ee0,$00ee,$eeee,$e000,$00d0
	DATA $0eee,$0eee,$ee0e,$ee00,$0010
	DATA $00ee,$eeee,$eeee,$e000,$0010
	DATA $0eee,$0eee,$eeee,$0000,$1010
	DATA $0ee0,$00ee,$eeee,$e000,$1110

	REM  squares
	DATA $0ff0,$0dd0,$0bb0,$0bb0,$0000
	DATA $0ff0,$0dd0,$0bb0,$0bb0,$0000
	DATA $000e,$e00c,$c00a,$a00c,$c000
	DATA $000e,$e00c,$c00a,$a00c,$c000
	DATA $0000,$0000,$0000,$0000,$0000

	REM  faces
	DATA $0077,$7777,$0000,$7777,$7700
	DATA $0771,$7177,$7007,$7177,$1770
	DATA $0777,$7777,$7007,$7777,$7770
	DATA $0727,$7727,$7007,$7722,$7770
	DATA $0072,$2277,$0000,$7277,$2700

		REM battery
	DATA $0000,$dddd,$dddd,$6000,$0000
	DATA $0000,$d2dd,$dddd,$6000,$0000
	DATA $0006,$222d,$dd11,$6000,$0000
	DATA $0000,$d2dd,$dddd,$6000,$0000
	DATA $0000,$dddd,$dddd,$6000,$0000

	REM sine wave
	DATA $7ddd,$dd7d,$dddd,$7ddd,$dd71
	DATA $17dd,$d7d7,$ddd7,$07dd,$d711
	DATA $117d,$7111,$7d71,$117d,$7111
	DATA $1117,$1111,$1711,$1117,$1111
	DATA $0000,$0000,$0000,$0000,$0000


gramchars:

REM 0

	BITMAP "...####."
	BITMAP "...###.."
	BITMAP "...####."
	BITMAP "....#..."

	BITMAP "..######"
	BITMAP "..#.#..."
	BITMAP "....#..."
	BITMAP ".....#.."

REM 1

	BITMAP "...####."
	BITMAP "...###.."
	BITMAP "...####."
	BITMAP "....#..."

	BITMAP "..######"
	BITMAP "..#.#..."
	BITMAP "....##.."
	BITMAP "....#..."

REM 2

	BITMAP "...####."
	BITMAP "...###.."
	BITMAP "...####."
	BITMAP "....#..."

	BITMAP "..######"
	BITMAP "..#.#..."
	BITMAP "....#.#."
	BITMAP "...#...."

REM 3

	BITMAP "...####."
	BITMAP "...###.."
	BITMAP "...####."
	BITMAP "....#..."

	BITMAP "..######"
	BITMAP "..#.#..."
	BITMAP "...##..."
	BITMAP "......#."

REM 4
	BITMAP "........"
	BITMAP "...####."
	BITMAP "...###.."
	BITMAP "...####."

	BITMAP "..######"
	BITMAP "..#.#..."
	BITMAP "....#..."
	BITMAP ".....#.."

REM 5
	BITMAP "........"
	BITMAP "...####."
	BITMAP "...###.."
	BITMAP "...####."


	BITMAP "..######"
	BITMAP "..#.#..."
	BITMAP "....##.."
	BITMAP "....#..."

REM 6
	BITMAP "........"
	BITMAP "...####."
	BITMAP "...###.."
	BITMAP "...####."


	BITMAP "..######"
	BITMAP "..#.#..."
	BITMAP "....#.#."
	BITMAP "...#...."

REM 7
	BITMAP "........"
	BITMAP "...####."
	BITMAP "...###.."
	BITMAP "...####."


	BITMAP "..######"
	BITMAP "..#.#..."
	BITMAP "...##..."
	BITMAP "......#."

REM 8

	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"

	BITMAP "######.."
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"


REM 9

	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"

	BITMAP "#......."
	BITMAP ".#####.."
	BITMAP "........"
	BITMAP "........"

gramchars10:

REM 10

	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"

	BITMAP "#......."
	BITMAP ".#......"
	BITMAP "..####.."
	BITMAP "........"

REM 11

	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"

	BITMAP "########"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"

REM 12,13

	BITMAP "...##..."
	BITMAP "...##..."
	BITMAP "........"
	BITMAP "########"
	BITMAP "#.####.#"
	BITMAP "#.####.#"
	BITMAP ".######."
	BITMAP "..####.."

	BITMAP "..####.."
	BITMAP "..#..#.."
	BITMAP ".##..##."
	BITMAP ".#....#."
	BITMAP ".#....#."
	BITMAP ".#....#."
	BITMAP "##....##"
	BITMAP "........"

	REM 14, 15

	BITMAP "...##..."
	BITMAP "...##..."
	BITMAP "........"
	BITMAP ".######."
	BITMAP "#.####.#"
	BITMAP "#.####.#"
	BITMAP "#.####.#"
	BITMAP "#.####.#"

	BITMAP "..####.."
	BITMAP "..#..#.."
	BITMAP "..#..#.."
	BITMAP "..#..#.."
	BITMAP "..#..#.."
	BITMAP "..#..#.."
	BITMAP "..#..#.."
	BITMAP "........"

	REM 16,17
	BITMAP "#......#"
	BITMAP "#..##..#"
	BITMAP "#..##..#"
	BITMAP "#......#"
	BITMAP ".######."
	BITMAP "..####.."
	BITMAP "..####.."
	BITMAP "..####.."

	BITMAP "..####.."
	BITMAP "..####.."
	BITMAP "..#..#.."
	BITMAP ".##..##."
	BITMAP ".#....#."
	BITMAP ".#....#."
	BITMAP "##....##"
	BITMAP "........"


REM 18

	BITMAP "..####.."
	BITMAP ".#######"
	BITMAP ".#######"
	BITMAP ".#######"
	BITMAP ".#######"
	BITMAP "..#####."
	bitmap "...###.."
	BITMAP "....#..."

REM 19

	BITMAP "..####.."
	BITMAP ".#######"
	BITMAP ".##..###"
	BITMAP ".##..###"
	BITMAP ".#######"
	BITMAP "..#####."
	bitmap "...###.."
	BITMAP "....#..."

gramchars20:
REM 20

	BITMAP "..####.."
	BITMAP ".##..###"
	BITMAP ".##...##"
	BITMAP ".##...##"
	BITMAP ".##...##"
	BITMAP "..##.##."
	bitmap "...###.."
	BITMAP "....#..."

REM 21

	BITMAP "..####.."
	BITMAP ".#.....#"
	BITMAP ".#.....#"
	BITMAP ".#..#..#"
	BITMAP ".#.....#"
	BITMAP "..#...#."
	bitmap "...#.#.."
	BITMAP "....#..."

REM 22

	BITMAP "...#.#.."
	BITMAP ".#......"
	BITMAP "#...#..#"
	BITMAP ".#.###.."
	BITMAP "#...#..#"
	BITMAP "..#....."
	bitmap "......#."
	BITMAP "....#..."

REM 23

	BITMAP "........"
	BITMAP "....#..."
	BITMAP "...###.."
	BITMAP "..#####."
	BITMAP "...###.."
	BITMAP "....#..."
	bitmap "........"
	BITMAP "........"

REM 24

	BITMAP "....#..."
	BITMAP "....#..."
	BITMAP "...###.."
	BITMAP ".#######"
	BITMAP "...###.."
	BITMAP "....#..."
	bitmap "....#..."
	BITMAP "........"

REM 25

	BITMAP "........"
	BITMAP "........"
	BITMAP "....#..."
	BITMAP "...###.."
	BITMAP "....#..."
	BITMAP "........"
	bitmap "........"
	BITMAP "........"

	REM 26

	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "....#..."
	BITMAP "........"
	BITMAP "........"
	bitmap "........"
	BITMAP "........"

		REM 27

	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "....#..."
	BITMAP "........"
	BITMAP "........"
	bitmap "........"
	BITMAP "........"

		REM 28,29
	BITMAP "........"
	BITMAP "........"
	BITMAP "#......#"
	BITMAP "#..##..#"
	BITMAP "#..##..#"
	BITMAP "#......#"
	BITMAP ".######."
	BITMAP "..####.."
	BITMAP "..####.."
	BITMAP "..####.."

	BITMAP "..####.."
	BITMAP "..####.."
	BITMAP "..#..#.."
	BITMAP ".##..##."
	BITMAP ".#....#."
	BITMAP ".#....#."

gramchars30:

		REM 30,31
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "#......#"
	BITMAP "#..##..#"
	BITMAP "#..##..#"
	BITMAP "#......#"
	BITMAP ".######."
	BITMAP "..####.."
	BITMAP "..####.."
	BITMAP "..####.."

	BITMAP "..####.."
	BITMAP "..####.."
	BITMAP "..#..#.."
	BITMAP ".##..##."

		REM 32,33
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "#......#"
	BITMAP "#..##..#"
	BITMAP "#..##..#"
	BITMAP "#......#"
	BITMAP ".######."
	BITMAP "..####.."
	BITMAP "..####.."
	BITMAP "..####.."

	BITMAP "..####.."
	BITMAP "..####.."

		REM 34,35
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "#......#"
	BITMAP "#..##..#"
	BITMAP "#..##..#"
	BITMAP "#......#"
	BITMAP ".######."
	BITMAP "..####.."
	BITMAP "..####.."
	BITMAP "..####.."

REM 36,37
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "#......#"
	BITMAP "#..##..#"
	BITMAP "#..##..#"
	BITMAP "#......#"
	BITMAP ".######."

REM number # 38
REM name: generic squash 4

	BITMAP   "........"
	BITMAP   "........"
	BITMAP   "........"
	BITMAP   "........"
	BITMAP   "..###..."
	BITMAP   ".#####.."
	BITMAP   "#######."
	BITMAP   "........"

REM number # 39
REM name: generic squash 5

	BITMAP   "........"
	BITMAP   "........"
	BITMAP   "...#...."
	BITMAP   ".#.#.#.."
	BITMAP   "##...##."
	BITMAP   "........"
	BITMAP   "#######."
	BITMAP   "........"

gramchars40:

REM number # 40
REM name: generic squash 6
	BITMAP   "........"
	BITMAP   "...#...."
	BITMAP   ".#.#.#.."
	BITMAP   "##...##."
	BITMAP   "........"
	BITMAP   ".#####.."
	BITMAP   "#.....#."
	BITMAP   "........"

REM number # 41
REM name: generic squash 7

	BITMAP   "...#...."
	BITMAP   ".#.#..#."
	BITMAP   "##....##"
	BITMAP   ".#.#.#.."
	BITMAP   "..#.#..."
	BITMAP   "#.....#."
	BITMAP   "........"
	BITMAP   "........"

REM number # 42
REM name: generic squash 8

	BITMAP   "#..#..#."
	BITMAP   "#......#"
	BITMAP   "...#...."
	BITMAP   ".#....#."
	BITMAP   "....#..."
	BITMAP   "#.#....#"
	BITMAP   "........"
	BITMAP   "........"

REM number # 43
REM name: generic squash 8

	BITMAP   "........"
	BITMAP   "#..#..#."
	BITMAP   "#......#"
	BITMAP   "........"
	BITMAP   "..#....."
	BITMAP   "......#."
	BITMAP   "#.#....."
	BITMAP   "........"

REM number # 44
REM name: generic squash 9
    BITMAP   "........"
	BITMAP   "........"
	BITMAP   "#..#..#."
	BITMAP   "#......#"
	BITMAP   "........"
	BITMAP   "..#....."
	BITMAP   "......#."
	BITMAP   "#.#....."

REM number # 45
REM name: generic squash 10
    BITMAP   "........"
    BITMAP   "........"
	BITMAP   "........"
	BITMAP   "#..#..#."
	BITMAP   "#......#"
	BITMAP   "........"
	BITMAP   ".##....."
	BITMAP   "......#."

REM number # 46
REM name: generic squash 11
    BITMAP   "........"
    BITMAP   "........"
	BITMAP   "........"
	BITMAP   "........"
	BITMAP   "#..#..#."
	BITMAP   "#......#"
	BITMAP   ".......#"
	BITMAP   ".##....."

REM number # 47
REM name: generic squash 11
    BITMAP   "........"
    BITMAP   "........"
	BITMAP   "........"
	BITMAP   "........"
	BITMAP   "........"
	BITMAP   "........"
	BITMAP   "#..#..#."
	BITMAP   "#.#.#..#"

REM number # 48
REM name: star
	BITMAP   "........"
	BITMAP   "....#..."
	BITMAP   "...###.."
	BITMAP   ".#######"
	BITMAP   "..#####."
	BITMAP   ".##...##"
	BITMAP   ".#.....#"
	BITMAP   "........"


REM number # 49
REM name: bottom line
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"

	BITMAP "........"
	BITMAP "########"
	BITMAP "########"
	BITMAP "........"

Rem Alternate ghost graphics created by dZ
ghostgrams10:
REM 10

	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"

	BITMAP "#......."
	BITMAP ".#......"
	BITMAP "..####.."
	BITMAP "........"

REM 11

	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"

	BITMAP "########"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"



REM 12,13

	BITMAP "..###..."
	BITMAP ".#####.."
	BITMAP ".#####.."
	BITMAP "#######."
	BITMAP "##.#.##."
	BITMAP "##.#.##."
	BITMAP "##.#.##."
	BITMAP "#######."

	BITMAP "#.###.#."
	BITMAP "##...##."
	BITMAP "#######."
	BITMAP "#######."
	BITMAP "#######."
	BITMAP "#.#.##.."
	BITMAP "#.#..#.."
	BITMAP "#....#.."

	REM 14, 15

	BITMAP "..###..."
	BITMAP ".#####.."
	BITMAP ".#.#.#.."
	BITMAP "##.#.##."
	BITMAP "##.#.##."
	BITMAP "#######."
	BITMAP "#######."
	BITMAP "##...##."

	BITMAP "#.###.#."
	BITMAP "#######."
	BITMAP "#######."
	BITMAP "#######."
	BITMAP "#######."
	BITMAP "#.#.##.."
	BITMAP "#.#..#.."
	BITMAP "#....#.."

	REM 16,17
	BITMAP "..###..."
	BITMAP ".#####.."
	BITMAP ".#####.."
	BITMAP "#######."
	BITMAP "#######."
	BITMAP "#######."
	BITMAP "##.#.##."
	BITMAP "##.#.##."

	BITMAP "##.#.##."
	BITMAP "#######."
	BITMAP "#.###.#."
	BITMAP "##...##."
	BITMAP "#######."
	BITMAP "#.#.##.."
	BITMAP "#.#..#.."
	BITMAP "#....#.."


REM 18

	BITMAP ".#######"
	BITMAP ".#######"
	BITMAP ".#######"
	BITMAP ".#######"
	BITMAP ".#######"
	BITMAP ".#######"
	bitmap ".#######"
	BITMAP "........"

REM 19

	BITMAP ".#######"
	BITMAP ".#######"
	BITMAP ".###.###"
	BITMAP ".##...##"
	BITMAP ".###.###"
	BITMAP ".#######"
	bitmap ".#######"
	BITMAP "........"

ghostgrams20:
REM 20

	BITMAP ".#######"
	BITMAP ".###.###"
	BITMAP ".##...##"
	BITMAP ".#.....#"
	BITMAP ".##...##"
	BITMAP ".###.###"
	bitmap ".#######"
	BITMAP "........"

REM 21

	BITMAP ".###.###"
	BITMAP ".##...##"
	BITMAP "........"
	BITMAP "....#..."
	BITMAP "........"
	BITMAP ".#.....#"
	bitmap ".###.###"
	BITMAP "........"

REM 22

	BITMAP ".#.....#"
	BITMAP "........"
	BITMAP "....#..."
	BITMAP "...###.."
	BITMAP "....#..."
	BITMAP "........"
	bitmap ".#.....#"
	BITMAP "........"

REM 23

	BITMAP "........"
	BITMAP "....#..."
	BITMAP "...###.."
	BITMAP "..##.##."
	BITMAP "...###.."
	BITMAP "....#..."
	bitmap "........"
	BITMAP ".#.....#"

REM 24

	BITMAP "....#..."
	BITMAP "....#..."
	BITMAP "...#.#.."
	BITMAP ".##...##"
	BITMAP "...#.#.."
	BITMAP "....#..."
	bitmap "....#..."
	BITMAP "........"

REM 25

	BITMAP "....#..."
	BITMAP "..#.#.#."
	BITMAP "........"
	BITMAP ".##...##"
	BITMAP "........"
	BITMAP "..#.#.#."
	bitmap "....#..."
	BITMAP "........"

REM 26

	BITMAP "....#..."
	BITMAP "........"
	BITMAP "........"
	BITMAP "##.....#"
	BITMAP "........"
	BITMAP "........"
	bitmap "....#..."
	BITMAP "....#..."

REM 27

	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "#......."
	BITMAP "........"
	BITMAP "........"
	bitmap "........"
	BITMAP "....#..."

REM 28,29
	BITMAP "........"
	BITMAP "........"
	BITMAP "..###..."
	BITMAP ".#####.."
	BITMAP ".#####.."
	BITMAP "#######."
	BITMAP "#.###.#."
	BITMAP "##.#.##."
	BITMAP "##.#.##."
	BITMAP "#.###.#."

	BITMAP "#######."
	BITMAP "#######."
	BITMAP "#######."
	BITMAP "#######."
	BITMAP "#######."
	BITMAP "#.#.##.."
ghostgrams30:

		REM 30,31
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "..###..."
	BITMAP ".#####.."
	BITMAP ".#####.."
	BITMAP "#######."
	BITMAP "#.###.#."
	BITMAP "##.#.##."
	BITMAP "##.#.##."
	BITMAP "#.###.#."

	BITMAP "#######."
	BITMAP "#######."
	BITMAP "#######."
	BITMAP "#######."

		REM 32,33
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "..###..."
	BITMAP ".#####.."
	BITMAP ".#####.."
	BITMAP "#######."
	BITMAP "#.###.#."
	BITMAP "##.#.##."
	BITMAP "##.#.##."
	BITMAP "#.###.#."

	BITMAP "#######."
	BITMAP "#######."

		REM 34,35
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "..###..."
	BITMAP ".#####.."
	BITMAP ".#####.."
	BITMAP "#######."
	BITMAP "#.###.#."
	BITMAP "##.#.##."
	BITMAP "##.#.##."
	BITMAP "#.###.#."

REM 36,37
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "..###..."
	BITMAP ".#####.."
	BITMAP ".#####.."
	BITMAP "#######."
	BITMAP "#.###.#."

REM number # 38
REM name: generic squash 4

	BITMAP   "........"
	BITMAP   "........"
	BITMAP   "........"
	BITMAP   "........"
	BITMAP   "..###..."
	BITMAP   ".#####.."
	BITMAP   "#######."
	BITMAP   "........"

REM number # 39
REM name: generic squash 5

	BITMAP   "........"
	BITMAP   "........"
	BITMAP   "...#...."
	BITMAP   ".#.#.#.."
	BITMAP   "##...##."
	BITMAP   "........"
	BITMAP   "#######."
	BITMAP   "........"


fushek_balloon:	' gram 18

	BITMAP "..####.."
	BITMAP ".#######"
	BITMAP ".#######"
	BITMAP ".#######"
	BITMAP ".#######"
	BITMAP "..#####."
	bitmap "........"
	BITMAP "........"


