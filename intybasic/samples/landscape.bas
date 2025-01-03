        REM
        REM Horizontal scrolling sample using big image (landscape.bmp)
        REM Demo for IntyBASIC
        REM by Oscar Toledo G. http://nanochess.org/
        REM Aug/31/2015
        REM

	INCLUDE "constants.bas"

	CONST LANDSCAPE_WIDTH = 60
	CONST SPEED = 2    ' Can be 1, 2 or 4

        BORDER BORDER_BLACK, 1
	MODE 1
	WAIT
        DEFINE DEF00,16,screen_bitmaps_0
	WAIT
        DEFINE DEF16,16,screen_bitmaps_1
	WAIT
        DEFINE DEF32,16,screen_bitmaps_2
	WAIT
        DEFINE DEF48,6,screen_bitmaps_3
	WAIT
	DEFINE DEF63,1,spaceship
	WAIT

        SCREEN screen_cards, 0, 0, BACKGROUND_COLUMNS, BACKGROUND_ROWS, LANDSCAPE_WIDTH
        next_column = 20
        offset_x = 0
        offset_y = 0
        offset_d = 0
	x_spaceship = 32
	y_spaceship = 32
        WAIT

	' Main loop
main_loop:
	' Pixel scrolling, check if it should displace full screen (move all cards left)
        IF offset_x = 0 THEN
                offset_d = 2:offset_x = 8 - SPEED
        ELSE
                offset_x = offset_x - SPEED
        END IF
	' Update spaceship sprite (does it after calculating new offset_x)
        SPRITE 0, x_spaceship + VISIBLE + ZOOMX2 - offset_x, y_spaceship + ZOOMY2 - offset_y, SPR63 + SPR_WHITE
	' Setup scrolling for next frame
        SCROLL offset_x, offset_y, offset_d
        WAIT
	' Now is time to update next column for screen if all cards were moved
        offset_d = 0
        IF offset_x = 8 - SPEED THEN
                SCREEN screen_cards, next_column, 19, 1, BACKGROUND_ROWS, LANDSCAPE_WIDTH
                next_column = next_column + 1
                IF next_column = LANDSCAPE_WIDTH THEN next_column = 0
        END IF
	' Displace spaceship 
	IF cont.up THEN IF y_spaceship > 8 THEN y_spaceship = y_spaceship - 1
	IF cont.down THEN IF y_spaceship < 80 THEN y_spaceship = y_spaceship + 1
	IF cont.left THEN IF x_spaceship > 8 THEN x_spaceship = x_spaceship - 1
	IF cont.right THEN IF x_spaceship < 120 THEN x_spaceship = x_spaceship + 1
        GOTO main_loop

	' Spaceship
spaceship:
	BITMAP "        "
	BITMAP "*       "
	BITMAP "**      "
	BITMAP " *** *  "
	BITMAP "********"
	BITMAP "  ***   "
	BITMAP " **     "
	BITMAP "        "
	
	' 54 bitmaps (created with intycolor -b landscape.bmp output.bas)
screen_bitmaps_0:
	DATA $0000,$F678,$F9FB,$3E7D
	DATA $0000,$0D0D,$FD0D,$FDFD
	DATA $0000,$F8F0,$FFFD,$FEFF
	DATA $0000,$BF3F,$BFBF,$7E3F
	DATA $000E,$0001,$0000,$0000
	DATA $007D,$FFFF,$FFFF,$C01F
	DATA $43F8,$BFBF,$BEBE,$0EA0
	DATA $F8FC,$C0E0,$0000,$0000
	DATA $FBFB,$FBFB,$E118,$FFFF
	DATA $FEFE,$E0FA,$DF1E,$DFDF
	DATA $0000,$0000,$0000,$8000
	DATA $0000,$F800,$FFFF,$FFFF
	DATA $0000,$0000,$FFF0,$FFFF
	DATA $0000,$0000,$0000,$FFF0
	DATA $0000,$0000,$0000,$FF00
	DATA $0000,$0000,$0000,$FE00
screen_bitmaps_1:
	DATA $0000,$0000,$0701,$7F3F
	DATA $0000,$0000,$FCC0,$FFFF
	DATA $0000,$0000,$0000,$FFFF
	DATA $0000,$0000,$0000,$FCC0
	DATA $0001,$0303,$0F07,$0F0F
	DATA $3FFF,$FD80,$F9FD,$FBFB
	DATA $DFDF,$FF00,$FFFF,$FFFF
	DATA $0080,$E060,$F0E0,$F0F0
	DATA $0000,$0000,$0100,$0F03
	DATA $0000,$0F07,$FF3F,$FFFF
	DATA $0000,$F000,$FFFF,$FFFF
	DATA $0000,$0000,$FFF8,$FFFF
	DATA $0000,$0000,$FC00,$FFFF
	DATA $0000,$0000,$0000,$FFE0
	DATA $0000,$0000,$0000,$0300
	DATA $0000,$0000,$0303,$FF7F
screen_bitmaps_2:
	DATA $0300,$FF1F,$FFFF,$FFFF
	DATA $FF00,$FFFF,$FFFF,$FFFF
	DATA $FC00,$FFFF,$FFFF,$FFFF
	DATA $0000,$FEE0,$FFFF,$FFFF
	DATA $0000,$0000,$C080,$FFFF
	DATA $0000,$0000,$0000,$E000
	DATA $0000,$0000,$0000,$FF3F
	DATA $0000,$0000,$7F1F,$FFFF
	DATA $0000,$3F00,$FFFF,$FFFF
	DATA $0000,$0000,$FFFF,$FFFF
	DATA $0000,$0000,$FEE0,$FFFF
	DATA $0000,$0000,$0000,$FF07
	DATA $0000,$0100,$7F1F,$FFFF
	DATA $0000,$FF3F,$FFFF,$FFFF
	DATA $F0C0,$FFFF,$FFFF,$FFFF
	DATA $0000,$FFFF,$FFFF,$FFFF
screen_bitmaps_3:
	DATA $FF01,$FFFF,$FFFF,$FFFF
	DATA $FFF0,$FFFF,$FFFF,$FFFF
	DATA $FF03,$FFFF,$FFFF,$FFFF
	DATA $FFFC,$FFFF,$FFFF,$FFFF
	DATA $0300,$FFFF,$FFFF,$FFFF
	DATA $FF7F,$FFFF,$FFFF,$FFFF

	REM 60x12 cards
screen_cards:
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0803,$080B,$0813,$081B,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0823,$082B,$0833,$083B,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0843,$084B,$0853,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	DATA $085D,$0865,$086D,$0875,$087D,$0000,$0000,$0000,$0000,$0000,$0000,$0885,$085D,$088D,$0895,$089D,$0000,$0000,$08A3,$08AB,$08B3,$08BB,$0000,$08C5,$08CD,$08D5,$08DD,$08E5,$08ED,$0875,$0875,$0000,$0000,$0000,$0000,$08F5,$08FD,$0905,$090D,$0915,$091D,$0925,$092D,$0000,$0000,$0000,$0000,$08F5,$0935,$093D,$0945,$08D5,$094D,$0955,$08ED,$0855,$0000,$095D,$0965,$096D
	DATA $2200,$2200,$2200,$2200,$2200,$2200,$0975,$097D,$097D,$0985,$2200,$2200,$2200,$2200,$2200,$2200,$098D,$090D,$090D,$090D,$090D,$090D,$0995,$2200,$2200,$2200,$2200,$2200,$2200,$2200,$2200,$2200,$2200,$2200,$2200,$2200,$2200,$2200,$2200,$2200,$2200,$2200,$099D,$090D,$097D,$097D,$09A5,$09AD,$2200,$2200,$2200,$2200,$2200,$2200,$2200,$2200,$2200,$2200,$2200,$2200
