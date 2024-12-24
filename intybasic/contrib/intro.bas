'IntyBASIC - Introduction to the Intellivision
'Version 1,0 - July 12, 2015
'By Tarzilla

Include "constants.bas""
MODE SCREEN_CS,1,2,7,4

Rem WAIT is required after a MODE
WAIT
cls
Rem Define first 2 cards of 64 for Inty-B, using BITMAPS from Inty1
DEFINE DEF00,2,Inty1

Rem WAIT is required after a DEFINE or DEFINE Alternate
WAIT

Rem Setup an array to hold the 16 colors, used to randomly pick from a color without IFs
Rem defined with # as half the color values are 16 bit numbers ($1001)
Dim #Color_Array (16)

Rem Read values from the label Color_Values into the #Color_Array
Restore Color_Values

Rem Initialize the Music Player for 2 channels
Play Simple

Rem Setup arrays for holding 8 sprites x,y locations and color
Dim Spritex (8)
Dim Spritey (8)
Dim #SpriteC (8)

Rem only using first sprite for now
Spritex(0)=70
Spritey(0)=50
#SpriteC(0)=6

Rem Various parts are broken up into PROCEDURES
Gosub StartScreen
Gosub wave
Gosub showEight
Gosub showSize
Gosub ShowTwoCards
Gosub showmusic
Gosub MyWorld
Gosub ShowPriority
Gosub IntyBasic

Rem Everything is done, loop forever, prevents falling into any code below
loop:
GOTO loop

Rem Procedures can be anywhere in the code and can be called in any order
StartScreen:Procedure
	PLAY Tune_1
	Print at SCREENPOS(4, 1) color CS_WHITE, "Welcome to an"
	Print at SCREENPOS(2, 2), "Introduction to"
	Print at SCREENPOS(1, 3), "the Intellivision"
	Print at SCREENPOS(3, 5) color CS_TAN, "When you see a"
	Print at SCREENPOS(3, 6), "flashing arrow,"
	Print at SCREENPOS(2, 7), "press a button or"
	Print at SCREENPOS(4, 8), "disc right to"
	Print at SCREENPOS(5, 9), "continue."
	Gosub Pause
	PLAY OFF
	Cls
End

Wave:Procedure
	Print at SCREENPOS(3, 0) color CS_WHITE, "Hi, I'm Inty-B!"
	Print at SCREENPOS(5, 1), "I'm a MOB!"
	Spriteframe=0
	For cnt1=0 to 60
		If CNT1=10 then Print at SCREENPOS(0, 2), "That means:         Movable Object..."
		If CNT1=25 then Print at SCREENPOS(0, 6) , "Most people call me a Sprite!"
		If CNT1=40 then gosub pause: cls: Print at SCREENPOS(0, 1) color CS_WHITE, "I live in a world   that is 160 pixels  wide by 96 pixels   high!"

		Sprite 0,Spritex(0)+VISIBLE,Spritey(0) , SPR00+ (8*spriteframe) +#SpriteC(0)
		gosub waitabit
		Spriteframe=Spriteframe+1:If SpriteFrame>1 then SpriteFrame=0
	next cnt1
	Gosub Pause
End

ShowEight:Procedure
	cls
	DEFINE DEF00,2,Inty2
	WAIT

	Print at SCREENPOS(0, 0) color CS_WHITE, " There are 8 of me!"
	Spriteframe=0
	For cnt1=0 to 50
		Spritex(0)=Spritex(0)-1
		Sprite 0,Spritex(0)+15+VISIBLE,Spritey(0) , SPR00+ (8*spriteframe) +#SpriteC(0)
		Sprite 1,Spritex(0)+30+VISIBLE,100-cnt1, SPR00+ (8*spriteframe) +#SpriteC(0)
		Sprite 2,Spritex(0)+45+VISIBLE,cnt1 , SPR00+ (8*spriteframe) +#SpriteC(0)
		Sprite 3,Spritex(0)+60+VISIBLE,100-cnt1 , SPR00+ (8*spriteframe) +#SpriteC(0)
		Sprite 4,Spritex(0)+75+VISIBLE,cnt1, SPR00+ (8*spriteframe) +#SpriteC(0)
		Sprite 5,Spritex(0)+90+VISIBLE,100-cnt1 , SPR00+ (8*spriteframe) +#SpriteC(0)
		Sprite 6,Spritex(0)+105+VISIBLE,cnt1 , SPR00+ (8*spriteframe) +#SpriteC(0)
		Sprite 7,Spritex(0)+120+VISIBLE,100-cnt1, SPR00+ (8*spriteframe) +#SpriteC(0)
		Wait
		Wait
		Spriteframe=Spriteframe+1:If SpriteFrame>1 then SpriteFrame=0
	next cnt1
	Gosub WaitaLot

	Print at SCREENPOS(0, 0), " We are all a solid  color, but we can   pick from 16        different ones:"
	DEFINE DEF00,2,Inty1
	wait
	Spriteframe=0
	#SpriteColor=0
	For cnt1=0 to 15
		Sprite 0,Spritex(0)+15+VISIBLE,Spritey(0) , SPR00+ (8*spriteframe) +#SpriteC(0)
		Sprite 1,Spritex(0)+30+VISIBLE,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(0)
		Sprite 2,Spritex(0)+45+VISIBLE,Spritey(0) , SPR00+ (8*spriteframe) +#SpriteC(0)
		Sprite 3,Spritex(0)+60+VISIBLE,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(0)
		Sprite 4,Spritex(0)+75+VISIBLE,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(0)
		Sprite 5,Spritex(0)+90+VISIBLE,Spritey(0) , SPR00+ (8*spriteframe) +#SpriteC(0)
		Sprite 6,Spritex(0)+105+VISIBLE,Spritey(0) , SPR00+ (8*spriteframe) +#SpriteC(0)
		Sprite 7,Spritex(0)+120+VISIBLE,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(0)

		gosub waitabit
		gosub waitabit
		Spriteframe=Spriteframe+1:If SpriteFrame>1 then SpriteFrame=0
		#SpriteColor=#SpriteColor+1:If #SpriteColor>15 then #SpriteColor=0
		#SpriteC(0)=Color_Values(#SpriteColor)
	next cnt1
	#SpriteC(0)=6
	SpriteFrame=0
	#SpriteC(1)=SPR_BLACK
	#SpriteC(2)=SPR_GREEN
	#SpriteC(3)=SPR_GREY
	#SpriteC(4)=SPR_ORANGE
	#SpriteC(5)=SPR_RED
	#SpriteC(6)=SPR_WHITE
	#SpriteC(7)=SPR_PINK

	Sprite 0,Spritex(0)+15+VISIBLE,Spritey(0) , SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 1,Spritex(0)+30+VISIBLE,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(1)
	Sprite 2,Spritex(0)+45+VISIBLE,Spritey(0) , SPR00+ (8*spriteframe) +#SpriteC(2)
	Sprite 3,Spritex(0)+60+VISIBLE,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(3)
	Sprite 4,Spritex(0)+75+VISIBLE,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(4)
	Sprite 5,Spritex(0)+90+VISIBLE,Spritey(0) , SPR00+ (8*spriteframe) +#SpriteC(5)
	Sprite 6,Spritex(0)+105+VISIBLE,Spritey(0) , SPR00+ (8*spriteframe) +#SpriteC(6)
	Sprite 7,Spritex(0)+120+VISIBLE,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(7)
	Gosub Pause
End

ShowMusic: Procedure
	Cls
	Print at SCREENPOS(0, 0) color CS_WHITE, "It's awfully quiet!"
	Gosub Pause
	Play Tune_1
	Gosub WaitaLittle
	Print at SCREENPOS(0, 1) color CS_WHITE, "That's better!"
	Gosub Pause
End

ShowSize:Procedure
	cls
	DEFINE DEF00,2,Inty2
	WAIT

	Print at SCREENPOS(0, 0) color CS_WHITE, " Normally we are 8  pixels by 8 pixels."
	Spriteframe=0
	For cnt1=0 to 15
		SpriteY(0)=50-cnt1
		Sprite 0,Spritex(0)+15+VISIBLE,Spritey(0) , SPR00+ (8*spriteframe) +#SpriteC(0)
		Sprite 1,Spritex(0)+30+VISIBLE,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(1)
		Sprite 2,Spritex(0)+45+VISIBLE,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(2)
		Sprite 3,Spritex(0)+60+VISIBLE,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(3)
		Sprite 4,Spritex(0)+75+VISIBLE,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(4)
		Sprite 5,Spritex(0)+90+VISIBLE,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(5)
		Sprite 6,Spritex(0)+105+VISIBLE,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(6)
		Sprite 7,Spritex(0)+120+VISIBLE,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(7)
		gosub waitalittle
		Spriteframe=Spriteframe+1:If SpriteFrame>1 then SpriteFrame=0
	next cnt1

	Print at SCREENPOS(6,  4), "01111000"
	Print at SCREENPOS(6,  5), "11010100"
	Print at SCREENPOS(6,  6), "01111000"
	Print at SCREENPOS(6,  7), "00101000"
	Print at SCREENPOS(6,  8), "01111110"
	Print at SCREENPOS(6,  9), "10111001"
	Print at SCREENPOS(6, 10), "01101000"
	Print at SCREENPOS(6, 11), "00001100"

	Gosub Pause
	Cls
	Print at SCREENPOS(0, 0) color CS_WHITE, "We can double our   width:"

	Sprite 0,Spritex(0)+15+VISIBLE,Spritey(0) , SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 1,Spritex(0)+30+VISIBLE+ZOOMX2,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 2,Spritex(0)+45+VISIBLE+ZOOMX2,Spritey(0) , SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 3,Spritex(0)+60+VISIBLE+ZOOMX2,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 4,Spritex(0)+75+VISIBLE+ZOOMX2,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 5,Spritex(0)+90+VISIBLE+ZOOMX2,Spritey(0) , SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 6,Spritex(0)+105+VISIBLE+ZOOMX2,Spritey(0) , SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 7,Spritex(0)+120+VISIBLE+ZOOMX2,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(0)

	Gosub Pause
	Gosub WaitaLittle
	Print at SCREENPOS(0, 0) Color CS_WHITE, "We can double our   height:  "

	Sprite 0,Spritex(0)+15+VISIBLE,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 1,Spritex(0)+30+VISIBLE,Spritey(0)+ZOOMY2, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 2,Spritex(0)+45+VISIBLE,Spritey(0) +ZOOMY2, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 3,Spritex(0)+60+VISIBLE,Spritey(0)+ZOOMY2, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 4,Spritex(0)+75+VISIBLE,Spritey(0)+ZOOMY2, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 5,Spritex(0)+90+VISIBLE,Spritey(0) +ZOOMY2, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 6,Spritex(0)+105+VISIBLE,Spritey(0) +ZOOMY2, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 7,Spritex(0)+120+VISIBLE,Spritey(0)+ZOOMY2, SPR00+ (8*spriteframe) +#SpriteC(0)
	Gosub Pause
	Gosub WaitaLittle
	Print at SCREENPOS(0, 0) color CS_WHITE, "We can be 4 times   our height:         "

	Sprite 0,Spritex(0)+15+VISIBLE,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 1,Spritex(0)+30+VISIBLE,Spritey(0)+ZOOMY4, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 2,Spritex(0)+45+VISIBLE,Spritey(0) +ZOOMY4, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 3,Spritex(0)+60+VISIBLE,Spritey(0)+ZOOMY4, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 4,Spritex(0)+75+VISIBLE,Spritey(0)+ZOOMY4, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 5,Spritex(0)+90+VISIBLE,Spritey(0) +ZOOMY4, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 6,Spritex(0)+105+VISIBLE,Spritey(0) +ZOOMY4, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 7,Spritex(0)+120+VISIBLE,Spritey(0)+ZOOMY4, SPR00+ (8*spriteframe) +#SpriteC(0)
	Gosub Pause
	Print at SCREENPOS(0, 0) color CS_WHITE, "Or 8 times our      height:            "

	Sprite 0,Spritex(0)+15+VISIBLE,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 1,Spritex(0)+30+VISIBLE,Spritey(0)+ZOOMY4, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 2,Spritex(0)+45+VISIBLE,Spritey(0) +ZOOMY8, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 3,Spritex(0)+60+VISIBLE,Spritey(0)+ZOOMY8, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 4,Spritex(0)+75+VISIBLE,Spritey(0)+ZOOMY8, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 5,Spritex(0)+90+VISIBLE,Spritey(0) +ZOOMY8, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 6,Spritex(0)+105+VISIBLE,Spritey(0) +ZOOMY8, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 7,Spritex(0)+120+VISIBLE,Spritey(0)+ZOOMY8, SPR00+ (8*spriteframe) +#SpriteC(0)
	Gosub Pause
	Gosub WaitaLittle
	Print at SCREENPOS(0, 0) color CS_WHITE, "We can mix and matchsizes:                 "

	Sprite 0,Spritex(0)+15+VISIBLE,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 1,Spritex(0)+30+VISIBLE,Spritey(0)+ZOOMY4, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 2,Spritex(0)+45+VISIBLE,Spritey(0) +ZOOMY8, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 3,Spritex(0)+60+VISIBLE+ZOOMX2,Spritey(0)+ZOOMY2, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 4,Spritex(0)+75+VISIBLE+ZOOMX2,Spritey(0)+ZOOMY4, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 5,Spritex(0)+90+VISIBLE+ZOOMX2,Spritey(0) +ZOOMY8, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 6,Spritex(0)+105+VISIBLE,Spritey(0) +ZOOMY8, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 7,Spritex(0)+120+VISIBLE+ZOOMX2,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(0)
	Gosub Pause
	Gosub WaitaLittle
	Print at SCREENPOS(0, 0) color CS_WHITE, "We can flip         horizontally:                 "

	Sprite 0,Spritex(0)+15+VISIBLE,Spritey(0)+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 1,Spritex(0)+30+VISIBLE,Spritey(0)+ZOOMY4+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 2,Spritex(0)+45+VISIBLE,Spritey(0) +ZOOMY8+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 3,Spritex(0)+60+VISIBLE+ZOOMX2,Spritey(0)+ZOOMY2+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 4,Spritex(0)+75+VISIBLE+ZOOMX2,Spritey(0)+ZOOMY4+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 5,Spritex(0)+90+VISIBLE+ZOOMX2,Spritey(0) +ZOOMY8+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 6,Spritex(0)+105+VISIBLE,Spritey(0) +ZOOMY8+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 7,Spritex(0)+120+VISIBLE+ZOOMX2,Spritey(0)+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)
	Gosub Pause
	Gosub WaitaLittle
	Print at SCREENPOS(0, 0) color CS_WHITE, "And vertically:                   "
	Sprite 0,Spritex(0)+15+VISIBLE,Spritey(0)+FlipX+FlipY, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 1,Spritex(0)+30+VISIBLE,Spritey(0)+ZOOMY4+FlipX+FlipY, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 2,Spritex(0)+45+VISIBLE,Spritey(0) +ZOOMY8+FlipX+FlipY, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 3,Spritex(0)+60+VISIBLE+ZOOMX2,Spritey(0)+ZOOMY2+FlipX+FlipY, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 4,Spritex(0)+75+VISIBLE+ZOOMX2,Spritey(0)+ZOOMY4+FlipX+FlipY, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 5,Spritex(0)+90+VISIBLE+ZOOMX2,Spritey(0) +ZOOMY8+FlipX+FlipY, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 6,Spritex(0)+105+VISIBLE,Spritey(0) +ZOOMY8+FlipX+FlipY, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 7,Spritex(0)+120+VISIBLE+ZOOMX2,Spritey(0)+FlipX+FlipY, SPR00+ (8*spriteframe) +#SpriteC(0)
	Gosub Pause
	Gosub WaitaLittle
	Print at SCREENPOS(0, 0) Color CS_WHITE, "We can be visible...                  "
	Sprite 0,Spritex(0)+15+VISIBLE,Spritey(0)+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 1,Spritex(0)+30,Spritey(0)+ZOOMY4+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 2,Spritex(0)+45+VISIBLE,Spritey(0) +ZOOMY8+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 3,Spritex(0)+60+VISIBLE+ZOOMX2,Spritey(0)+ZOOMY2+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 4,Spritex(0)+75+VISIBLE+ZOOMX2,Spritey(0)+ZOOMY4+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 5,Spritex(0)+90+VISIBLE+ZOOMX2,Spritey(0) +ZOOMY8+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 6,Spritex(0)+105+VISIBLE,Spritey(0) +ZOOMY8+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 7,Spritex(0)+120+VISIBLE+ZOOMX2,Spritey(0)+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)

	'gosub WaitaLot
	gosub WaitaLot
	Print at SCREENPOS(0, 9) Color CS_WHITE, "   Or not!"

	For Cnt2=0 to 3
		for cnt1=0 to 10
			'Sprite 0,Spritex(0)+15+VISIBLE,Spritey(0)+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)
			'Sprite 1,Spritex(0)+30,Spritey(0)+ZOOMY4+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)
			'Sprite 2,Spritex(0)+45+VISIBLE,Spritey(0) +ZOOMY8+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)
			if (cnt1 > 5 ) then Sprite 3,Spritex(0)+60+ZOOMX2+VISIBLE,Spritey(0)+ZOOMY2+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0) ELSE Sprite 3,Spritex(0)+60+ZOOMX2,Spritey(0)+ZOOMY2+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)

			'Sprite 4,Spritex(0)+75+VISIBLE+ZOOMX2,Spritey(0)+ZOOMY4+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)
			if (cnt1 <5 ) then Sprite 5,Spritex(0)+90+ZOOMX2,Spritey(0) +ZOOMY8+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0) ELSE Sprite 5,Spritex(0)+90+VISIBLE+ZOOMX2,Spritey(0) +ZOOMY8+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)
			'Sprite 6,Spritex(0)+105+VISIBLE,Spritey(0) +ZOOMY8+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)
			'Sprite 7,Spritex(0)+120+VISIBLE+ZOOMX2,Spritey(0)+FlipX, SPR00+ (8*spriteframe) +#SpriteC(0)
			gosub WaitaLittle
		next cnt1
	Next Cnt2
	Gosub Pause
End

ShowTwoCards: Procedure
	cls
	Define DEF00,6,Inty2
	Wait
	SpriteFrame=2
	Print at SCREENPOS(0, 0) color CS_WHITE, "Another trick is we can be 8x16:"
	Sprite 0,Spritex(0)+15+VISIBLE,Spritey(0)+DOUBLEY, SPR01+ (8*spriteframe) +#SpriteC(0)
	Sprite 1,Spritex(0)+30+VISIBLE,Spritey(0)+ZOOMY4+DOUBLEY, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 2,Spritex(0)+45+VISIBLE,Spritey(0) +ZOOMY8+ DOUBLEY,SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 3,Spritex(0)+60+VISIBLE+ZOOMX2,Spritey(0)+ZOOMY2+ DOUBLEY,SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 4,Spritex(0)+75+VISIBLE+ZOOMX2,Spritey(0)+ZOOMY4+ DOUBLEY,SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 5,Spritex(0)+90+VISIBLE+ZOOMX2,Spritey(0) +ZOOMY8+ DOUBLEY,SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 6,Spritex(0)+105+VISIBLE,Spritey(0) +ZOOMY8+ DOUBLEY,SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 7,Spritex(0)+120+VISIBLE+ZOOMX2,Spritey(0)+ DOUBLEY,SPR00+ (8*spriteframe) +#SpriteC(0)

	Gosub Pause
End

MyWorld: Procedure
	Spritey(0)=60
	SpriteX(0)=SpriteX(0)-2
	Sprite 0,Spritex(0)+15+VISIBLE,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 1,Spritex(0)+30,Spritey(0)+ZOOMY4+DOUBLEY, SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 2,Spritex(0)+45,Spritey(0) +ZOOMY8+ DOUBLEY,SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 3,Spritex(0)+60+ZOOMX2,Spritey(0)+ZOOMY2+ DOUBLEY,SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 4,Spritex(0)+75+ZOOMX2,Spritey(0)+ZOOMY4+ DOUBLEY,SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 5,Spritex(0)+90+ZOOMX2,Spritey(0) +ZOOMY8+ DOUBLEY,SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 6,Spritex(0)+105,Spritey(0) +ZOOMY8+ DOUBLEY,SPR00+ (8*spriteframe) +#SpriteC(0)
	Sprite 7,Spritex(0)+120+ZOOMX2,Spritey(0)+ DOUBLEY,SPR00+ (8*spriteframe) +#SpriteC(0)

	Cls
	Print at SCREENPOS(0, 0) Color CS_WHITE, "It would be boring  if it was just us,  but we also have a  background to live  in. It is officiallycalled the BACKTAB."
	Gosub Pause

	Print at SCREENPOS(0, 8) color CS_WHITE, "It is made up of a  grid of 'cards' 20  across by 12 down."
	Gosub Pause
	Cls
	Wait
Rem print 12 rows of cards in different colors
	For Cnt1=0 to 11
		Print  color Cnt1, "\260\261\260\261\260\261\260\261\260\261\260\261\260\261\260\261\260\261\260\261"
		Gosub WaitaBit
	Next Cnt1

	Print at (SCREENPOS(3, 6)), 8

	Gosub WaitaBit
	Gosub WaitaBit
	Gosub WaitaBit
	Gosub Pause
	Cls
	wait
	Print at SCREENPOS(0, 7) color CS_WHITE, "There are a bunch ofbuilt in cards, theyare stored in the   GROM."
	Print color CS_YELLOW, " (Graphics ROM)"
	Gosub Pause
Rem Print GROM cards
	'For cnt1=SCREENPOS(0, 0) to SCREENPOS(11, 19)
	For cnt1=0 to 239
		Print at cnt1, cnt1*8
		'If Cnt1>SCREENPOS(10, 5) then Spriteframe=Spriteframe+1:SpriteY(0)=SpriteY(0)+1:Sprite 0,Spritex(0)+15+VISIBLE,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(0)
		If Cnt1>210 then Spriteframe=Spriteframe+1:SpriteY(0)=SpriteY(0)+1:Sprite 0,Spritex(0)+15+VISIBLE,Spritey(0), SPR00+ (8*spriteframe) +#SpriteC(0)
		If SpriteFrame>1 then SpriteFrame=0
		Wait
	next cnt1
	Gosub Pause
	Cls
	Wait

	Print Color CS_WHITE, "We can also define  64 custom cards that are stored in GRAM"
	Print Color CS_YELLOW, " (Graphics RAM)."
	Gosub Pause
	cls
	Print at SCREENPOS(0,  0) color CS_White, "One thing to note isthat"
	Print at SCREENPOS(0,  1) color CS_WHITE,"background cards are"
	Print at SCREENPOS(0,  2) color CS_WHITE,"double height"
	Print at SCREENPOS(0,  3) color CS_WHITE, "compared to me, eventhough they are"
	Print at SCREENPOS(0,  5) color CS_WHITE, "defined as 8x8."
	Print at SCREENPOS(5, 10) color CS_YELLOW, "\256\257\258\259\260\261"

	Gosub Pause
	DEFINE DEF06,3,screen_bitmaps_Old
	Define Alternate DEF60, 1, Knight
	Wait
Rem Show some of the custom GRAM cards
	Print at SCREENPOS(5, 10) color CS_YELLOW, "\256\257\258\259\260\261\276\277\278\316"

	WAIT
	Print at SCREENPOS(0, 0) color CS_WHITE, "Now I'll build a    screen and show you that my Priority canbe in front of or   BEHIND the     "
	Print at SCREENPOS(0, 5) color CS_WHITE, "background...  "
	Gosub Pause
	End

Rem We are filling the first memory slot, move the following code to the next one
ASM ORG $D000

ShowPriority: Procedure
	Play OFF
	Gosub ShowFence
	SpriteDelay=0
Rem Inty-B
	SpriteY(0)=64
	SpriteX(0)=0
	Spriteframe=0

Rem Flag Up
	SpriteX(2)=128
	SpriteY(2)=57
	#SpriteC(2)=SPR_RED

Rem Door
	SpriteX(3)=102
	SpriteY(3)=70
	#SpriteC(3)=SPR_ORANGE

Rem Box (left right halves)
	SpriteX(4)=118
	SpriteY(4)=63
	#SpriteC(4)=SPR_ORANGE

	SpriteX(5)=134
	SpriteY(5)=63
	#SpriteC(5)=SPR_ORANGE

Rem Post
	SpriteX(6)=125
	SpriteY(6)=67
	#SpriteC(6)=SPR_Brown

	Sprite 0,Spritex(2)+VISIBLE+ZOOMX2,Spritey(2) +ZOOMY2, SPR24 +#SpriteC(2)
	Sprite 1,Spritex(3)+ZOOMX2,Spritey(3) +ZOOMY2, SPR28 +#SpriteC(3)
	Sprite 2,Spritex(4)+VISIBLE+ZOOMX2,Spritey(4) +ZOOMY2, SPR26 +#SpriteC(4)
	Sprite 3,Spritex(5)+VISIBLE+ZOOMX2,Spritey(5) +ZOOMY2, SPR27 +#SpriteC(5)
	Sprite 4,Spritex(6)+VISIBLE+ZOOMX2,Spritey(6) +ZOOMY4, SPR29 +#SpriteC(6)
	Sprite 5,Spritex(4)+4+VISIBLE+ZOOMX2,Spritey(4) +ZOOMY2+1, SPR30 +SPR_BLACK


	For cnt1=Spritex(0) to 100
		Spritex(0)=Spritex(0)+1
		If SpriteX(0)<80 then Sprite 7,Spritex(0)+VISIBLE+ZOOMX2,Spritey(0) +ZOOMY4, SPR00+ (8*spriteframe) +#SpriteC(0)+BEHIND
		If SpriteX(0)>79 then Sprite 7,Spritex(0)+VISIBLE+ZOOMX2,Spritey(0)+ZOOMY4+2, SPR00+ (8*spriteframe) +#SpriteC(0)

		Wait
		Wait
		SpriteDelay=SpriteDelay+1: If SpriteDelay>2 then SpriteDelay=0:Spriteframe=Spriteframe+1:If SpriteFrame>1 then SpriteFrame=0
	next cnt1

Rem Open Mailbox Door
	Sprite 1,Spritex(3)+VISIBLE+ZOOMX2,Spritey(3) +ZOOMY2, SPR28 +#SpriteC(3)
	Sprite 7,Spritex(0)+VISIBLE+ZOOMX2,Spritey(0) +ZOOMY4+2, SPR38 +#SpriteC(0)

	Gosub WaitaLot
	Sprite 7,Spritex(0)+VISIBLE+ZOOMX2,FlipX+Spritey(0) +ZOOMY4+2, SPR38 +#SpriteC(0)
	Sprite 6,Spritex(0)+VISIBLE-6,Spritey(0)-10 +ZOOMY4+1, SPR60 +SPR_BLACK
	Play Tune_2
	Gosub WaitaLot
Play Tune_1
	For cnt1=Spritex(0) to 80 step -1
		Spritex(0)=Spritex(0)-1
		Sprite 7,Spritex(0)+VISIBLE +ZOOMX2,Spritey(0)+2+ FlipX+ZOOMY4, SPR00+ (8*spriteframe) +#SpriteC(0)
		Sprite 6,Spritex(0)+VISIBLE+4,Spritey(0)-15 +ZOOMY4+1, SPR60 +SPR_BLACK
		Wait
		Wait

		SpriteDelay=SpriteDelay+1: If SpriteDelay>2 then SpriteDelay=0:Spriteframe=Spriteframe+1:If SpriteFrame>1 then SpriteFrame=0
	next cnt1

	For cnt1=Spritex(0) to 166
		Spritex(0)=Spritex(0)+1
		Sprite 7,Spritex(0)+VISIBLE+ZOOMX2,Spritey(0)+ZOOMY4, SPR00+ (8*spriteframe) +#SpriteC(0) +BEHIND
		Sprite 6,Spritex(0)+VISIBLE+2,FLIPX+Spritey(0)-16 +ZOOMY4, SPR60 +SPR_BLACK
		Wait
		Wait
		SpriteDelay=SpriteDelay+1: If SpriteDelay>2 then SpriteDelay=0:Spriteframe=Spriteframe+1:If SpriteFrame>1 then SpriteFrame=0
	next cnt1

	Gosub Pause
End


IntyBasic: Procedure
	Sprite 0,0,0,0
	Sprite 1,0,0,0
	Sprite 2,0,0,0
	Sprite 3,0,0,0
	Sprite 4,0,0,0
	Sprite 5,0,0,0
	Sprite 6,0,0,0
	Sprite 7,0,0,0
	Gosub Init_Flakes
	ShowFlakes=0
	Rem Define a Color Stack screen using colors 1,2,7,4
	MODE SCREEN_CS,1,2,7,4
	WAIT
	SpriteY(0)=SpriteY(0)+8
	Cls
	Wait
	Print at SCREENPOS(0,  0) color CS_WHITE, "Now, with the power of IntyBASIC, you   can join Inty-B in  his fun filled      universe!"
	Print at SCREENPOS(3, 11), "IntyBASIC.Com"
	Gosub WalkAround
End

WalkAround:Procedure


WalkLoop:
	Spriteframe=0
	For cnt1=Spritex(0) to 158
		Spritex(0)=Spritex(0)+1
		Sprite 0,Spritex(0)+VISIBLE,Spritey(0) +ZOOMY2, SPR00+ (8*spriteframe) +#SpriteC(0)
		Sprite 1,Spritex(0)+VISIBLE+1,Spritey(0)-8 +FlipX+ZOOMY2, SPR60 + SPR_BLACK
		If ShowFlakes=1 then Gosub UpdateAnimationCounters
		gosub waitalittle
		Spriteframe=Spriteframe+1:If SpriteFrame>1 then SpriteFrame=0
	next cnt1

	For cnt1=Spritex(0) to 8 step -1
		Spritex(0)=Spritex(0)-1
		Sprite 0,Spritex(0)+VISIBLE,Spritey(0)+FlipX +ZOOMY2, SPR00+ (8*spriteframe) +#SpriteC(0)
		Sprite 1,Spritex(0)+VISIBLE,Spritey(0)-8+ZOOMY2, SPR60 + SPR_BLACK
		If ShowFlakes=1 then Gosub UpdateAnimationCounters
		gosub waitalittle
		if RANDOM(250)> 247 then Showflakes=1
		Spriteframe=Spriteframe+1:If SpriteFrame>1 then SpriteFrame=0
	next cnt1
Rem Loop forever
	Goto WalkLoop
End

Pause: Procedure
	PauseColor=0
	PauseDelay=0
PauseLoop:
	Print at SCREENPOS(19, 11) color PauseColorArray(PauseColor), ">"
	wait
	'Wait
	PauseDelay=PauseDelay+1: If PauseDelay>7 then PauseDelay=0: PauseColor=PauseColor+1: if PauseColor>7 then PauseColor=0
Rem check both controllers for a button or disc right
	if cont.button or Cont.right then gosub checkforNOKeys: Print at SCREENPOS(19, 11) color FG_Yellow, " ": Gosub WaitaBit: return
	Goto PauseLoop
End

PauseColorArray:
	Data CS_DARKGREEN,CS_GREEN,CS_TAN,CS_WHITE,CS_WHITE,CS_TAN,CS_GREEN,CS_DARKGREEN

checkforNOKeys: Procedure
Rem wait for controller release
clearloop:
	clearkey=cont1
	wait
	if clearkey then goto clearloop
	wait
end

WaitaLot:Procedure
gosub waitabit
gosub waitabit
gosub waitabit
gosub waitabit
gosub waitabit
gosub waitabit
gosub waitabit
gosub waitabit
End

WaitaLittle:Procedure
	For cnt=0 to 5
		Wait
	Next cnt
End

WaitaBit:Procedure
	For cnt=0 to 10
		Wait
	Next cnt
End


Color_Values:
	Data $0000,$0001,$0002,$0003 ,$0004,$0005,$0006 ,$0007,$1000,$1001,$1002,$1003,$1004,$1005,$1006 ,$1007
Rem definitions for a bunch of bitmaps
' 2 bitmaps
Inty1:
	BITMAP ".####..."
	BITMAP "##.#.#.."
	BITMAP ".####..#"
	BITMAP "..#.#..#"
	BITMAP ".######."
	BITMAP "#.###..."
	BITMAP "..#.#..."
	BITMAP ".##.##.."

	BITMAP ".####..."
	BITMAP "##.#.#.."
	BITMAP ".####.#."
	BITMAP "..#.#.#."
	BITMAP ".#####.."
	BITMAP "#.###..."
	BITMAP "..#.#..."
	BITMAP ".##.##.."
Inty2:
	BITMAP ".####..."
	BITMAP "##.#.#.."
	BITMAP ".####..."
	BITMAP "..#.#..."
	BITMAP ".#####.."
	BITMAP "#.###.#."
	BITMAP "..#.##.."
	BITMAP ".##....."

	BITMAP ".####..."
	BITMAP "##.#.#.."
	BITMAP ".####..."
	BITMAP "..#.#..."
	BITMAP ".#####.."
	BITMAP "#.###.#."
	BITMAP ".##.#..."
	BITMAP "....##.."

	BITMAP ".####..."
	BITMAP "##.#.#.."
	BITMAP ".####..."
	BITMAP "..#.#..."
	BITMAP ".#####.."
	BITMAP "#.###.#."
	BITMAP "..#.#..."
	BITMAP ".##.##.."

	BITMAP ".#...#.."
	BITMAP ".#...#.."
	BITMAP ".#...#.."
	BITMAP ".#...#.."
	BITMAP ".#...#.."
	BITMAP ".#...#.."
	BITMAP ".#...#.."
	BITMAP "###.###."

Rem Cards

	BITMAP "########"
	BITMAP "##....##"
	BITMAP "#......#"
	BITMAP "#......#"
	BITMAP "#......#"
	BITMAP "#......#"
	BITMAP "##....##"
	BITMAP "########"

	BITMAP ".######."
	BITMAP "##....##"
	BITMAP "#.#..#.#"
	BITMAP "#..##..#"
	BITMAP "#..##..#"
	BITMAP "#.#..#.#"
	BITMAP "##....##"
	BITMAP ".######."
Knight:
	BITMAP "..#.#..."
	BITMAP ".#####.."
	BITMAP ".#.##.#."
	BITMAP ".####.#."
	BITMAP "###.##.#"
	BITMAP "###.##.#"
	BITMAP ".#.#####"
	BITMAP "...#####"


Rem 6 parts of mailbox
MailBox:
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP ".###...."
	BITMAP ".####..."
	BITMAP ".####..."
	BITMAP "....#..."
	BITMAP "....#..."

	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
	BITMAP "....####"
	BITMAP "...###.."
	BITMAP "...###.."
	BITMAP "...###.."

	BITMAP "........"
	BITMAP ".#######"
	BITMAP "####...#"
	BITMAP "####.##."
	BITMAP "####...."
	BITMAP "####.##."
	BITMAP "####...#"
	BITMAP "########"

	BITMAP "........"
	BITMAP "#######."
	BITMAP "########"
	BITMAP "########"
	BITMAP "########"
	BITMAP "#.######"
	BITMAP "#.######"
	BITMAP "########"
Rem door
	BITMAP ".......#"
	BITMAP ".......#"
	BITMAP ".......#"
	BITMAP "......#."
	BITMAP "......#."
	BITMAP "......#."
	BITMAP "........"
	BITMAP "........"

	BITMAP "........"
	BITMAP "........"
	BITMAP "...###.."
	BITMAP "...###.."
	BITMAP "...###.."
	BITMAP "...###.."
	BITMAP "...###.."
	BITMAP "...###.."

	BITMAP "........"
	BITMAP "########"
	BITMAP "########"
	BITMAP "########"
	BITMAP "########"
	BITMAP "########"
	BITMAP "........"
	BITMAP "........"


tune_2:	DATA 7

	MUSIC G4,S
	MUSIC A4,F3
	MUSIC A4#,S
	MUSIC A4,C4
	MUSIC A4#,S
	MUSIC C5,F3

	MUSIC STOP

' Bach Invention 8 (BWV779)
        ' Fragment
tune_1:	DATA 7
	MUSIC F4,-
	MUSIC S,-
	MUSIC A4,-
	MUSIC S,-
	MUSIC F4,-
	MUSIC S,-
	MUSIC C5,-
	MUSIC S,-
	MUSIC F4,-
	MUSIC S,-

	MUSIC F5,-
	MUSIC S,-
	MUSIC E5,F3
	MUSIC D5,S
	MUSIC C5,A3
	MUSIC D5,S
	MUSIC C5,F3
	MUSIC A4#,S
	MUSIC A4,C4
	MUSIC A4#,S
	MUSIC A4,F3
	MUSIC G4,S

	MUSIC F4,F4
	MUSIC S,S
	MUSIC A4,E4
	MUSIC S,D4
	MUSIC C5,C4
	MUSIC S,D4
	MUSIC A4,C4
	MUSIC S,A3#
	MUSIC F5,A3
	MUSIC S,A3#
	MUSIC C5,A3
	MUSIC S,G3

	MUSIC A5,F3
	MUSIC C6,S
	MUSIC A5#,A3
	MUSIC C6,S
	MUSIC A5,C4
	MUSIC C6,S
	MUSIC A5#,A3
	MUSIC C6,S
	MUSIC A5,F4
	MUSIC C6,S
	MUSIC A5#,C4
	MUSIC C6,S

	MUSIC F5,A3
	MUSIC A5,C4
	MUSIC G5,A3#
	MUSIC A5,C4
	MUSIC F5,A3
	MUSIC A5,C4
	MUSIC G5,A3#
	MUSIC A5,C4
	MUSIC F5,A3
	MUSIC A5,C4
	MUSIC G5,A3#
	MUSIC A5,C4

	MUSIC D5,F3
	MUSIC F5,A3
	MUSIC E5,G3
	MUSIC F5,A3
	MUSIC D5,F3
	MUSIC F5,A3
	MUSIC E5,G3
	MUSIC F5,A3
	MUSIC D5,F3
	MUSIC F5,A3
	MUSIC E5,G3
	MUSIC F5,A3

	MUSIC B4,D3
	MUSIC S,F3
	MUSIC G4,E3
	MUSIC S,F3
	MUSIC D5,D3
	MUSIC S,F3
	MUSIC B4,E3
	MUSIC S,F3
	MUSIC F5,D3
	MUSIC S,F3
	MUSIC D5,E3
	MUSIC S,F3

	MUSIC G5,B3
	MUSIC A5,S
	MUSIC G5,G3
	MUSIC F5,S
	MUSIC E5,C4
	MUSIC F5,S
	MUSIC E5,G3
	MUSIC D5,S
	MUSIC C5,E4
	MUSIC D5,S
	MUSIC C5,C4
	MUSIC A4#,S

	MUSIC A4,F4
	MUSIC S,G4
	MUSIC D5,F4
	MUSIC C5,E4
	MUSIC B4,D4
	MUSIC C5,E4
	MUSIC B4,D4
	MUSIC A4,C4
	MUSIC G4,B3
	MUSIC A4,C4
	MUSIC G4,B3
	MUSIC F4,A3

	MUSIC E4,G3
	MUSIC F4,S
	MUSIC E4,C4
	MUSIC D4,B3
	MUSIC C4,A3
	MUSIC S,B3
	MUSIC C5,A3
	MUSIC B4,G3
	MUSIC C5,F3
	MUSIC S,G3
	MUSIC E4,F3
	MUSIC S,E3

	MUSIC F4,D3
	MUSIC S,E3
	MUSIC C5,D3
	MUSIC S,C3
	MUSIC E4,G3
	MUSIC S,F3
	MUSIC C5,E3
	MUSIC S,F3
	MUSIC D4,G3
	MUSIC S,S
	MUSIC B4,G2
	MUSIC S,S

	MUSIC C5,C4
	MUSIC S,S
	MUSIC S,S
	MUSIC S,S
	MUSIC REPEAT


ShowFence:Procedure
	MODE SCREEN_FB
	WAIT
	CLS
	DEFINE DEF20,3,screen_bitmaps_old
	DEFINE ALTERNATE DEF24,7,MailBox
	WAIT
	DEFINE DEF38,1,Inty1
	WAIT
	Define  DEF60, 1, Knight
	WAIT
	SCREEN screen_cardsold
End

	' 3 bitmaps
screen_bitmaps_old:
	BITMAP "........"
	BITMAP "....#..."
	BITMAP "...###.."
	BITMAP "..#####."
	BITMAP "..#####."
	BITMAP "########"
	BITMAP "########"
	BITMAP "..#####."

	BITMAP "..#####."
	BITMAP "..#####."
	BITMAP "..#####."
	BITMAP "########"
	BITMAP "########"
	BITMAP "..#####."
	BITMAP "..#####."
	BITMAP "..#####."

	BITMAP "#.#.#.#."
	BITMAP ".#.#.#.#"
	BITMAP "########"
	BITMAP "########"
	BITMAP "########"
	BITMAP "########"
	BITMAP "########"
	BITMAP "########"


	REM 20x12 cards
screen_cardsold:
	DATA $1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200
	DATA $1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200
	DATA $1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200
	DATA $1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200
	DATA $1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200
	DATA $1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200
	DATA $1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200,$1200
	DATA $1AA7,$1AA7,$1AA7,$1AA7,$1AA7,$1AA7,$1AA7,$1AA7,$1AA7,$1200,$1200,$1AA7,$1AA7,$1AA7,$1AA7,$1AA7,$1AA7,$1AA7,$1AA7,$1AA7
	DATA $1AAF,$1AAF,$1AAF,$1AAF,$1AAF,$1AAF,$1AAF,$1AAF,$1AAF,$1200,$1200,$1AAF,$1AAF,$1AAF,$1AAF,$1AAF,$1AAF,$1AAF,$1AAF,$1AAF
	DATA $08B5,$08B5,$08B5,$08B5,$08B5,$08B5,$08B5,$08B5,$08B5,$08B5,$08B5,$08B5,$08B5,$08B5,$08B5,$08B5,$08B5,$08B5,$08B5,$08B5
	DATA $1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000
	DATA $1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000,$1000


Init_Flakes: Procedure
Define Def50,2, Flakes
Dim Flakesx(8)
DIM Flakesy(8)
DIM FlakesSize(8)

for Flakecounter=2 to 7
	Flakesx(FlakeCounter)=RANDOM(159)+1
	Flakesy(FlakeCounter)=RANDOM(45)-Random(10) 'Flakesx(Flakecounter)
	If Flakesy(Flakecounter)>200 then Flakesy(Flakecounter)=200
	FlakesSize(FlakeCounter)= RANDOM(2)+1
	wait
Next Flakecounter

'Flakesy(2)=40
'Flakesy(3)=78
'Flakesy(4)=30
'Flakesy(5)=1
'Flakesy(6)=89
'Flakesy(7)=13

END

UpdateAnimationCounters: Procedure
	aframecounter=aframecounter+1
	if aframecounter >5 then aframecounter=0: aframe=aframe+1
	if aframe>2 then aframe=1


for Flakecounter=2 to 7
	Flakesy(FlakeCounter)=Flakesy(Flakecounter)+1 'Flakessize(Flakecounter)
	if Flakesy(Flakecounter)>80 then Flakesx(FlakeCounter)=RANDOM(150)+5: Flakesy(FlakeCounter)=Random(5)

	If Flakecounter=2 then	Sprite 2,Flakesx(Flakecounter)+VISIBLE,Flakesy(Flakecounter),(256+49+Flakessize(Flakecounter))*8+SPR_WHITE
	If Flakecounter=3 then	Sprite 3,Flakesx(Flakecounter)+VISIBLE,Flakesy(Flakecounter),(256+49+Flakessize(Flakecounter))*8+SPR_WHITE
	If Flakecounter=4 then	Sprite 4,Flakesx(Flakecounter)+VISIBLE,Flakesy(Flakecounter),(256+49+Flakessize(Flakecounter))*8+SPR_WHITE
	If Flakecounter=5 then	Sprite 5,Flakesx(Flakecounter)+VISIBLE,Flakesy(Flakecounter),(256+49+Flakessize(Flakecounter))*8+SPR_WHITE
	If Flakecounter=6 then	Sprite 6,Flakesx(Flakecounter)+VISIBLE,Flakesy(Flakecounter),(256+49+Flakessize(Flakecounter))*8+SPR_WHITE
	If Flakecounter=7 then	Sprite 7,Flakesx(Flakecounter)+VISIBLE,Flakesy(Flakecounter),(256+49+Flakessize(Flakecounter))*8+SPR_WHITE
Next Flakecounter

	wiggleFlake=random(7)
	wiggleFlakedir=random(6)-3

	if wiggleFlake=2 then Flakesx(wiggleFlake)=Flakesx(wiggleFlake)+wiggleFlakedir
	if wiggleFlake=3 then Flakesx(wiggleFlake)=Flakesx(wiggleFlake)+wiggleFlakedir
	if wiggleFlake=4 then Flakesx(wiggleFlake)=Flakesx(wiggleFlake)+wiggleFlakedir
	if wiggleFlake=5 then Flakesx(wiggleFlake)=Flakesx(wiggleFlake)+wiggleFlakedir
	if wiggleFlake=6 then Flakesx(wiggleFlake)=Flakesx(wiggleFlake)+wiggleFlakedir
	if wiggleFlake=7 then Flakesx(wiggleFlake)=Flakesx(wiggleFlake)+wiggleFlakedir
End

Flakes:

	BITMAP "........"
	BITMAP "..#.#.#."
	Bitmap "...#.#.."
	BITMAP "..#.#.#."
	Bitmap "...#.#.."
	BITMAP "..#.#.#."
	BITMAP "........"
	BITMAP "........"


FlakeSmall:
	BITMAP "........"
	BITMAP "........"
	BITMAP "..#.#..."
	BITMAP "...#...."
	BITMAP "..#.#..."
	BITMAP "........"
	BITMAP "........"
	BITMAP "........"
