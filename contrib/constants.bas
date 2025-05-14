REM -------------------------------------------------------------------------
REM HEADER - CONSTANTS.BAS
REM 
REM Started by Mark Ball, July 2015
REM
REM Constants for use in IntyBASIC
REM
REM HISTORY
REM -------
REM 1.00F 05/07/15 - First version.
REM 1.01F 07/07/15 - Added disc directions.
REM                - Added background modes.
REM                - Minor comment changes.
REM 1.02F 08/07/15 - Renamed constants.
REM                - Added background access information.
REM                - Adjustments to layout.
REM 1.03F 08/07/15 - Fixed comment delimiter.
REM 1.04F 11/07/15 - Added useful functions.
REM	               - Added controller movement mask.
REM 1.05F 11/07/15 - Added BACKGROUND constants.
REM 1.06F 11/07/15 - Changed Y, X order to X, Y in DEF FN functions.
REM 1.07F 11/07/15 - Added colour stack advance.
REM 1.08F 12/07/15 - Added functions for sprite position handling.
REM 1.09F 12/07/15 - Added a function for resetting a sprite.
REM 1.10F 13/07/15 - Added keypad constants.
REM 1.11F 13/07/15 - Added side button constants.
REM 1.12F 13/07/15 - Updated sprite functions.
REM 1.13F 19/07/15 - Added border masking constants.
REM 1.14F 20/07/15 - Added a combined border masking constant.
REM 1.15F 20/07/15 - Renamed border masking constants to BORDER_HIDE_xxxx.
REM 1.16F 28/09/15 - Fixed disc direction typos.
REM 1.17F 30/09/15 - Fixed DISC_SOUTH_WEST value.
REM 1.18F 05/12/15 - Fixed BG_XXXX colours.
REM 1.19F 01/01/16 - Changed name of BACKTAB constant to avoid confusion with #BACKTAB array.
REM                - Added pause key constants.
REM 1.20F 14/01/16 - Added coloured squares mode's pixel colours.
REM 1.21F 15/01/16 - Added coloured squares mode's X and Y limits.
REM 1.22F 23/01/16 - Added PSG constants.
REM -------------------------------------------------------------------------

REM /////////////////////////////////////////////////////////////////////////

REM -------------------------------------------------------------------------
REM Background information.
REM -------------------------------------------------------------------------
CONST BACKTAB_ADDR			= $0200		' Start of the BACKground TABle (BACKTAB) in RAM.
CONST BACKGROUND_ROWS		= 12		' Height of the background in cards.
CONST BACKGROUND_COLUMNS	= 20		' Width of the background in cards.

REM -------------------------------------------------------------------------
REM Background GRAM cards.
REM -------------------------------------------------------------------------
CONST BG00 					= $0800
CONST BG01 					= $0808
CONST BG02 					= $0810
CONST BG03 					= $0818
CONST BG04 					= $0820
CONST BG05 					= $0828
CONST BG06 					= $0830
CONST BG07 					= $0838
CONST BG08 					= $0840
CONST BG09 					= $0848
CONST BG10 					= $0850
CONST BG11 					= $0858
CONST BG12 					= $0860
CONST BG13 					= $0868
CONST BG14 					= $0870
CONST BG15 					= $0878
CONST BG16 					= $0880
CONST BG17 					= $0888
CONST BG18 					= $0890
CONST BG19 					= $0898
CONST BG20 					= $08A0
CONST BG21 					= $08A8
CONST BG22 					= $08B0
CONST BG23 					= $08B8
CONST BG24 					= $08C0
CONST BG25 					= $08C8
CONST BG26 					= $08D0
CONST BG27 					= $08D8
CONST BG28 					= $08E0
CONST BG29 					= $08E8
CONST BG30 					= $08F0
CONST BG31 					= $08F8
CONST BG32 					= $0900
CONST BG33 					= $0908
CONST BG34 					= $0910
CONST BG35 					= $0918
CONST BG36 					= $0920
CONST BG37 					= $0928
CONST BG38 					= $0930
CONST BG39 					= $0938
CONST BG40 					= $0940
CONST BG41 					= $0948
CONST BG42 					= $0950
CONST BG43 					= $0958
CONST BG44 					= $0960
CONST BG45 					= $0968
CONST BG46 					= $0970
CONST BG47 					= $0978
CONST BG48 					= $0980
CONST BG49 					= $0988
CONST BG50 					= $0990
CONST BG51 					= $0998
CONST BG52 					= $09A0
CONST BG53 					= $09A8
CONST BG54 					= $09B0
CONST BG55 					= $09B8
CONST BG56 					= $09C0
CONST BG57 					= $09C8
CONST BG58 					= $09D0
CONST BG59 					= $09D8
CONST BG60 					= $09E0
CONST BG61 					= $09E8
CONST BG62 					= $09F0
CONST BG63 					= $09F8
	
REM /////////////////////////////////////////////////////////////////////////

REM -------------------------------------------------------------------------
REM GRAM card index numbers.
REM -------------------------------------------------------------------------
REM Note: For use with the "define" command.
REM -------------------------------------------------------------------------
CONST DEF00 				= $0000
CONST DEF01 				= $0001
CONST DEF02 				= $0002
CONST DEF03 				= $0003
CONST DEF04 				= $0004
CONST DEF05 				= $0005
CONST DEF06 				= $0006
CONST DEF07 				= $0007
CONST DEF08 				= $0008
CONST DEF09 				= $0009
CONST DEF10 				= $000A
CONST DEF11 				= $000B
CONST DEF12 				= $000C
CONST DEF13 				= $000D
CONST DEF14 				= $000E
CONST DEF15 				= $000F
CONST DEF16 				= $0010
CONST DEF17 				= $0011
CONST DEF18 				= $0012
CONST DEF19 				= $0013
CONST DEF20 				= $0014
CONST DEF21 				= $0015
CONST DEF22 				= $0016
CONST DEF23 				= $0017
CONST DEF24 				= $0018
CONST DEF25 				= $0019
CONST DEF26 				= $001A
CONST DEF27 				= $001B
CONST DEF28 				= $001C
CONST DEF29 				= $001D
CONST DEF30 				= $001E
CONST DEF31 				= $001F
CONST DEF32 				= $0020
CONST DEF33 				= $0021
CONST DEF34 				= $0022
CONST DEF35 				= $0023
CONST DEF36 				= $0024
CONST DEF37 				= $0025
CONST DEF38 				= $0026
CONST DEF39 				= $0027
CONST DEF40 				= $0028
CONST DEF41 				= $0029
CONST DEF42 				= $002A
CONST DEF43 				= $002B
CONST DEF44 				= $002C
CONST DEF45 				= $002D
CONST DEF46 				= $002E
CONST DEF47 				= $002F
CONST DEF48 				= $0030
CONST DEF49 				= $0031
CONST DEF50 				= $0032
CONST DEF51 				= $0033
CONST DEF52 				= $0034
CONST DEF53 				= $0035
CONST DEF54 				= $0036
CONST DEF55 				= $0037
CONST DEF56 				= $0038
CONST DEF57 				= $0039
CONST DEF58 				= $003A
CONST DEF59 				= $003B
CONST DEF60 				= $003C
CONST DEF61 				= $003D
CONST DEF62 				= $003E
CONST DEF63 				= $003F

REM /////////////////////////////////////////////////////////////////////////

REM -------------------------------------------------------------------------
REM Screen modes.
REM -------------------------------------------------------------------------
REM Note: For use with the "mode" command.
REM -------------------------------------------------------------------------
CONST SCREEN_COLOR_STACK			= $0000
CONST SCREEN_FOREGROUND_BACKGROUND	= $0001
REM Abbreviated versions.
CONST SCREEN_CS						= $0000
CONST SCREEN_FB						= $0001

REM /////////////////////////////////////////////////////////////////////////

REM -------------------------------------------------------------------------
REM COLORS - Border.
REM -------------------------------------------------------------------------
REM Notes:
REM - For use with the commands "mode 0" and "mode 1".
REM - For use with the "border" command.
REM -------------------------------------------------------------------------
CONST BORDER_BLACK			= $0000
CONST BORDER_BLUE			= $0001
CONST BORDER_RED			= $0002
CONST BORDER_TAN			= $0003
CONST BORDER_DARKGREEN		= $0004
CONST BORDER_GREEN			= $0005
CONST BORDER_YELLOW			= $0006
CONST BORDER_WHITE			= $0007
CONST BORDER_GREY			= $0008
CONST BORDER_CYAN			= $0009
CONST BORDER_ORANGE			= $000A
CONST BORDER_BROWN			= $000B
CONST BORDER_PINK			= $000C
CONST BORDER_LIGHTBLUE		= $000D
CONST BORDER_YELLOWGREEN	= $000E
CONST BORDER_PURPLE			= $000F

REM /////////////////////////////////////////////////////////////////////////

REM -------------------------------------------------------------------------
REM BORDER - Edge masks.
REM -------------------------------------------------------------------------
REM Note: For use with the "border color, edge" command.
REM -------------------------------------------------------------------------
CONST BORDER_HIDE_LEFT_EDGE		= $0001		' Hide the leftmost column of the background.
CONST BORDER_HIDE_TOP_EDGE		= $0002		' Hide the topmost row of the background.
CONST BORDER_HIDE_TOP_LEFT_EDGE	= $0003		' Hide both the topmost row and leftmost column of the background.

REM /////////////////////////////////////////////////////////////////////////

REM -------------------------------------------------------------------------
REM COLORS - Mode 0 (Color Stack).
REM -------------------------------------------------------------------------
REM Stack
REM -------------------------------------------------------------------------
REM Note: For use as the last 4 parameters used in the "mode 1" command.
REM -------------------------------------------------------------------------
CONST STACK_BLACK			= $0000
CONST STACK_BLUE			= $0001
CONST STACK_RED				= $0002
CONST STACK_TAN				= $0003
CONST STACK_DARKGREEN		= $0004
CONST STACK_GREEN			= $0005
CONST STACK_YELLOW			= $0006
CONST STACK_WHITE			= $0007
CONST STACK_GREY			= $0008
CONST STACK_CYAN			= $0009
CONST STACK_ORANGE			= $000A
CONST STACK_BROWN			= $000B
CONST STACK_PINK			= $000C
CONST STACK_LIGHTBLUE		= $000D
CONST STACK_YELLOWGREEN		= $000E
CONST STACK_PURPLE			= $000F

REM -------------------------------------------------------------------------
REM Foreground.
REM -------------------------------------------------------------------------
REM Notes:
REM - For use with "peek/poke" commands that access BACKTAB.
REM - Only one foreground colour permitted per background card.
REM -------------------------------------------------------------------------
CONST CS_BLACK				= $0000
CONST CS_BLUE				= $0001
CONST CS_RED				= $0002
CONST CS_TAN				= $0003
CONST CS_DARKGREEN			= $0004
CONST CS_GREEN				= $0005
CONST CS_YELLOW				= $0006
CONST CS_WHITE				= $0007
CONST CS_GREY				= $1000
CONST CS_CYAN				= $1001
CONST CS_ORANGE				= $1002
CONST CS_BROWN				= $1003
CONST CS_PINK				= $1004
CONST CS_LIGHTBLUE			= $1005
CONST CS_YELLOWGREEN		= $1006
CONST CS_PURPLE				= $1007

CONST CS_CARD_DATA_MASK		= $07F8		' Mask to get the background card's data.

CONST CS_ADVANCE			= $2000		' Advance the colour stack by one position.

REM -------------------------------------------------------------------------
REM Coloured squares mode.
REM -------------------------------------------------------------------------
REM Notes :
REM - Only available in colour stack mode.
REM - Pixels in each BACKTAB card are arranged in the following manner:
REM +-------+-------+
REM | Pixel | Pixel |
REM |   0   |   1   !
REM +-------+-------+
REM | Pixel | Pixel |
REM |   2   |   3   !
REM +-------+-------+
REM
REM -------------------------------------------------------------------------
CONST CS_COLOUR_SQUARES_ENABLE	=$1000
CONST CS_PIX0_BLACK				=0
CONST CS_PIX0_BLUE				=1
CONST CS_PIX0_RED				=2
CONST CS_PIX0_TAN				=3
CONST CS_PIX0_DARKGREEN			=4
CONST CS_PIX0_GREEN				=5
CONST CS_PIX0_YELLOW			=6
CONST CS_PIX0_BACKGROUND		=7
CONST CS_PIX1_BLACK				=0
CONST CS_PIX1_BLUE				=1*8
CONST CS_PIX1_RED				=2*8
CONST CS_PIX1_TAN				=3*8
CONST CS_PIX1_DARKGREEN			=4*8
CONST CS_PIX1_GREEN				=5*8
CONST CS_PIX1_YELLOW			=6*8
CONST CS_PIX1_BACKGROUND		=7*8
CONST CS_PIX2_BLACK				=0
CONST CS_PIX2_BLUE				=1*64
CONST CS_PIX2_RED				=2*64
CONST CS_PIX2_TAN				=3*64
CONST CS_PIX2_DARKGREEN			=4*64
CONST CS_PIX2_GREEN				=5*64
CONST CS_PIX2_YELLOW			=6*64
CONST CS_PIX2_BACKGROUND		=7*64
CONST CS_PIX3_BLACK				=0
CONST CS_PIX3_BLUE				=$0200
CONST CS_PIX3_RED				=$0400
CONST CS_PIX3_TAN				=$0600
CONST CS_PIX3_DARKGREEN			=$2000
CONST CS_PIX3_GREEN				=$2200
CONST CS_PIX3_YELLOW			=$2400
CONST CS_PIX3_BACKGROUND		=$2600
CONST CS_PIX_MASK				=CS_COLOUR_SQUARES_ENABLE+CS_PIX0_BACKGROUND+CS_PIX1_BACKGROUND+CS_PIX2_BACKGROUND+CS_PIX3_BACKGROUND

CONST CS_PIX_X_MIN				=0		' Minimum x coordinate.
CONST CS_PIX_X_MAX				=39		' Maximum x coordinate.
CONST CS_PIX_Y_MIN				=0		' Minimum Y coordinate.
CONST CS_PIX_Y_MAX				=23		' Maximum Y coordinate.

REM /////////////////////////////////////////////////////////////////////////

REM -------------------------------------------------------------------------
REM COLORS - Mode 1 (Foreground Background)
REM -------------------------------------------------------------------------
REM Foreground.
REM -------------------------------------------------------------------------
REM Notes:
REM - For use with "peek/poke" commands that access BACKTAB.
REM - Only one foreground colour permitted per background card.
REM -------------------------------------------------------------------------
CONST FG_BLACK				= $0000
CONST FG_BLUE				= $0001
CONST FG_RED				= $0002
CONST FG_TAN				= $0003
CONST FG_DARKGREEN			= $0004
CONST FG_GREEN				= $0005
CONST FG_YELLOW				= $0006
CONST FG_WHITE				= $0007

REM -------------------------------------------------------------------------
REM Background.
REM -------------------------------------------------------------------------
REM Notes:
REM - For use with "peek/poke" commands that access BACKTAB.
REM - Only one background colour permitted per background card.
REM -------------------------------------------------------------------------
CONST BG_BLACK				= $0000
CONST BG_BLUE				= $0200
CONST BG_RED				= $0400
CONST BG_TAN				= $0600
CONST BG_DARKGREEN			= $2000
CONST BG_GREEN				= $2200
CONST BG_YELLOW				= $2400
CONST BG_WHITE				= $2600
CONST BG_GREY				= $1000
CONST BG_CYAN				= $1200
CONST BG_ORANGE				= $1400
CONST BG_BROWN				= $1600
CONST BG_PINK				= $3000
CONST BG_LIGHTBLUE			= $3200
CONST BG_YELLOWGREEN		= $3400
CONST BG_PURPLE				= $3600

CONST FGBG_CARD_DATA_MASK	= $01F8		' Mask to get the background card's data.

REM /////////////////////////////////////////////////////////////////////////

REM -------------------------------------------------------------------------
REM Sprites.
REM -------------------------------------------------------------------------
REM Note: For use with "sprite" command.
REM -------------------------------------------------------------------------
REM X
REM -------------------------------------------------------------------------
REM Note: Add these constants to the sprite command's X parameter.
REM -------------------------------------------------------------------------
CONST HIT					= $0100		' Enable the sprite's collision detection.
CONST VISIBLE				= $0200		' Make the sprite visible.
CONST ZOOMX2				= $0400		' Make the sprite twice the width.

REM -------------------------------------------------------------------------
REM Y
REM -------------------------------------------------------------------------
REM Note: Add these constants to the sprite command's Y parameter.
REM -------------------------------------------------------------------------
CONST DOUBLEY				= $0080		' Make a double height sprite (with 2 GRAM cards).
CONST ZOOMY2				= $0100		' Make the sprite twice (x2) the normal height.
CONST ZOOMY4				= $0200		' Make the sprite quadruple (x4) the normal height.
CONST ZOOMY8				= $0300		' Make the sprite octuple (x8) the normal height.
CONST FLIPX					= $0400		' Flip/mirror the sprite in X.
CONST FLIPY					= $0800		' Flip/mirror the sprite in Y.
CONST MIRROR				= $0C00		' Flip/mirror the sprite in both X and Y.

REM -------------------------------------------------------------------------
REM A
REM -------------------------------------------------------------------------
REM Notes:
REM - Combine to create the sprite command's A parameter.
REM - Only one colour per sprite.
REM -------------------------------------------------------------------------
CONST GRAM					= $0800		' Sprite's data is located in GRAM.
CONST BEHIND				= $2000		' Sprite is behind the background.
CONST SPR_BLACK				= $0000
CONST SPR_BLUE				= $0001
CONST SPR_RED				= $0002
CONST SPR_TAN				= $0003
CONST SPR_DARKGREEN			= $0004
CONST SPR_GREEN				= $0005
CONST SPR_YELLOW			= $0006
CONST SPR_WHITE				= $0007
CONST SPR_GREY				= $1000
CONST SPR_CYAN				= $1001
CONST SPR_ORANGE			= $1002
CONST SPR_BROWN				= $1003
CONST SPR_PINK				= $1004
CONST SPR_LIGHTBLUE			= $1005
CONST SPR_YELLOWGREEN		= $1006
CONST SPR_PURPLE			= $1007

REM -------------------------------------------------------------------------
REM GRAM numbers.
REM -------------------------------------------------------------------------
REM Note: For use in the sprite command's parameter A.
REM -------------------------------------------------------------------------
CONST SPR00 				= $0800
CONST SPR01 				= $0808
CONST SPR02 				= $0810
CONST SPR03 				= $0818
CONST SPR04 				= $0820
CONST SPR05 				= $0828
CONST SPR06 				= $0830
CONST SPR07 				= $0838
CONST SPR08 				= $0840
CONST SPR09 				= $0848
CONST SPR10 				= $0850
CONST SPR11 				= $0858
CONST SPR12 				= $0860
CONST SPR13 				= $0868
CONST SPR14 				= $0870
CONST SPR15 				= $0878
CONST SPR16 				= $0880
CONST SPR17 				= $0888
CONST SPR18 				= $0890
CONST SPR19 				= $0898
CONST SPR20 				= $08A0
CONST SPR21 				= $08A8
CONST SPR22 				= $08B0
CONST SPR23 				= $08B8
CONST SPR24 				= $08C0
CONST SPR25 				= $08C8
CONST SPR26 				= $08D0
CONST SPR27 				= $08D8
CONST SPR28 				= $08E0
CONST SPR29 				= $08E8
CONST SPR30 				= $08F0
CONST SPR31 				= $08F8
CONST SPR32 				= $0900
CONST SPR33 				= $0908
CONST SPR34 				= $0910
CONST SPR35 				= $0918
CONST SPR36 				= $0920
CONST SPR37 				= $0928
CONST SPR38 				= $0930
CONST SPR39 				= $0938
CONST SPR40 				= $0940
CONST SPR41 				= $0948
CONST SPR42 				= $0950
CONST SPR43 				= $0958
CONST SPR44 				= $0960
CONST SPR45 				= $0968
CONST SPR46 				= $0970
CONST SPR47 				= $0978
CONST SPR48 				= $0980
CONST SPR49 				= $0988
CONST SPR50 				= $0990
CONST SPR51 				= $0998
CONST SPR52 				= $09A0
CONST SPR53 				= $09A8
CONST SPR54 				= $09B0
CONST SPR55 				= $09B8
CONST SPR56 				= $09C0
CONST SPR57 				= $09C8
CONST SPR58 				= $09D0
CONST SPR59 				= $09D8
CONST SPR60 				= $09E0
CONST SPR61 				= $09E8
CONST SPR62 				= $09F0
CONST SPR63 				= $09F8

REM -------------------------------------------------------------------------
REM Sprite collision.
REM -------------------------------------------------------------------------
REM Notes:
REM - For use with variables COL0, COL1, COL2, COL3, COL4, COL5, COL6 and COL7.
REM - More than one collision can occur simultaneously.
REM -------------------------------------------------------------------------
CONST HIT_SPRITE0			= $0001		' Sprite collided with sprite 0.
CONST HIT_SPRITE1			= $0002		' Sprite collided with sprite 1.
CONST HIT_SPRITE2			= $0004		' Sprite collided with sprite 2.
CONST HIT_SPRITE3			= $0008		' Sprite collided with sprite 3.
CONST HIT_SPRITE4			= $0010		' Sprite collided with sprite 4.
CONST HIT_SPRITE5			= $0020		' Sprite collided with sprite 5.
CONST HIT_SPRITE6			= $0040		' Sprite collided with sprite 6.
CONST HIT_SPRITE7			= $0080		' Sprite collided with sprite 7.
CONST HIT_BACKGROUND		= $0100		' Sprite collided with a background pixel.
CONST HIT_BORDER			= $0200		' Sprite collided with the top/bottom/left/right border.

REM /////////////////////////////////////////////////////////////////////////

REM -------------------------------------------------------------------------
REM DISC - Compass.
REM -------------------------------------------------------------------------
REM   NW         N         NE
REM     \   NNW  |  NNE   /
REM       \      |      /
REM         \    |    /
REM    WNW    \  |  /    ENE
REM             \|/
REM  W ----------+---------- E
REM             /|\ 
REM    WSW    /  |  \    ESE
REM         /    |    \
REM       /      |      \
REM     /   SSW  |  SSE   \
REM   SW         S         SE
REM -------------------------------------------------------------------------
REM Notes:
REM - North points upwards on the hand controller.
REM - Directions are listed in a clockwise manner.
REM -------------------------------------------------------------------------
CONST DISC_NORTH			= $0004
CONST DISC_NORTH_NORTH_EAST = $0014
CONST DISC_NORTH_EAST		= $0016
CONST DISC_EAST_NORTH_EAST	= $0006
CONST DISC_EAST				= $0002
CONST DISC_EAST_SOUTH_EAST	= $0012
CONST DISC_SOUTH_EAST		= $0013
CONST DISC_SOUTH_SOUTH_EAST	= $0003
CONST DISC_SOUTH			= $0001
CONST DISC_SOUTH_SOUTH_WEST	= $0011
CONST DISC_SOUTH_WEST		= $0019
CONST DISC_WEST_SOUTH_WEST	= $0009
CONST DISC_WEST				= $0008
CONST DISC_WEST_NORTH_WEST	= $0018
CONST DISC_NORTH_WEST		= $001C
CONST DISC_NORTH_NORTH_WEST	= $000C

REM -------------------------------------------------------------------------
REM DISC - Compass abbreviated versions.
REM -------------------------------------------------------------------------
CONST DISC_N				= $0004
CONST DISC_NNE 				= $0014
CONST DISC_NE				= $0016
CONST DISC_ENE				= $0006
CONST DISC_E				= $0002
CONST DISC_ESE				= $0012
CONST DISC_SE				= $0013
CONST DISC_SSE				= $0003
CONST DISC_S				= $0001
CONST DISC_SSW				= $0011
CONST DISC_SW				= $0019
CONST DISC_WSW				= $0009
CONST DISC_W				= $0008
CONST DISC_WNW				= $0018
CONST DISC_NW				= $001C
CONST DISC_NNW				= $000C

REM -------------------------------------------------------------------------
REM DISC - Directions.
REM -------------------------------------------------------------------------
CONST DISC_UP				= $0004
CONST DISC_UP_RIGHT			= $0016		' Up and right diagonal.
CONST DISC_RIGHT			= $0002
CONST DISC_DOWN_RIGHT		= $0013		' Down  and right diagonal.
CONST DISC_DOWN				= $0001
CONST DISC_DOWN_LEFT		= $0019		' Down and left diagonal.
CONST DISC_LEFT				= $0008
CONST DISC_UP_LEFT			= $001C		' Up and left diagonal.

REM -------------------------------------------------------------------------
REM DISK - Mask.
REM -------------------------------------------------------------------------
CONST DISK_MASK				= $001F

REM -------------------------------------------------------------------------
REM Controller - Keypad.
REM -------------------------------------------------------------------------
CONST KEYPAD_0				= 72
CONST KEYPAD_1				= 129
CONST KEYPAD_2				= 65
CONST KEYPAD_3				= 33
CONST KEYPAD_4				= 130
CONST KEYPAD_5				= 66
CONST KEYPAD_6				= 34
CONST KEYPAD_7				= 132
CONST KEYPAD_8				= 68
CONST KEYPAD_9				= 36
CONST KEYPAD_CLEAR			= 136
CONST KEYPAD_ENTER			= 40

REM -------------------------------------------------------------------------
REM Controller - Pause buttons (1+9 or 3+7 held down simultaneously).
REM -------------------------------------------------------------------------
REM Notes:
REM - Key codes for 3+7 and 1+9 are the same (165).
REM -------------------------------------------------------------------------
CONST KEYPAD_PAUSE			= (KEYPAD_1 XOR KEYPAD_9)

REM -------------------------------------------------------------------------
REM Controller - Side buttons.
REM -------------------------------------------------------------------------
CONST BUTTON_TOP_LEFT		= $A0		' Top left and top right are the same button.
CONST BUTTON_TOP_RIGHT		= $A0		' Note: Bit 6 is low. 
CONST BUTTON_BOTTOM_LEFT	= $60		' Note: Bit 7 is low.
CONST BUTTON_BOTTOM_RIGHT	= $C0		' Note: Bit 5 is low

REM Abbreviated versions.
CONST BUTTON_1				= $A0		' Top left or top right.
CONST BUTTON_2				= $60		' Bottom left.
CONST BUTTON_3				= $C0		' Bottom right.

REM Mask.
CONST BUTTON_MASK			= $E0

REM /////////////////////////////////////////////////////////////////////////

REM -------------------------------------------------------------------------
REM Programmable Sound Generator (PSG)
REM -------------------------------------------------------------------------
REM Notes:
REM - For use with the SOUND command
REM -------------------------------------------------------------------------

REM -------------------------------------------------------------------------
REM Internal sound hardware.
REM -------------------------------------------------------------------------
CONST PSG_CHANNELA		=0
CONST PSG_CHANNELB		=1
CONST PSG_CHANNELC		=2
CONST PSG_ENVELOPE		=3
CONST PSG_MIXER			=4

REM -------------------------------------------------------------------------
REM ECS sound hardware.
REM -------------------------------------------------------------------------
CONST PSG_ECS_CHANNELA	=5
CONST PSG_ECS_CHANNELB	=6
CONST PSG_ECS_CHANNELC	=7
CONST PSG_ECS_ENVELOPE	=8
CONST PSG_ECS_MIXER		=9

REM -------------------------------------------------------------------------
REM PSG - Volume control.
REM -------------------------------------------------------------------------
REM Notes:
REM - For use in the volume field of the SOUND command.
REM - Internal channels: PSG_CHANNELA, PSG_CHANNELB, PSG_CHANNELC
REM - ECS channels: PSG_ECS_CHANNELA, PSG_ECS_CHANNELB, PSG_ECS_CHANNELC
REM -------------------------------------------------------------------------
CONST PSG_VOLUME_MAX		=15	' Maximum channel volume.
CONST PSG_ENVELOPE_ENABLE	=48	' Channel volume is controlled by envelope generator.

REM -------------------------------------------------------------------------
REM PSG - Mixer control.
REM -------------------------------------------------------------------------
REM Notes:
REM - Internal channel: PSG_MIXER
REM - EXS channel: PSG_ECS_MIXER
REM -------------------------------------------------------------------------
CONST PSG_TONE_CHANNELA_DISABLE		=$01	' Disable channel A tone.
CONST PSG_TONE_CHANNELB_DISABLE		=$02	' Disable channel B tone.
CONST PSG_TONE_CHANNELC_DISABLE		=$04	' Disable channel C tone.
CONST PSG_NOISE_CHANNELA_DISABLE	=$08	' Disable channel A noise.
CONST PSG_NOISE_CHANNELB_DISABLE	=$10	' Disable channel B noise.
CONST PSG_NOISE_CHANNELC_DISABLE	=$20	' Disable channel C noise.
CONST PSG_MIXER_DEFAULT				=$38 	' All notes enabled. all noise disabled.

REM -------------------------------------------------------------------------
REM PSG - Envelope control.
REM -------------------------------------------------------------------------
REM Notes:
REM - Internal channel: PSG_ENVELOPE
REM - EXS channel: PSG_ECS_ENVELOPE
REM -------------------------------------------------------------------------
CONST PSG_ENVELOPE_HOLD								=$01
CONST PSG_ENVELOPE_ALTERNATE						=$02
CONST PSG_ENVELOPE_ATTACK							=$04
CONST PSG_ENVELOPE_CONTINUE							=$08
CONST PSG_ENVELOPE_SINGLE_SHOT_RAMP_DOWN_AND_OFF	=$00 '\______
CONST PSG_ENVELOPE_SINGLE_SHOT_RAMP_UP_AND_OFF		=$04 '/______
CONST PSG_ENVELOPE_CYCLE_RAMP_DOWN_SAWTOOTH			=$08 '\\\\\\\
CONST PSG_ENVELOPE_CYCLE_RAMP_DOWN_TRIANGLE			=$0A '\/\/\/\
CONST PSG_ENVELOPE_SINGLE_SHOT_RAMP_DOWN_AND_MAX	=$0B '\^^^^^^
CONST PSG_ENVELOPE_CYCLE_RAMP_UP_SAWTOOTH			=$0C '///////
CONST PSG_ENVELOPE_SINGLE_SHOT_RAMP_UP_AND_MAX		=$0D '/^^^^^^
CONST PSG_ENVELOPE_CYCLE_RAMP_UP_TRIANGLE			=$0E '/\/\/\/

REM /////////////////////////////////////////////////////////////////////////

REM -------------------------------------------------------------------------
REM Useful functions.
REM -------------------------------------------------------------------------
DEF FN screenpos(aColumn, aRow)		= (((aRow)*BACKGROUND_COLUMNS)+(aColumn))
DEF FN screenaddr(aColumn, aRow)	= (BACKTAB_ADDR+(((aRow)*BACKGROUND_COLUMNS)+(aColumn)))

DEF FN setspritex(aSpriteNo,anXPosition)	= #mobshadow(aSpriteNo)=(#mobshadow(aSpriteNo) and $ff00)+anXPosition
DEF FN setspritey(aSpriteNo,aYPosition)		= #mobshadow(aSpriteNo+8)=(#mobshadow(aSpriteNo+8) and $ff80)+aYPosition
DEF FN resetsprite(aSpriteNo)				= sprite aSpriteNo, 0, 0, 0

REM /////////////////////////////////////////////////////////////////////////

REM -------------------------------------------------------------------------
REM END
REM -------------------------------------------------------------------------
