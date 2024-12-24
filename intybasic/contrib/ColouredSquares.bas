REM Module:			ColouredSquares.bas
REM
REM Description:	An example of line drawing in Coloured Squares mode.
REM Author:			Mark Ball
REM Date:			14/01/16
REM Version:		1.00F
REM
REM HISTORY
REM -------
REM 1.00F 14/01/16 - First release
REM
REM -------------------------------------------------------------------------

	' We need some important constants.
    include "constants.bas"

REM -------------------------------------------------------------------------
REM Constants.
REM -------------------------------------------------------------------------
CONST ROBOTRON_WIPE_START_WIDTH	=17
CONST ROBOTRON_WIPE_START_HEIGHT	=1
CONST ROBOTRON_WIPE_GROW_STEP_X	=2
CONST ROBOTRON_WIPE_GROW_STEP_Y	=2

REM -------------------------------------------------------------------------
REM Variables.
REM -------------------------------------------------------------------------
    dim xx(2)	' Array to hold start/end X coordinates for the line.
    dim yy(2)	' Array to hold start/end Y coordinates for the line.

REM -------------------------------------------------------------------------
REM Main loop
REM -------------------------------------------------------------------------	
loop:

	' Rotating line wipe.
    gosub RotatingWipe

	' "Robotron" style wipe.
    cls
    gosub RobotronWipe

    goto loop

REM -------------------------------------------------------------------------
REM RobotronWipe - Robotron style wipe.
REM
REM Notes:
REM
REM Inputs:
REM
REM Trashes:
REM
REM -------------------------------------------------------------------------
RobotronWipe: procedure

	' ======================================================================
    ' PHASE 1 - Alternating colour bars spreading out from centre of screen.
	' ======================================================================
	
    ' Initial box width and height.
    width=ROBOTRON_WIPE_START_WIDTH
    height=ROBOTRON_WIPE_START_HEIGHT

    ' start with a red box.
    colour=2

    for p=11 to 0 step -1

        ' Horizontal bar across the top.
        xx(0)=p
        yy(0)=p
        yy(1)=p
        xx(1)=p+width
        gosub CsLine

        ' Vertical bar down the right side.
        xx(0)=p+width
        yy(0)=p
        yy(1)=p+height
        xx(1)=p+width
        gosub CsLine

        ' Horizontal bar across the bottom.
        xx(0)=p+width
        yy(0)=p+height
        yy(1)=p+height
        xx(1)=p
        gosub CsLine

        ' Vertical bar up the left side.
        xx(0)=p
        yy(0)=p+height
        yy(1)=p
        xx(1)=p
        gosub CsLine

        ' Increase box size.
        width=width+ROBOTRON_WIPE_GROW_STEP_X
        height=height+ROBOTRON_WIPE_GROW_STEP_Y

        ' Alternate colours between red and yellow.
        colour=colour XOR 4

        ' Wait for VBLANK
        wait
    next p

	' ========================================================
    ' PHASE 2 - Black box spreading out from centre of screen.
	' ========================================================
	
    ' Initial box width and height.
    width=ROBOTRON_WIPE_START_WIDTH
    height=ROBOTRON_WIPE_START_HEIGHT

    ' start with a black box.
    colour=0

    for p=11 to 0 step -1

        ' Horizontal bar across the top.
        xx(0)=p
        yy(0)=p
        yy(1)=p
        xx(1)=p+width
        gosub CsLine

        ' Vertical bar down the right side.
        xx(0)=p+width
        yy(0)=p
        yy(1)=p+height
        xx(1)=p+width
        gosub CsLine

        ' Horizontal bar across the bottom.
        xx(0)=p+width
        yy(0)=p+height
        yy(1)=p+height
        xx(1)=p
        gosub CsLine

        ' Vertical bar up the left side.
        xx(0)=p
        yy(0)=p+height
        yy(1)=p
        xx(1)=p
        gosub CsLine

        ' Increase box size.
        width=width+ROBOTRON_WIPE_GROW_STEP_X
        height=height+ROBOTRON_WIPE_GROW_STEP_Y

        ' Wait for VBLANK
        wait
    next p

    end
REM -------------------------------------------------------------------------
REM RobotronWipe - END
REM -------------------------------------------------------------------------

REM -------------------------------------------------------------------------
REM RotatingWipe - Rotating line wipe.
REM
REM Notes:
REM
REM Inputs:
REM
REM Trashes:
REM
REM -------------------------------------------------------------------------
RotatingWipe: procedure

    for colour=1 to 6
        ' Along top and bottom X.
        for x=0 to 39
            xx(0)=x
            yy(0)=0
            xx(1)=39-x
            yy(1)=23
            gosub CsLine
			
            wait
        next x

        ' Along left and right Y.
        for y=1 to 22
            xx(0)=39
            yy(0)=y
            xx(1)=0
            yy(1)=23-y
            gosub CsLine
			
            wait
        next y
    next colour

    end
REM -------------------------------------------------------------------------
REM RotatingWipe - END
REM -------------------------------------------------------------------------

REM -------------------------------------------------------------------------
REM CsLine - Draw a line using coloured squares mode.
REM -------------------------------------------------------------------------
REM
REM Notes:
REM - Ref: http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#C
REM
REM Input:
REM xx(0) - Source X coordinate (in range 0 to 39).
REM yy(0) - Source Y coordinate (in range 0 to 23).
REM xx(1) - Destination X coordinate (in range 0 to 39).
REM yy(1) - Destination Y coordinate (in range 0 to 23).
REM colour - Colour of the pixel (in the range 0 to 6).
REM
REM Trashes:-
REM x(0), y(0), deltaX, deltaY, signX, signY, #e2, #error
REM
REM -------------------------------------------------------------------------
CsLine: procedure

' Compute slope delta and direction for X.
    deltaX=abs(xx(1)-xx(0))
    signX=sgn(xx(1)-xx(0))

' Compute slope delta and direction for Y.
    deltaY=abs(yy(1)-yy(0))
    signY=sgn(yy(1)-yy(0))

' Initialise the error.
    if (deltaX>deltaY) then #error=deltaX/2 else #error=-(deltaY/2)

CsLineLoop:

' Plot the point in the current colour,
    gosub CsPlot

' Finished drawing the line?
    if xx(0)=xx(1) AND yy(0)=yy(1) then return

    #e2=#error

' Check if its time to change X coordinate.
    if #e2>-deltaX then
        #error=#error-deltaY
        xx(0)=xx(0)+signX
    end if

' Check if its time to change Y coordinate.
    if #e2<deltaY then
        #error=#error+deltaX
        yy(0)=yy(0)+signY
    end if

    goto CsLineLoop
	
    end	
REM -------------------------------------------------------------------------
REM CsLine - END
REM -------------------------------------------------------------------------

REM -------------------------------------------------------------------------
REM CsPlot - Plot a pixel in coloured squares mode.
REM -------------------------------------------------------------------------
REM
REM Input:
REM xx(0) - X coordinate (in the range 0 to 39).
REM yy(0) - Y coordinate (in the range 0 to 23).
REM colour - Colour of the pixel (in the range 0 to 6).
REM
REM Trashes:
REM #newPixel, #oldPixelMask, where, #what
REM
REM -------------------------------------------------------------------------
CsPlot: procedure

' Pick the colour of the pixel from the right table based on whether
' the X and Y coordinates are odd or even.
    if yy(0) AND 1 then
        if xx(0) AND 1 then
            #newPixel=CsColourPix3Table(colour)
            #oldPixelMask=NOT CS_PIX3_BACKGROUND
        else
            #newPixel=CsColourPix2Table(colour)
            #oldPixelMask=NOT CS_PIX2_BACKGROUND
        end if
    else
        if xx(0) AND 1 then
            #newPixel=CsColourPix1Table(colour)
            #oldPixelMask=NOT CS_PIX1_BACKGROUND
         else
            #newPixel=CsColourPix0Table(colour)
            #oldPixelMask=NOT CS_PIX0_BACKGROUND
        end if
    end if

' Compute an index into BACKTAB from the coordinates.
    where=SCREENPOS(xx(0)/2,yy(0)/2)

' Get the card from BACKTAB and mask out anything we don't need.
    #what=#BACKTAB(where) AND #oldPixelMask

' Update BACKTAB with the new pixel colour (and coloured squares mode enabled).
    #BACKTAB(where)=#what OR #newPixel

    end

' Colour table for pixel 0.
CsColourPix0Table:
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX0_BLACK
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX0_BLUE
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX0_RED
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX0_TAN
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX0_DARKGREEN
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX0_GREEN
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX0_YELLOW

' Colour table for pixel 1.
CsColourPix1Table:
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX1_BLACK
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX1_BLUE
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX1_RED
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX1_TAN
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX1_DARKGREEN
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX1_GREEN
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX1_YELLOW

' Colour table for pixel 2.
CsColourPix2Table:
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX2_BLACK
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX2_BLUE
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX2_RED
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX2_TAN
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX2_DARKGREEN
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX2_GREEN
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX2_YELLOW

' Colour table for pixel 3.
CsColourPix3Table:
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX3_BLACK
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX3_BLUE
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX3_RED
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX3_TAN
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX3_DARKGREEN
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX3_GREEN
    data CS_COLOUR_SQUARES_ENABLE+CS_PIX3_YELLOW
REM -------------------------------------------------------------------------
REM CsPlot - END
REM -------------------------------------------------------------------------
