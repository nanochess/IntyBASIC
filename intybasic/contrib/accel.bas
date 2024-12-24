'' ======================================================================== ''
''  Acceleration and velocity                                               ''
''  J. Zbiciak                                                              ''
''  Licence: Creative Commons CC0                                           ''
''                                                                          ''
''  This demo illustrates one method for implementing object motion.        ''
''                                                                          ''
''  Press keypad 1 through 8 to select an object.                           ''
''  Press DISC to accelerate an object in the desired direction.            ''
''  Press an action button to decelerate the object.                        ''
'' ======================================================================== ''
ASM CFGVAR "author" = "Joe Zbiciak"
ASM CFGVAR "name" = "Acceleration and Velocity Demo"
ASM CFGVAR "short_name" = "Accel & Vel Demo"
ASM CFGVAR "description" = \
"A simple demonstration of acceleration and velocity for object motion."
ASM CFGVAR "year" = 2018
ASM CFGVAR "build_date" = TODAY_STR_LOC("%Y-%m-%d %H:%M:%S %z")
ASM CFGVAR "license" = "CC CC0"

'' ======================================================================== ''
''  Belt, braces, staples, and glue.                                        ''
'' ======================================================================== ''
OPTION WARNINGS ON
OPTION EXPLICIT ON

'' ======================================================================== ''
''  Position, velocity and acceleration variables:                          ''
''                                                                          ''
''      #PX() and #PY() contain 8.8 object positions.                       ''
''      VX() and VY() contain 2.6 object velocities.                        ''
''      AX and AY contain 2.6 object acceleration for current object.       ''
''      SEL is the currently selected object.                               ''
''                                                                          ''
''  Here, 8.8 means "8 integer bits, 8 fractional bits."  Also, for Y       ''
''  coordinates, it's actually 7.8.  We ignore the MSB of the Y coord,      ''
''  since the display is only 96 rows tall.                                 ''
''                                                                          ''
''  2.6 is similar, meaning "2 integer bits, 6 fractional bits."            ''
''                                                                          ''
''  User input only modifies the acceleration of the object.  The rest is   ''
''  handled by the physics computations.                                    ''
'' ======================================================================== ''
DIM #PX(8), #PY(8)      ' Positions
DIM VX(8), VY(8)        ' Velocities
DIM AX, AY              ' Acceleration for selected object
DIM SEL                 ' Currently selected object

SIGNED VX, VY, AX, AY
UNSIGNED #PX, #PY

'' ======================================================================== ''
''  Movement smoothing:                                                     ''
''                                                                          ''
''  We accelerate/decelerate smoothly with a first-order differential       ''
''  equation.  All that really means is that we compute our new velocity    ''
''  in terms of the current velocity, plus a fraction of how far we are     ''
''  from our target velocity.                                               ''
''                                                                          ''
''     accel = (vel_target - vel_current) / smoothing_factor                ''
''     vel_current = vel_current + accel                                    ''
''                                                                          ''
''  The following two smoothing constants control the smoothing factors     ''
''  used for acceleration and deceleration. Smaller factors converge        ''
''  faster.  For efficient computation, these should be powers of 2.        ''
'' ======================================================================== ''
CONST ACCEL_SMOOTHING = $20
CONST DECEL_SMOOTHING = $10

'' ======================================================================== ''
''  Other variables and temporaries.                                        ''
'' ======================================================================== ''
DIM I                   ' Used as a loop index everywhere
DIM K                   ' Temporary used in HandleInput
DIM #V                  ' Temporary used in velocity update

'' ======================================================================== ''
''  Initialize the world.                                                   ''
'' ======================================================================== ''
WAIT
MODE 0, 0, 0, 0, 0      ' Black screen, color-stack
WAIT
SEL = 0
AX = 0
AY = 0
FOR I = 0 TO 7
    ' Middle of the screen
    #PX(I) = 80 * 256
    #PY(I) = 48 * 256

    ' Moving random directions away from center, slowly.
    VX(I) = RANDOM(16) - 8
    VY(I) = RANDOM(16) - 8
NEXT I
GOSUB UpdateObjects     ' Establish an object baseline

'' ======================================================================== ''
''  Main loop.                                                              ''
'' ======================================================================== ''
MainLoop:
    WAIT
    GOSUB HandleInput
    GOSUB UpdatePhysics
    GOSUB UpdateObjects
    GOTO MainLoop

'' ======================================================================== ''
''  HandleInput                                                             ''
''                                                                          ''
''  Keypad 1 - 8 selects an object 0 - 7.                                   ''
''  DISC accelerates the object in the requested direction.                 ''
''  Action buttons decelerate the object.                                   ''
'' ======================================================================== ''
HandleInput:    PROCEDURE
    I = CONT
    K = CONT.KEY

    ' No input at all:  Coast.
    IF I = 0 THEN
        AX = 0
        AY = 0
        RETURN
    END IF

    ' If keypad pressed, change object selection, and coast.
    IF K < 12 THEN
        IF K > 0 AND K < 9 THEN SEL = K - 1
        AX = 0
        AY = 0
        RETURN
    END IF

    ' If action button pressed, decelerate current object.
    IF HasAction(I / $20) THEN
        #V = -VX(SEL)
        #V = #V + ((DECEL_SMOOTHING - 1) XOR 2*(#V < 0)) ' Round away from 0
        AX = #V / DECEL_SMOOTHING

        #V = -VY(SEL)
        #V = #V + ((DECEL_SMOOTHING - 1) XOR 2*(#V < 0)) ' Round away from 0
        AY = #V / DECEL_SMOOTHING
        RETURN
    END IF

    ' Try to interpret the input as a DISC input.  If the disc is pressed,
    ' pick an acceleration that will eventually cause velocity to match the
    ' direction pushed.
    IF I < $20 AND DiscOK(I) THEN
        #V = DiscX(I) - VX(SEL)
        #V = #V + ((ACCEL_SMOOTHING - 1) XOR 2*(#V < 0)) ' Round away from 0
        AX = #V / ACCEL_SMOOTHING

        #V = DiscY(I) - VY(SEL)
        #V = #V + ((ACCEL_SMOOTHING - 1) XOR 2*(#V < 0)) ' Round away from 0
        AY = #V / ACCEL_SMOOTHING
    END IF
END

' HasAction should be indexed by the upper 3 bits of CONT.  Returns 1 if
' the bit-pattern corresponds to one or more action buttons being pressed.
HasAction:  DATA 0, 0, 0, 1, 0, 1, 1, 1

' DiscOK returns 1 if the corresponding bit pattern is a valid disc input.
' DiscX and DiscY are signed 8 bit acceleration values, scaled to the signed
' range (-127, 127).
DiscOK: DATA 0,1,1,1,1,0,1,0,1,1,0,0,1,0,0,0,0,1,1,1,1,0,1,0,1,1,0,0,1,0,0,0

DiscX:  DATA $0000, $0000, $007F, $0030, $0000, $0000, $0075, $0000
        DATA $FF81, $FF8B, $0000, $0000, $FFD0, $0000, $0000, $0000
        DATA $0000, $FFD0, $0075, $0059, $0030, $0000, $0059, $0000
        DATA $FF8B, $FFA7, $0000, $0000, $FFA7, $0000, $0000, $0000
DiscY:  DATA $0000, $007F, $0000, $0075, $FF81, $0000, $FFD0, $0000
        DATA $0000, $0030, $0000, $0000, $FF8B, $0000, $0000, $0000
        DATA $0000, $0075, $0030, $0059, $FF8B, $0000, $FFA7, $0000
        DATA $FFD0, $0059, $0000, $0000, $FFA7, $0000, $0000, $0000

'' ======================================================================== ''
''  UpdatePhysics                                                           ''
''                                                                          ''
''  This computes the new position and velocity given the current velocity  ''
''  and acceleration.                                                       ''
''                                                                          ''
''  AX, AY give the new acceleration input for the selected object.         ''
''  The acceleration inputs are zero for all other objects.                 ''
'' ======================================================================== ''
UpdatePhysics:  PROCEDURE
    ' Compute new velocity for selected object.
    #V = VX(SEL) + AX
    IF #V > 127 THEN #V = 127
    IF #V < -127 THEN #V = -127
    VX(SEL) = #V

    #V = VY(SEL) + AY
    IF #V > 127 THEN #V = 127
    IF #V < -127 THEN #V = -127
    VY(SEL) = #V

    ' Compute new position for all 8 objects.
    FOR I = 0 to 7
        #V = VX(I) * 4
        #PX(I) = #V + #PX(I)
        ' Stay on visible display by keeping X in [0, 168].
        IF #PX(I) >= 168*256 THEN #PX(I) = (168*256 XOR (#V > 0)) + #PX(I)

        #V = VY(I) * 4
        #PY(I) = #V + #PY(I)
        ' Stay on visible display by keeping Y in [0, 104].
        IF #PY(I) >= 104*256 THEN #PY(I) = (104*256 XOR (#V > 0)) + #PY(I)
    NEXT I
END

'' ======================================================================== ''
''  UpdateObjects                                                           ''
'' ======================================================================== ''
UpdateObjects:  PROCEDURE
    SPRITE 0, #PX(0) / 256 + $200, #PY(0) / 256 + $100, 10*8 + $1000
    SPRITE 1, #PX(1) / 256 + $200, #PY(1) / 256 + $100, 10*8 + 1
    SPRITE 2, #PX(2) / 256 + $200, #PY(2) / 256 + $100, 10*8 + 2
    SPRITE 3, #PX(3) / 256 + $200, #PY(3) / 256 + $100, 10*8 + 3
    SPRITE 4, #PX(4) / 256 + $200, #PY(4) / 256 + $100, 10*8 + 4
    SPRITE 5, #PX(5) / 256 + $200, #PY(5) / 256 + $100, 10*8 + 5
    SPRITE 6, #PX(6) / 256 + $200, #PY(6) / 256 + $100, 10*8 + 6
    SPRITE 7, #PX(7) / 256 + $200, #PY(7) / 256 + $100, 10*8 + $1007

    ' Blink the color for the selected object.
    ' Force it to white approximately twice per second.
    IF FRAME AND 16 THEN
        SPRITE SEL, #PX(SEL) / 256 + $200, #PY(SEL) / 256 + $100, 10*8 + 7
    END IF
END
