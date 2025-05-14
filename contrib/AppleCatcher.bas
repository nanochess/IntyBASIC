'Apple catching

REM Include useful predefined constants
INCLUDE "constants.bas"

reset:
cls
print at SCREENPOS(0,  4) color CS_RED,"Apple Catching Game"
print at SCREENPOS(0,  5) color CS_RED," v0.1"
print at SCREENPOS(0,  9) color CS_RED,"Made by Kiwi"
print at SCREENPOS(0, 10) color CS_RED,"IntyBASIC by"
print at SCREENPOS(0, 11) color CS_RED,"nanochess"
title:
IF cont1.button THEN goto start
goto title
start:
wait
define DEF00,2,basket
wait
define DEF02,2,apple
wait
define DEF04,2,worm
wait
define DEF16,15,tiles
wait
wait
wait
POKE $2C,$00
POKE $28,$04
POKE $29,$0b
POKE $2A,$01
POKE $2B,$0f
'POKE $32,$01
cls

gosub background1

'print at SCREENPOS(0,  0) color CS_BLUE,"\274\275\276\277\278\279\280\281\282\283\284\285\286\287\288\289"
print at SCREENPOS(4, 11) color CS_LIGHTBLUE + CS_ADVANCE,"\256"
'print at SCREENPOS(16, 11) color CS_PINK,"\274\274\274\274"
print at SCREENPOS(16, 11) color CS_PINK + CS_ADVANCE,"\273"


#score=0
lives=3
applecount=0
basketx=80
baskety=80
applex=RAND/2+20
appley=40
applep=0
gravity=0
time1=16
time1max=16
time2=3
time2max=3
hardness=0
wormchance=10
#univclock=0
#nextlife=250
univtick=0
worm=0
gosub writescore
gosub drawheart

gameloop1:
'POKE $2C,hardness
if COL2 AND $3 then if worm=0 then #score=#score+1:applecount=applecount+1:gosub applecaught:gosub drawapple:gosub writescore
if COL2 AND $3 then if worm=15 then gosub checkcount:applecount=0:gosub applecaught:gosub drawapple

if applecount>11 then gosub basketfull:#score=#score+9:gosub writescore
if #score>#nextlife then lives=lives+1:#nextlife=#nextlife+#nextlife:gosub drawheart
sprite 0,basketx+VISIBLE+HIT+ZOOMX2,baskety+ZOOMY2, SPR00+SPR_ORANGE
sprite 1,basketx+VISIBLE+HIT+ZOOMX2,baskety+ZOOMY2, SPR01+SPR_BROWN
sprite 2,applex+VISIBLE+HIT,appley+DOUBLEY, SPR02+SPR_PURPLE+worm

if time2=1 then appley=appley+applep

if cont1.left then if basketx>20 then basketx=basketx-3
if cont1.right then if basketx<138 then basketx=basketx+3

if time1=1 then applep=applep+1

if appley>96 then wormchance=wormchance+2:gosub applecaught

univtick=univtick+1
'if univtick=55 then POKE $2C,$0f
'if univtick=59 then POKE $2C,hardness
if wormchance>245 then wormchance=230
if #univclock=60 then gosub secretbonus
if univtick=60 then wormchance=wormchance+4:univtick=0:#univclock=#univclock+1
if lives=0 then POKE $29,$02:print at 105 color 4,"Game Over":goto end
time2=time2-1
if time2=0 then time2=time2max
time1=time1-1
if time1=0 then time1=time1max
wait
goto gameloop1


end:
IF cont1.button THEN goto reset
goto end


drawapple:procedure
wait
print at SCREENPOS(4, 11) color CS_LIGHTBLUE + CS_ADVANCE,"\256"
if applecount= 0 then print at SCREENPOS(5, 11) color CS_RED,"           "
if applecount= 1 then print at SCREENPOS(5, 11) color CS_RED,"\272          "
if applecount= 2 then print at SCREENPOS(5, 11) color CS_RED,"\272\272         "
if applecount= 3 then print at SCREENPOS(5, 11) color CS_RED,"\272\272\272        "
if applecount= 4 then print at SCREENPOS(5, 11) color CS_RED,"\272\272\272\272       "
if applecount= 5 then print at SCREENPOS(5, 11) color CS_RED,"\272\272\272\272\272      "
if applecount= 6 then #score=#score+1:print at SCREENPOS(5, 11) color CS_RED,"\272\272\272\272\272\272     "
if applecount= 7 then #score=#score+1:print at SCREENPOS(5, 11) color CS_RED,"\272\272\272\272\272\272\272    "
if applecount= 8 then #score=#score+2:print at SCREENPOS(5, 11) color CS_RED,"\272\272\272\272\272\272\272\272   "
if applecount= 9 then #score=#score+2:print at SCREENPOS(5, 11) color CS_RED,"\272\272\272\272\272\272\272\272\272  "
if applecount=10 then #score=#score+3:print at SCREENPOS(5, 11) color CS_RED,"\272\272\272\272\272\272\272\272\272\272 "
if applecount=11 then #score=#score+3:print at SCREENPOS(5, 11) color CS_RED,"\272\272\272\272\272\272\272\272\272\272\272"
return
end

applecaught: procedure
'if worm=15 then applecount=0:wait:drawapple
appley=20
applep=1
applex=RAND/2+20
wait
x=rand
if x<wormchance then worm=15 else worm=0
wait
sprite 2,applex+VISIBLE+HIT,appley+DOUBLEY, SPR02+SPR_PURPLE+worm
wait
return
end

writescore: procedure

print at (SCREENPOS(0, 11)),(#score/1000%10+16)*8+6
print at (SCREENPOS(1, 11)),(#score/100%10+16)*8+6
print at (SCREENPOS(2, 11)),(#score/10%10+16)*8+6
print at (SCREENPOS(3, 11)),(#score%10+16)*8+6

return
end


basketfull: procedure
for a=0 to 40
print at SCREENPOS(5, 11) color CS_RED,"\272\272\272\272\272\272\272\272\272\272\272"
wait
wait
print at SCREENPOS(5, 11) color CS_YELLOW," 10 POINTS "
wait
wait
next a
print at SCREENPOS(5, 11) color CS_RED,"           "
hardness=hardness+1
gosub difftier
applecount=0
return
end


secretbonus: procedure
b=CS_BLACK
for a=0 to 60
print at SCREENPOS(5, 11) color b,"  SECRET   "
b=b+1
if b>CS_WHITE then b=CS_BLACK
wait
next a
for a=0 to 60
print at SCREENPOS(5, 11) color b,"ENDURANCE "
b=b+1
if b>CS_WHITE then b=CS_BLACK
wait
next a
for a=0 to 60
print at SCREENPOS(5, 11) color b,"  BONUS    "
b=b+1
if b>CS_WHITE then b=CS_BLACK
wait
next a
for a=0 to 90
print at SCREENPOS(5, 11) color b,"100 POINTS "
b=b+1
if b>CS_WHITE then b=CS_BLACK
wait
next a
#score=#score+100
gosub writescore
print at SCREENPOS(5, 11) color CS_RED,"           "
hardness=hardness+1
gosub difftier
applecount=0
return
end
difftier: procedure
wormchance=20
#univclock=0
if hardness=0 then time1max=16:time2max=3
if hardness=1 then time1max=15:time2max=3
if hardness=2 then time1max=14:time2max=3
if hardness=3 then time1max=13:time2max=3
if hardness=4 then time1max=12:time2max=3
if hardness=5 then time1max=11:time2max=3
if hardness=6 then time1max=10:time2max=3
if hardness=7 then time1max=16:time2max=2
if hardness=8 then time1max=14:time2max=2
if hardness=9 then time1max=12:time2max=2
if hardness=10 then time1max=10:time2max=2
if hardness=11 then time1max=9:time2max=2
if hardness=12 then time1max=8:time2max=2
if hardness=13 then time1max=12:time2max=1
if hardness=14 then time1max=11:time2max=1
if hardness=15 then time1max=10:time2max=1
if hardness=16 then time1max=9:time2max=1
if hardness=17 then time1max=8:time2max=1
if hardness=18 then time1max=7:time2max=1
if hardness=19 then time1max=6:time2max=1
if hardness=20 then time1max=5:time2max=1
if hardness=21 then time1max=4:time2max=1
if hardness=22 then time1max=3:time2max=1
if hardness=23 then time1max=2:time2max=1
if hardness=24 then wormchance=30
if hardness=25 then wormchance=40
if hardness=26 then wormchance=50
if hardness=27 then wormchance=60
if hardness=28 then wormchance=70
if hardness=29 then wormchance=80
if hardness=30 then wormchance=90
if hardness=31 then wormchance=120
if hardness=32 then wormchance=150:hardness=31
wait
POKE $2C,hardness
return
end

drawheart:procedure
POKE $2B,$02
wait
if lives=1 then print at SCREENPOS(16, 11) color CS_PINK,"\274\274\274\274":print at SCREENPOS(16, 11) color CS_PINK + CS_ADVANCE,"\274"
if lives=2 then print at SCREENPOS(16, 11) color CS_PINK,"\273\274\274\274":print at SCREENPOS(16, 11) color CS_PINK + CS_ADVANCE,"\273"
if lives=3 then print at SCREENPOS(16, 11) color CS_PINK,"\273\273\274\274":print at SCREENPOS(16, 11) color CS_PINK + CS_ADVANCE,"\273"
if lives=4 then print at SCREENPOS(16, 11) color CS_PINK,"\273\273\273\274":print at SCREENPOS(16, 11) color CS_PINK + CS_ADVANCE,"\273"
if lives=5 then print at SCREENPOS(16, 11) color CS_PINK,"\273\273\273\273":print at SCREENPOS(16, 11) color CS_PINK + CS_ADVANCE,"\273"
wait
POKE $2B,$0f
return
end

checkcount:procedure
if applecount=0 then lives=lives-1:gosub drawheart
return
end

background1:procedure
for a=SCREENPOS(0, 1) to SCREENPOS(18, 1) step 2
print at a color CS_PURPLE,"\275\281"
next a

for a=SCREENPOS(0, 0) to SCREENPOS(18, 0) step 2
print at a color CS_GREEN,"\279\280"
next a

for a=SCREENPOS(0, 2) to SCREENPOS(19, 2)
print at a color CS_BLACK,"\275"
next a

for a=SCREENPOS(0, 3) to SCREENPOS(18, 3) step 2
print at a color CS_BLACK,"\277\278"
next a

for a=SCREENPOS(0, 4) to SCREENPOS(19, 4)
print at a color CS_BLACK,"\276"
next a

for a=SCREENPOS(0, 10) to SCREENPOS(19, 10)
print at a color CS_BLACK,"\286"
next a



for a=SCREENPOS(8, 3) to SCREENPOS(11, 9) step BACKGROUND_COLUMNS
print at a color CS_YELLOW,"\282\283\284\285"
next a

print at SCREENPOS(0, 4) color CS_ADVANCE,"\276"
'print at SCREENPOS(1, 3) color CS_GRAY + CS_ADVANCE,"\275"
'print at SCREENPOS(2, 3) color CS_GRAY + CS_ADVANCE,"\275"
'print at SCREENPOS(3, 3) color CS_GRAY + CS_ADVANCE,"\275"
return
end


basket:
BITMAP "########"
BITMAP ".#.#.#.#"
BITMAP "#.#.#.#."
BITMAP ".#.#.#.#"
BITMAP "#.#.#.#."
BITMAP ".#.#.#.."
BITMAP "..#.#.#."
BITMAP ".######."

BITMAP "########"
BITMAP "#.#.#.#."
BITMAP ".#.#.#.#"
BITMAP "#.#.#.#."
BITMAP ".#.#.#.#"
BITMAP "..#.#.#."
BITMAP ".#.#.#.."
BITMAP ".######."
apple:

BITMAP ".....#.."
BITMAP ".##.#..."
BITMAP "####.##."
BITMAP "########"
BITMAP "########"
BITMAP "########"
BITMAP "########"
BITMAP "########"

BITMAP "########"
BITMAP "########"
BITMAP "########"
BITMAP "########"
BITMAP "#######."
BITMAP ".######."
BITMAP ".######."
BITMAP ".##..#.."

worm:

BITMAP ".....##."
BITMAP "....###."
BITMAP "...###.."
BITMAP "..###..."
BITMAP ".###...."
BITMAP ".##....."
BITMAP ".###...."
BITMAP "..###..."

BITMAP "...###.."
BITMAP "....###."
BITMAP ".....#.."
BITMAP "...#...#"
BITMAP "....###."
BITMAP "...#.#.#"
BITMAP "...#.#.#"
BITMAP "....###."

tiles:
'apple icon
BITMAP ".....#.."
BITMAP "....#..."
BITMAP "..##.##."
BITMAP ".#######"
BITMAP ".#######"
BITMAP ".#######"
BITMAP "..#####."
BITMAP "..##.##."
'heart icon
BITMAP "........"
BITMAP ".##.##.."
BITMAP "#.##.##."
BITMAP "#######."
BITMAP ".#####.."
BITMAP "..###..."
BITMAP "...#...."
BITMAP "........"
'empty heart icon
BITMAP "........"
BITMAP ".##.##.."
BITMAP "##.##.#."
BITMAP "#.....#."
BITMAP ".#...#.."
BITMAP "..#.#..."
BITMAP "...#...."
BITMAP "........"

'background  tiles
BITMAP "........"
BITMAP "........"
BITMAP "........"
BITMAP "........"
BITMAP "........"
BITMAP "........"
BITMAP "........"
BITMAP "........"

BITMAP "########"
BITMAP "########"
BITMAP "########"
BITMAP "########"
BITMAP "########"
BITMAP "########"
BITMAP "########"
BITMAP "########"


'bottom tree left

BITMAP "........"
BITMAP "........"
BITMAP "........"
BITMAP "........"
BITMAP "........"
BITMAP "#......#"
BITMAP "##...###"
BITMAP "########"
'bottom tree right
BITMAP "........"
BITMAP "........"
BITMAP "........"
BITMAP "........"
BITMAP "#......."
BITMAP "##.....#"
BITMAP "####..##"
BITMAP "########"
'green top tree
BITMAP "########"
BITMAP "########"
BITMAP "########"
BITMAP ".#######"
BITMAP "..######"
BITMAP "....####"
BITMAP "........"
BITMAP "........"

BITMAP "########"
BITMAP "########"
BITMAP "########"
BITMAP "#######."
BITMAP "######.."
BITMAP "####...."
BITMAP "........"
BITMAP "........"
'apple tree
BITMAP "........"
BITMAP "........"
BITMAP "#...##.."
BITMAP "#######."
BITMAP "#######."
BITMAP ".######."
BITMAP "..##.#.."
BITMAP "........"
'treetrunk
BITMAP ".##....#"
BITMAP "..#...#."
BITMAP ".###...#"
BITMAP "..#...##"
BITMAP ".#.#.#.#"
BITMAP ".##...#."
BITMAP ".#.#...#"
BITMAP "..#...#."


BITMAP "#.####.."
BITMAP "#..####."
BITMAP "#..####."
BITMAP "...####."
BITMAP "#..###.."
BITMAP ".#.###.."
BITMAP "#..####."
BITMAP "...####."

BITMAP ".####.##"
BITMAP ".####.##"
BITMAP ".####..#"
BITMAP "..###.##"
BITMAP "..##..##"
BITMAP ".###...#"
BITMAP ".####..#"
BITMAP ".####.##"

BITMAP ".##...#."
BITMAP ".###.#.."
BITMAP "..#...#."
BITMAP ".#.#.##."
BITMAP ".##...#."
BITMAP ".#.#...."
BITMAP "..#...#."
BITMAP ".###.#.."


BITMAP "########"
BITMAP "#.######"
BITMAP "#.#.###."
BITMAP "#.#.#.#."
BITMAP "#...#.#."
BITMAP "....#..."
BITMAP "........"
BITMAP "........"

