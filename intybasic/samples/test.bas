	rem
	rem Test of Intybasic features
	rem by Oscar Toledo G. http://nanochess.org
	rem Jan/22/2014, last updated Jan/27/2014
	rem

	REM Include useful predefined constants
	INCLUDE "constants.bas"

main:
	cls
	a = 5
	b = 9
	if not a = $fffa then poke SCREENADDR(0, 0),$10f
	if -a = $fffb then poke SCREENADDR(1, 0),$10f
	if peek(SCREENADDR(0, 0)) = $010f then poke SCREENADDR(2, 0),$10f
	if a+1=6 then poke SCREENADDR(3, 0),$10f
	if a+b=14 then poke SCREENADDR(4, 0),$10f
	if a+(b+1)=15 then poke SCREENADDR(5, 0),$10f
	if a-1=4 then poke SCREENADDR(6, 0),$10f
	if a-b=$fffc then poke SCREENADDR(7, 0),$10f
	if a-(b+1)=$fffb then poke SCREENADDR(8, 0),$10f
	if (a or 2)=7 then poke SCREENADDR(9, 0),$10f
	if (a or b)=13 then poke SCREENADDR(10, 0),$10f
	if (a or (b+1))=15 then poke SCREENADDR(11, 0),$10f
	if (a xor 2)=7 then poke SCREENADDR(12, 0),$10f
	if (a xor b)=12 then poke SCREENADDR(13, 0),$10f
	if (a xor (b+1))=15 then poke SCREENADDR(14, 0),$10f
	if (a and 1)=1 then poke SCREENADDR(15, 0),$10f
	if (a and b)=1 then poke SCREENADDR(16, 0),$10f
	if (a and (b+1))=0 then poke SCREENADDR(17, 0),$10f
	if a=5 then poke SCREENADDR(18, 0),$10f
	if a<>b then poke SCREENADDR(19, 0),$10f
	if a<b then poke SCREENADDR(0, 1),$10f
	if a<=b then poke SCREENADDR(1, 1),$10f
	if b>a then poke SCREENADDR(2, 1),$10f
	if b>=a then poke SCREENADDR(3, 1),$10f
	if a=4+1 then poke SCREENADDR(4, 1),$10f
	if a<>3-1 then poke SCREENADDR(5, 1),$10f
	if a<8+1 then poke SCREENADDR(6, 1),$10f
	if a<=8+1 then poke SCREENADDR(7, 1),$10f
	if a>1-1 then poke SCREENADDR(8, 1),$10f
	if a>=1-1 then poke SCREENADDR(9, 1),$10f
	if a*2=10 then poke SCREENADDR(10, 1),$10f
	if a*4=20 then poke SCREENADDR(11, 1),$10f
	if a*b=45 then poke SCREENADDR(12, 1),$10f
	if a*(b+1)=50 then poke SCREENADDR(13, 1),$10f
	if a/2=2 then poke SCREENADDR(14, 1),$10f
	if a/4=1 then poke SCREENADDR(15, 1),$10f
	if 20/a=4 then poke SCREENADDR(16, 1),$10f
	if b/a=1 then poke SCREENADDR(17, 1),$10f
	if a%2=1 then poke SCREENADDR(18, 1),$10f
	if a%4=1 then poke SCREENADDR(19, 1),$10f
	for a=SCREENPOS(0, 2) to SCREENPOS(19, 11) step 3
	poke BACKTAB+a,$002f*8+7
	next a
	for a=SCREENPOS(19, 11) to SCREENPOS(1, 2) step -3
	poke BACKTAB+a,$0034*8+7
	next a
	for a=SCREENPOS(2, 2) to SCREENPOS(19, 11) step 3
	poke BACKTAB+a,$0027*8+7
	next a
	a = 5
	b = 6
	a = b
	a = a + b
	a = a + 5
	a = a + b + 5
	a = (a + b) + 3
	for a=1 to 5
	b=b+1
	next a

	a = not a
	a = -a
	a = a = b
	a = a <> b
	a = a < b
	a = a > b
	a = a <= b
	a = a >= b
	if a=b then a=9 else a=11

	restore table
loop:	read value
	if value=0 then goto end
	poke value+512,95*8+7
	goto loop

end:
	goto end

table:
	data 5*20+7,5*20+8,5*20+9,5*20+10,5*20+11
	data 6*20+7,6*20+11
	data 7*20+7
	data 7*20+11
	data 8*20+7,8*20+8,8*20+9,8*20+10,8*20+11
	data 0

sample: PROCEDURE
	asm NOP
	asm NOP
        END

