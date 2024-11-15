IntyBASIC compiler v1.5.0
(c) Copyright 2014-2020 Óscar Toledo Gutiérrez
http://nanochess.org/

BASIC language cross-compiler for the Intellivision.

IntyBASIC.cpp   The IntyBASIC compiler C++ language source code
LICENSE         Source code license

intybasic_prologue.asm    Prologue file needed for compiled programs
intybasic_epilogue.asm    Epilogue file needed for compiled programs

IntyBASIC.m    A version of IntyBASIC written in Objective-C
               (not maintained, I wrote it for learning purposes)

manual.txt     English manual for IntyBASIC
manual_es.txt  Spanish manual for IntyBASIC

ReadMe.txt     This file.


>>>>>>>>>>>>>>  Usage guide

    intybasic [--jlp] [--cc3] [--title \"title\"] infile.bas outfile.asm [library_path]

    --jlp        Enables use of 8K-words extra memory feature of JLP
                 and also usage of hardware acceleration for
                 multiplication and division.
    --cc3        Enables use of 8K-words extra memory feature of
                 Cuttle Cart 3.
    --cc3 0xc000 Enables Cuttle Cart 3 in selected page, useful
                 with Keyboard Component.
    --title "a"  Selects title of the compiled program.
                 By default this is "IntyBASIC program".
                 Only appears in emulators/multicarts.
    -w           Disable warnings globally (has priority over
                 OPTION WARNINGS)

    The library path is where the intybasic_prologue.asm and
    intybasic_epilogue.asm files are searched for inclusion.

    It will return a zero error code if compilation was successful and
    non-zero otherwise.


>>>>>>>>>>>>>>  Notes

The current official version is v1.4.2, and the latest bleeding-edge version
is v1.5.0 that corrects bugs and is in testing state.

Also look for the swiss-knife companion tool IntyColor for converting BMP
images to IntyBASIC data. Located at:

    https://github.com/nanochess/IntyColor


>>>>>>>>>>>>>>  Acknowledgments

Thanks to following members of Atariage for contributing valuable suggestions,
test programs and even support libraries:

  Albert
  artrag
  atari2600land
  awhite2600
  carlsson
  catsfolly
  ckblackm
  CrazyBoss
  Cybearg
  DZ-Jay
  First Spear
  freewheel
  GroovyBee
  intvnut
  Jess Ragan
  Kiwi
  mmarrero
  RevEng
  SpiceWare
  Tarzilla


>>>>>>>>>>>>>>  Supporting the developer

If you find IntyBASIC useful, please show your appreciation making a donation
via Paypal ($9 USD suggested) to b-i+y-u+b-i (at) gmail.com

If you find a bug, please report to same email and I'll try to look into
it. Because lack of time I cannot guarantee it will be corrected.

Also you can get the book "Programming Games for Intellivision" that is a book
version of the manual, and also including an introductory course to game
programming with full examples and source code: Game of Ball, Monkey Moon,
Space Raider & Bouncy Cube.

Paperback:
    https://www.lulu.com/en/en/shop/oscar-toledo-gutierrez/programming-games-for-intellivision/paperback/product-1p7mvg87.html

Hardcover:
    https://www.lulu.com/en/en/shop/oscar-toledo-gutierrez/programming-games-for-intellivision/hardcover/product-16ye5m5n.html

Ebook:
    https://nanochess.org/store.html
