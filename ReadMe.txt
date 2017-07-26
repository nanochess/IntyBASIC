IntyBASIC compiler v1.2.9
by Oscar Toledo Gutierrez
http://nanochess.org/

BASIC language cross-compiler for the Intellivision.

IntyBASIC.cpp   The IntyBASIC compiler C++ language source code
LICENSE         Source code license

intybasic_prologue.asm    Prologue file needed for compiled programs
intybasic_epilogue.asm    Epilogue file needed for compiled programs

IntyBASIC.m    A version of IntyBASIC written in Objective-C
               (not maintained, I wrote it for learning purposes)

manual.txt     Manual for IntyBASIC
manual_es.txt  Spanish manual for IntyBASIC

ReadMe.txt     This file.

>>>>>>>>>>>>>>  Usage guide

    intybasic [--jlp] [--cc3] [--title \"title\"] infile.bas outfile.asm [library_path]

    --jlp       Enables use of 8K-words extra memory feature of JLP
                and also usage of hardware acceleration for
                multiplication and division.
    --cc3       Enables use of 8K-words extra memory feature of
                Cuttle Cart 3.
    --title "a" Selects title of the compiled program.
                By default this is "IntyBASIC program".
                Only appears in emulators/multicarts.
    -w          Disable warnings globally (has priority over
                OPTION WARNINGS)

    The library path is where the intybasic_prologue.asm and
    intybasic_epilogue.asm files are searched for inclusion.

    It will return a zero error code if compilation was successful and
    non-zero otherwise.

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
