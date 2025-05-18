OBJS= node.o microcode.o code.o IntyBASIC.o
CXXFLAGS= -O2

intybasic: $(OBJS)
	$(CXX) -o $@ $(OBJS)

node.o: node.cpp global.h microcode.h code.h node.h

microcode.o: microcode.cpp global.h microcode.h

code.o: code.cpp global.h microcode.h code.h

IntyBASIC.o: IntyBASIC.cpp global.h microcode.h code.h node.h

man: intybasic
	help2man --no-discard-stderr --name="BASIC language cross-compiler for the Intellivision" --no-info --output=$?.1 ./$?


clean:
	$(RM) $(OBJS)
