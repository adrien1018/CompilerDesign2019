TARGET = parser
INTERMEDIATE = parser.cc scanner.cc
OBJS = $(INTERMEDIATE:%.cc=%.o) main.o driver.o analysis.o codegen.o error.o instruction.o ast.o
PROFILE_OBJS = codegen.o instruction.o
LEX = flex
YACC = bison
YACCFLAGS = -v -t -d -Lc++
VERBOSE ?= 0
CODEGEN_DEBUG ?= 0
INSRGEN_DEBUG ?= 0
LEXER_DEBUG ?= 0
CXXFLAGS = -Og -Wall -std=c++17 -Wno-register -Wno-switch -DVERBOSE=$(VERBOSE) -DCODEGEN_DEBUG=$(CODEGEN_DEBUG) -DINSRGEN_DEBUG=$(INSRGEN_DEBUG) -DLEXER_DEBUG=$(LEXER_DEBUG) # -DNDEBUG

$(TARGET): $(OBJS)
	$(CXX) $(CXXFLAGS) -fprofile-arcs -ftest-coverage -o $@ $^

scanner.cc: lexer.ll parser.cc
	$(LEX) -+ -o $@ $<

parser.cc: parser.yy
	$(YACC) -o $@ $(YACCFLAGS) $<

$(OBJS): %.o: %.cc Makefile

$(PROFILE_OBJS): CXXFLAGS += -fprofile-arcs -ftest-coverage

# dependency list
analysis.o: analysis.h entry.h ast.h error.h symtab.h utils.h
ast.o: ast.h symtab.h
codegen.o: codegen.h entry.h ast.h symtab.h instruction.h utils.h analysis.h
driver.o: driver.h ast.h codegen.h analysis.h entry.h error.h symtab.h parser.cc
error.o: error.h ast.h symtab.h
gv.o: gv.h ast.h symtab.h
instruction.o: instruction.h symtab.h utils.h
main.o: driver.h ast.h symtab.h codegen.h analysis.h error.h gv.h parser.cc

clean: clean_gcov
	rm -f $(TARGET) $(OBJS) $(INTERMEDIATE) parser.hh parser.output
clean_gcov:
	rm -f *.gcda *.gcov *.out
