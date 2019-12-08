TARGET = parser
INTERMEDIATE = parser.cc scanner.cc
OBJS = $(INTERMEDIATE:%.cc=%.o) gv.o main.o driver.o analysis.o error.o
LEX = flex
YACC = bison
YACCFLAGS = -v -t -d -Lc++
CXXFLAGS = -O3 -g -Wall -std=c++17 -Wno-register -Wno-switch # -DNDEBUG

analysis.o: CXXFLAGS += -fprofile-arcs -ftest-coverage

$(TARGET): $(OBJS)
	$(CXX) $(CXXFLAGS) -fprofile-arcs -ftest-coverage -o $@ $^

scanner.cc: lexer.ll parser.cc
	$(LEX) -+ -o $@ $<

parser.cc: parser.yy
	$(YACC) -o $@ $(YACCFLAGS) $<

$(OBJS): %.o: %.cc

main.o: parser.cc
driver.o: parser.cc

clean:
	rm -f $(TARGET) $(OBJS) $(INTERMEDIATE) parser.hh parser.output *.gcno *.gcda *.gcov
clean_gcov:
	rm -f *.gcda *.gcov
