TARGET = parser
INTERMEDIATE = parser.cc scanner.cc
OBJS = $(INTERMEDIATE:%.cc=%.o) gv.o main.o driver.o analysis.o
LEX = flex
YACC = bison
YACCFLAGS = -v -t -d -Lc++
CXXFLAGS = -O3 -Wall -std=c++17 -Wno-register -Wno-switch

$(TARGET): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $^

scanner.cc: lexer.ll parser.cc
	$(LEX) -+ -o $@ $<

parser.cc: parser.yy
	$(YACC) -o $@ $(YACCFLAGS) $<

$(OBJS): %.o: %.cc

main.o: parser.cc
driver.o: parser.cc

clean:
	rm -f $(TARGET) $(OBJS) $(INTERMEDIATE) parser.hh parser.output
