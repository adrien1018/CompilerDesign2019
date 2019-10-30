TARGET = parser
INTERMEDIATE = parser.cc scanner.cc
OBJS = $(INTERMEDIATE:%.cc=%.o) alloc.o functions.o main.o
LEX = flex
YACC = bison -v -Lc++
YACCFLAGS = -d
CXXFLAGS = -O3 -Wall

$(TARGET): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $^

scanner.cc: lexer.ll parser.cc
	$(LEX) -o $@ $<

parser.cc: parser.yy
	$(YACC) -o $@ $(YACCFLAGS) $<

$(OBJS): %.o: %.cc

main.o: parser.cc

clean:
	rm -f $(TARGET) $(OBJS) $(INTERMEDIATE) parser.hh parser.output location.hh
