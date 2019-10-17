TARGET = scanner
OBJECT = lex.yy.o symboltable.o
CXXFLAGS = -g -Wall -O2 # -DDEBUG
LEX = flex
LEXFLAGS = -+
LIBS = -lfl

all: scanner
scanner: lex.yy.o symboltable.o
	$(CXX) -o $@ $^

$(OBJECT): %.o: %.cc

lex.yy.cc: lexer.l
	$(LEX) $(LIB) $(LEXFLAGS) lexer.l

clean:
	rm -f $(TARGET) $(OBJECT) lex.yy.cc
