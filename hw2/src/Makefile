TARGET = scanner
OBJECT = lex.yy.o symboltable.o
CC = g++ -g
LEX = flex
LEXFLAGS = -+
LIBS = -lfl

scanner:	lex.yy.o symboltable.o
	$(CC) -o scanner lex.yy.o symboltable.o

symboltable.o: symboltable.cc
	$(CC) -c symboltable.cc

lex.yy.o: lex.yy.cc
	$(CC) -c lex.yy.cc

lex.yy.cc: lexer.l
	$(LEX) $(LIB) $(LEXFLAGS) lexer.l

clean:
	rm -f $(TARGET) $(OBJECT) lex.yy.cc
