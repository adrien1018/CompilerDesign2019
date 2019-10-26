TARGET = parser
OBJECT = parser.tab.cc parser.tab.o lex.yy.c alloc.o functions.o
CC = gcc -g
LEX = flex
YACC = bison -v
YACCFLAG = -d
LIBS = -lfl 

parser: parser.tab.o alloc.o functions.o
	$(CC) -o $(TARGET) parser.tab.o alloc.o functions.o $(LIBS)

parser.tab.o: parser.tab.cc lex.yy.c alloc.o functions.c
	$(CC) -c parser.tab.cc
    
lex.yy.c: lexer3.l
	$(LEX) lexer3.l

parser.tab.cc: parser.y 
	$(YACC) $(YACCFLAG) parser.y

alloc.o: alloc.c
	$(CC) -c alloc.c
	
functions.o: functions.c
	$(CC) -c functions.c

clean:
	rm -f $(TARGET) $(OBJECT) 

