TARGET = parser
OBJECT = parser.tab.c parser.tab.o lex.yy.c alloc.o functions.o
CXXFLAGS = -Wall -Wno-write-strings
LEX = flex
YACC = bison -v
YACCFLAG = -d
LIBS = -lfl 

parser: parser.tab.o alloc.o functions.o
	$(CXX) $(CXXFLAGS) -o $(TARGET) parser.tab.o alloc.o functions.o $(LIBS)

parser.tab.o: parser.tab.c lex.yy.c alloc.o functions.c
	$(CXX) $(CXXFLAGS) -c parser.tab.c
    
lex.yy.c: lexer.l
	$(LEX) lexer.l

parser.tab.c: parser.y 
	$(YACC) $(YACCFLAG) parser.y

alloc.o: alloc.c
	$(CXX) $(CXXFLAGS) -c alloc.c
	
functions.o: functions.c
	$(CXX) $(CXXFLAGS) -c functions.c

clean:
	rm -f $(TARGET) $(OBJECT) 

