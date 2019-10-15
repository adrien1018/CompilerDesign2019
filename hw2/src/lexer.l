%option noyywrap
%{
#include <fstream>
#include <algorithm>
#include "header.h"

int linenumber;
SymTab symtab;
std::vector<std::string> comments;

enum TokenType {
  // reserved words
  R_RETURN = kReservedStart,
  R_TYPEDEF,
  R_IF,
  R_ELSE,
  R_INT,
  R_FLOAT,
  R_FOR,
  R_VOID,
  R_WHILE = kReservedEnd,

  // arithmetic operators
  O_ADDITION = kOperationStart,
  O_SUBTRACTION,
  O_DIVISION,
  O_MULTIPLICATION = kOperationEnd,

  // relational operators
  O_LESS_THAN,
  O_GREATER_THAN,
  O_LESS_THAN_OR_EQ,
  O_GREATER_THAN_OR_EQ,
  O_NOT_EQ,
  O_EQ,

  // logical operators
  O_LOGICAL_OR,
  O_LOGICAL_AND,
  O_LOGICAL_NOT,

  // assignment operator
  O_ASSIGN,
  
  // constants
  C_INT = kConstantStart,
  C_FLOAT,
  C_STRING = kConstantEnd,

  // identifiers
  IDENTIFIER,

  // separators
  S_L_BRACE = kSeparatorStart,
  S_R_BRACE,
  S_L_BRACKET,
  S_R_BRACKET,
  S_L_PAREN,
  S_R_PAREN,
  S_SEMICOLON,
  S_COMMA,
  S_PERIOD = kSeparatorEnd,

  kNumTokenType
};

%}

/*** basic things ***/
LETTER                     [A-Za-z]
DIGIT                      [0-9]
WHITE_SPACE                [ \t]+
NEWLINE                    "\n"
ERROR                      .
COMMENT                    // TODO

REGEX_IDENTIFIER           {LETTER}({LETTER}|{DIGIT}|"_")*

/*** constants ***/
REGEX_C_INT                {DIGIT}+
REGEX_C_FLOAT              {DIGIT}+
REGEX_C_STRING             {DIGIT}+

/*** operators ***/
REGEX_O_ADDITION           "+"
REGEX_O_SUBTRACTION        "-"
REGEX_O_DIVISION           "/"
REGEX_O_MULTIPLICATION     "*"

REGEX_O_LESS_THAN          "<"
REGEX_O_GREATER_THAN       ">"
REGEX_O_LESS_THAN_OR_EQ    "<="
REGEX_O_GREATER_THAN_OR_EQ ">="
REGEX_O_NOT_EQ             "!="
REGEX_O_EQ                 "=="

REGEX_O_LOGICAL_OR         "||"
REGEX_O_LOGICAL_AND        "&&"
REGEX_O_LOGICAL_NOT        "!"

REGEX_O_ASSIGN             "="

/*** separators ***/
REGEX_S_L_BRACE            "{"
REGEX_S_R_BRACE            "}"
REGEX_S_L_BRACKET          "("
REGEX_S_R_BRACKET          ")"
REGEX_S_L_PAREN            "("
REGEX_S_R_PAREN            ")"
REGEX_S_SEMICOLON          ";"
REGEX_S_COMMA              ","
REGEX_S_PERIOD             "."

%%

{WHITE_SPACE}                {}

{REGEX_C_INT}                { return C_INT; }
{REGEX_C_FLOAT}              { return C_FLOAT; }
{REGEX_C_STRING}             { return C_STRING; }

{REGEX_IDENTIFIER}            {
      auto it = symtab.insert({std::string(yytext), {linenumber, 0}});
      it.first->second.counter++;
    }

{REGEX_O_ADDITION}           { return O_ADDITION; }
{REGEX_O_SUBTRACTION}        { return O_SUBTRACTION; } 
{REGEX_O_DIVISION}           { return O_DIVISION; } 
{REGEX_O_MULTIPLICATION}     { return O_MULTIPLICATION; }  

{REGEX_O_LESS_THAN}          { return O_LESS_THAN; }
{REGEX_O_GREATER_THAN}       { return O_GREATER_THAN; } 
{REGEX_O_LESS_THAN_OR_EQ}    { return O_LESS_THAN_OR_EQ; }  
{REGEX_O_GREATER_THAN_OR_EQ} { return O_GREATER_THAN_OR_EQ; }
{REGEX_O_NOT_EQ}             { return O_NOT_EQ; } 
{REGEX_O_EQ}                 { return O_EQ; } 

{REGEX_O_LOGICAL_OR}         { return O_LOGICAL_OR; } 
{REGEX_O_LOGICAL_AND}        { return O_LOGICAL_AND; } 
{REGEX_O_LOGICAL_NOT}        { return O_LOGICAL_NOT; } 

{REGEX_O_ASSIGN}             { return O_ASSIGN; }

{REGEX_S_L_BRACE}            { return S_L_BRACE; }
{REGEX_S_R_BRACE}            { return S_R_BRACE; }
{REGEX_S_L_BRACKET}          { return S_L_BRACKET; }
{REGEX_S_R_BRACKET}          { return S_R_BRACKET; }
{REGEX_S_L_PAREN}            { return S_L_PAREN; } 
{REGEX_S_R_PAREN}            { return S_R_PAREN; }
{REGEX_S_SEMICOLON}          { return S_SEMICOLON; }
{REGEX_S_COMMA}              { return S_COMMA; }
{REGEX_S_PERIOD}             { return S_PERIOD; }

{NEWLINE}                    { linenumber += 1; }
{COMMENT}       {
      comments.push_back(yytext);
      linenumber += std::count(comments.back().begin(), comments.back().end(), '\n');
    }

{ERROR}         printf("ERR \n");/* return ERROR; */


%%

int main(int argc, char **argv) {
  yyFlexLexer* lexer;
  std::ifstream input;
  if (argc > 1) {
    input.open(argv[1]);
    lexer = new yyFlexLexer(input, std::cout);
  } else {
    lexer = new yyFlexLexer();
  }
  while (lexer->yylex() != 0);
  delete lexer;
  PrintStrings(comments);
  PrintSymTab(symtab);
}

