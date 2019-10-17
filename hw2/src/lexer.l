%option noyywrap
%{
#include <fstream>
#include <algorithm>
#include "header.h"

int linenumber;
SymTab symtab;
std::vector<std::string> comments;

const int kReservedStart = 10;
const int kOperatorStart = 30;
const int kConstantStart = 50;
const int kIdentifier = 69;
const int kSeparatorStart = 70;

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
  R_WHILE,

  // arithmetic operators
  O_ADDITION = kOperatorStart,
  O_SUBTRACTION,
  O_DIVISION,
  O_MULTIPLICATION,

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
  C_STRING,

  // identifiers
  IDENTIFIER = kIdentifier,

  // separators
  S_L_BRACE = kSeparatorStart,
  S_R_BRACE,
  S_L_BRACKET,
  S_R_BRACKET,
  S_L_PAREN,
  S_R_PAREN,
  S_SEMICOLON,
  S_COMMA,
  S_PERIOD,

  ERROR,

  kNumTokenType
};

const std::map<std::string, TokenType> kReservedWords = {
  {"return",  R_RETURN},
  {"typedef", R_TYPEDEF},
  {"if",      R_IF},
  {"else",    R_ELSE},
  {"int",     R_INT},
  {"float",   R_FLOAT},
  {"for",     R_FOR},
  {"void",    R_VOID},
  {"while",   R_WHILE},
};

#ifdef DEBUG
#define RETURN_TOKEN(x) printf("Get %s: [%s]\n", #x, yytext); return x
#define RETURN_RESERVED(x) printf("Get reserved word: [%s]\n", yytext); return x
#else
#define RETURN_TOKEN(x) return x
#define RETURN_RESERVED(x) return x
#endif

%}

/*** basic things ***/
LETTER                     [A-Za-z]
DIGIT                      [0-9]
WHITE_SPACE                [ \t]+
NEWLINE                    "\n"
COMMENT                    \/\*([^*]|\*[^/])*\*\/

/*** constants ***/
REGEX_C_INT                {DIGIT}+
REGEX_C_FLOAT              ({DIGIT}+(\.{DIGIT}*)?|\.{DIGIT}+)([eE][+-]?{DIGIT}+)?
REGEX_C_STRING             \"([^\\"\n]|\\(.|\n))*\"

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
REGEX_S_L_BRACKET          "["
REGEX_S_R_BRACKET          "]"
REGEX_S_L_PAREN            "("
REGEX_S_R_PAREN            ")"
REGEX_S_SEMICOLON          ";"
REGEX_S_COMMA              ","
REGEX_S_PERIOD             "."

REGEX_IDENTIFIER           {LETTER}({DIGIT}|{LETTER}|_)*

ERROR                      .

%%

{WHITE_SPACE}                {}

{REGEX_C_INT}                { RETURN_TOKEN(C_INT); }
{REGEX_C_FLOAT}              { RETURN_TOKEN(C_FLOAT); }
{REGEX_C_STRING}             { RETURN_TOKEN(C_STRING); }

{REGEX_O_ADDITION}           { RETURN_TOKEN(O_ADDITION); }
{REGEX_O_SUBTRACTION}        { RETURN_TOKEN(O_SUBTRACTION); }
{REGEX_O_DIVISION}           { RETURN_TOKEN(O_DIVISION); }
{REGEX_O_MULTIPLICATION}     { RETURN_TOKEN(O_MULTIPLICATION); }

{REGEX_O_LESS_THAN}          { RETURN_TOKEN(O_LESS_THAN); }
{REGEX_O_GREATER_THAN}       { RETURN_TOKEN(O_GREATER_THAN); }
{REGEX_O_LESS_THAN_OR_EQ}    { RETURN_TOKEN(O_LESS_THAN_OR_EQ); }
{REGEX_O_GREATER_THAN_OR_EQ} { RETURN_TOKEN(O_GREATER_THAN_OR_EQ); }
{REGEX_O_NOT_EQ}             { RETURN_TOKEN(O_NOT_EQ); }
{REGEX_O_EQ}                 { RETURN_TOKEN(O_EQ); }

{REGEX_O_LOGICAL_OR}         { RETURN_TOKEN(O_LOGICAL_OR); }
{REGEX_O_LOGICAL_AND}        { RETURN_TOKEN(O_LOGICAL_AND); }
{REGEX_O_LOGICAL_NOT}        { RETURN_TOKEN(O_LOGICAL_NOT); }

{REGEX_O_ASSIGN}             { RETURN_TOKEN(O_ASSIGN); }

{REGEX_S_L_BRACE}            { RETURN_TOKEN(S_L_BRACE); }
{REGEX_S_R_BRACE}            { RETURN_TOKEN(S_R_BRACE); }
{REGEX_S_L_BRACKET}          { RETURN_TOKEN(S_L_BRACKET); }
{REGEX_S_R_BRACKET}          { RETURN_TOKEN(S_R_BRACKET); }
{REGEX_S_L_PAREN}            { RETURN_TOKEN(S_L_PAREN); }
{REGEX_S_R_PAREN}            { RETURN_TOKEN(S_R_PAREN); }
{REGEX_S_SEMICOLON}          { RETURN_TOKEN(S_SEMICOLON); }
{REGEX_S_COMMA}              { RETURN_TOKEN(S_COMMA); }
{REGEX_S_PERIOD}             { RETURN_TOKEN(S_PERIOD); }

{NEWLINE}                    { linenumber += 1; }
{COMMENT}                    {
      comments.push_back(yytext);
      linenumber += std::count(comments.back().begin(), comments.back().end(), '\n');
#ifdef DEBUG
      printf("Get comment: [%s]\n", yytext);
#endif
    }

{REGEX_IDENTIFIER}            {
      std::string text = yytext;
      auto reserved_it = kReservedWords.find(text);
      if (reserved_it != kReservedWords.end()) {
        RETURN_RESERVED(reserved_it->second);
      }
      auto it = symtab.insert({text, {linenumber, 0}});
      it.first->second.counter++;
#ifdef DEBUG
      printf("Get identifier: [%s]\n", yytext);
#endif
      return IDENTIFIER;
    }


{ERROR}                      { printf("ERR \n"); return ERROR; }


%%

int main(int argc, char **argv) {
  yyFlexLexer* lexer;
  std::ifstream input;
  if (argc > 1) {
    input.open(argv[1]);
    lexer = new yyFlexLexer(&input, &std::cout);
  } else {
    lexer = new yyFlexLexer();
  }
  while (lexer->yylex() != 0);
  delete lexer;
  PrintStrings(comments);
  PrintSymTab(symtab);
}

