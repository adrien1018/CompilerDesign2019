%option noyywrap nounput noinput batch debug
%{
#include <fstream>
#include <algorithm>
#include "header.h"
#include "header_lex.h"
#include "driver.h"

//int linenumber;
SymTab symtab;
std::vector<std::string> comments;

const int kReservedStart = 10;
const int kOperatorStart = 30;
const int kConstantStart = 50;
const int kIdentifier = 69;
const int kSeparatorStart = 70;

//enum TokenType {
//  // reserved words
//  R_RETURN = kReservedStart,
//  R_TYPEDEF,
//  R_IF,
//  R_ELSE,
//  R_INT,
//  R_FLOAT,
//  R_FOR,
//  R_VOID,
//  R_WHILE,
//
//  // arithmetic operators
//  O_ADDITION = kOperatorStart,
//  O_SUBTRACTION,
//  O_DIVISION,
//  O_MULTIPLICATION,
//
//  // relational operators
//  O_LESS_THAN,
//  O_GREATER_THAN,
//  O_LESS_THAN_OR_EQ,
//  O_GREATER_THAN_OR_EQ,
//  O_NOT_EQ,
//  O_EQ,
//
//  // logical operators
//  O_LOGICAL_OR,
//  O_LOGICAL_AND,
//  O_LOGICAL_NOT,
//
//  // assignment operator
//  O_ASSIGN,
//
//  // constants
//  C_INT = kConstantStart,
//  C_FLOAT,
//  C_STRING,
//
//  // identifiers
//  IDENTIFIER = kIdentifier,
//
//  // separators
//  S_L_BRACE = kSeparatorStart,
//  S_R_BRACE,
//  S_L_BRACKET,
//  S_R_BRACKET,
//  S_L_PAREN,
//  S_R_PAREN,
//  S_SEMICOLON,
//  S_COMMA,
//  S_PERIOD,
//
//  ERROR,
//
//  kNumTokenType
//};

const std::map<std::string, yy::parser::token_type> kReservedWords = {
  {"return",  yy::parser::token::R_RETURN},
  {"typedef", yy::parser::token::R_TYPEDEF},
  {"if",      yy::parser::token::R_IF},
  {"else",    yy::parser::token::R_ELSE},
  {"int",     yy::parser::token::R_INT},
  {"float",   yy::parser::token::R_FLOAT},
  {"for",     yy::parser::token::R_FOR},
  {"void",    yy::parser::token::R_VOID},
  {"while",   yy::parser::token::R_WHILE},
};

#ifdef DEBUG
#define RETURN_TOKEN(x) printf("Get %s: [%s]\n", #x, yytext); return yy::parser::make_##x(loc)
#define RETURN_RESERVED(x) printf("Get reserved word: [%s]\n", yytext); return yy::parser::make##x(loc)
#else
#define RETURN_TOKEN(x) return yy::parser::make_##x(loc)
#define RETURN_RESERVED(x) return yy::parser::symbol_type(x, loc)
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

%{
  yy::location& loc = drv.location;
  loc.step();
%}

{WHITE_SPACE}                { loc.step(); }

{REGEX_C_INT}                { 
                               AstNode *node = Allocate(CONST_VALUE_NODE);
                               ConstType *p = new ConstType();
                               p->const_type = INTEGERC;
                               std::cerr << "REGEX_C_INT: " << yytext << std::endl;
                               p->const_u.intval = atoi(yytext);
                               node->semantic_value.const1 = p;
                               return yy::parser::make_CONST(node, loc);
                             }
{REGEX_C_FLOAT}              {
                               AstNode *node = Allocate(CONST_VALUE_NODE);
                               ConstType *p = new ConstType();
                               p->const_type = FLOATC;
                               p->const_u.fval = atof(yytext);
                               node->semantic_value.const1 = p;
                               return yy::parser::make_CONST(node, loc);
                             }
{REGEX_C_STRING}             { 
                               AstNode *node = Allocate(CONST_VALUE_NODE);
                               ConstType *p = new ConstType();
                               p->const_type = STRINGC;
                               p->const_u.sc = strdup(yytext);
                               node->semantic_value.const1 = p;
                               return yy::parser::make_CONST(node, loc);
                             }

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

{NEWLINE}                    { /*linenumber += 1;*/ loc.lines(yyleng); }
{COMMENT}                    {
      comments.push_back(yytext);
      //linenumber += std::count(comments.back().begin(), comments.back().end(), '\n');
      loc.lines(yyleng);
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
      auto it = symtab.insert({text, {yyleng, 0}});
      it.first->second.counter++;
#ifdef DEBUG
      printf("Get identifier: [%s]\n", yytext);
#endif
      return yy::parser::make_IDENTIFIER(yytext, loc);
    }


{ERROR}                      {
      throw yy::parser::syntax_error(loc,
          "invalid character: " + std::string(yytext));
    }

<<EOF>>    return yy::parser::make_END(loc);

%%

//int yyerror(char* mesg) {
//  printf("%s\t%d\t%s\t%s\n", "Error found in Line ", linenumber,
//          "next token: ", yytext);
//  exit(1);
//}

void Driver::scan_begin() {
  if (file.empty () || file == "-") {
    yyin = stdin;
  } else if (!(yyin = fopen(file.c_str(), "r"))) {
    std::cerr << "cannot open " << file << ": " << strerror(errno) << '\n';
    exit(EXIT_FAILURE);
  }
}

void Driver::scan_end() { fclose(yyin); }
