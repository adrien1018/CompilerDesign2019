%option noyywrap nounput noinput batch debug
%{
#include <map>
#include <fstream>
#include <algorithm>
#include "header.h"
#include "driver.h"

const std::map<std::string, yy::parser::token_type> kReservedWords = {
  {"return",  yy::parser::token::R_RETURN},
  {"typedef", yy::parser::token::R_TYPEDEF},
  {"if",      yy::parser::token::R_IF},
  {"else",    yy::parser::token::R_ELSE},
  {"for",     yy::parser::token::R_FOR},
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

/* --------- Regexp --------- */
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

/* --------- End regexp --------- */

%{
  #define YY_USER_ACTION loc.columns(yyleng);
%}

%%

%{
  yy::location& loc = drv.location;
  loc.step();
%}

{WHITE_SPACE}  { loc.step(); }

{REGEX_C_INT} {
  AstNode *node = new AstNode(CONST_VALUE_NODE);
  node->semantic_value.const1 = new ConstType();
  node->semantic_value.const1->const_type = INTEGERC;
  node->semantic_value.const1->const_u.intval = atoi(yytext);
  node->data_type = INT_TYPE;
  return yy::parser::make_CONST(node, loc);
}

{REGEX_C_FLOAT} {
  AstNode *node = new AstNode(CONST_VALUE_NODE);
  node->semantic_value.const1 = new ConstType();
  node->semantic_value.const1->const_type = FLOATC;
  node->semantic_value.const1->const_u.fval = atof(yytext);
  node->data_type = FLOAT_TYPE;
  return yy::parser::make_CONST(node, loc);
}

{REGEX_C_STRING} {
  AstNode *node = new AstNode(CONST_VALUE_NODE);
  node->semantic_value.const1 = new ConstType();
  node->semantic_value.const1->const_type = STRINGC;
  node->semantic_value.const1->const_u.sc = strdup(yytext);
  node->data_type = CONST_STRING_TYPE;
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

{NEWLINE}                    { loc.lines(1); loc.step(); }
{COMMENT} {
  loc.lines(std::count(yytext, yytext + yyleng, '\n'));
  int h = 0;
  for (int x = yyleng - 1; x >= 0 && yytext[x] != '\n'; x--, h++);
  loc.columns(h);
#ifdef DEBUG
  printf("Get comment: [%s]\n", yytext);
#endif
}

{REGEX_IDENTIFIER} {
  std::string text = yytext;
  auto reserved_it = kReservedWords.find(text);
  if (reserved_it != kReservedWords.end()) {
    RETURN_RESERVED(reserved_it->second);
  }
#ifdef DEBUG
  printf("Get identifier: [%s]\n", yytext);
#endif
  return yy::parser::make_IDENTIFIER(yytext, loc);
}

{ERROR} {
  throw yy::parser::syntax_error(loc,
      "invalid character: " + std::string(yytext));
}

<<EOF>> {
  return yy::parser::make_END(loc);
}

%%

void Driver::scan_begin() {
  if (file.empty () || file == "-") {
    yyin = stdin;
  } else if (!(yyin = fopen(file.c_str(), "r"))) {
    std::cerr << "cannot open " << file << ": " << strerror(errno) << '\n';
    exit(EXIT_FAILURE);
  }
}

void Driver::scan_end() { fclose(yyin); }
