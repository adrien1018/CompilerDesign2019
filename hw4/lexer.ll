%option noyywrap nounput noinput batch
%{
#include <algorithm>
#include <fstream>
#include <map>

#include "ast.h"
#include "driver.h"

const std::map<std::string, yy::parser::token_type> kReservedWords = {
  {"return",  yy::parser::token::R_RETURN},
  {"typedef", yy::parser::token::R_TYPEDEF},
  {"if",      yy::parser::token::R_IF},
  {"else",    yy::parser::token::R_ELSE},
  {"for",     yy::parser::token::R_FOR},
  {"while",   yy::parser::token::R_WHILE},
};

inline void Step(Location& loc, const char* yytext, int yyleng) {
  loc.end.offset += yyleng;
  size_t x = std::count(yytext, yytext + yyleng, '\n');
  if (x == 0) {
    loc.end.column += yyleng;
  } else {
    loc.end.line += x;
    int h = 1;
    for (int x = yyleng - 1; x >= 0 && yytext[x] != '\n'; x--, h++);
    loc.end.column = h;
  }
}

AstNode *MakeConstNode(const Location &loc, DataType type, const char *text) {
  AstNode *node = new AstNode(CONST_VALUE_NODE, loc);
  node->data_type = type;
  ConstValue cv;
  switch (type) {
    case INT_TYPE:
      cv = atoi(text);
      break;
    case FLOAT_TYPE:
      cv = FloatType(atof(text));
      break;
    case CONST_STRING_TYPE:
      cv = std::string(text);
      break;
  }
  node->semantic_value = cv;
  return node;
}

#ifdef DEBUG
#define RETURN_TOKEN(x)                   \
  printf("Get %s: [%s]\n", #x, YYText()); \
  return yy::parser::make_##x(loc)
#define RETURN_RESERVED(x)                       \
  printf("Get reserved word: [%s]\n", YYText()); \
  return yy::parser::symbol_type(x, loc)
#else
#define RETURN_TOKEN(x) return yy::parser::make_##x(loc)
#define RETURN_RESERVED(x) return yy::parser::symbol_type(x, loc)
#endif  // DEBUG

%}

/* --------- Regexp --------- */
/*** basic things ***/
LETTER                     [A-Za-z]
DIGIT                      [0-9]
WHITE_SPACE                [ \t]+
NEWLINE                    \n|\r|\r\n
COMMENT                    \/\*([^*]|\*[^/])*\*\/

/*** constants ***/
REGEX_C_INT                [+-]?{DIGIT}+
REGEX_C_FLOAT              [+-]?({DIGIT}+(\.{DIGIT}*)?|\.{DIGIT}+)([eE][+-]?{DIGIT}+)?
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
  #define YY_USER_ACTION Step(loc, YYText(), YYLeng());
%}

%%

%{
  Location& loc = location;
  loc.step();
%}

{WHITE_SPACE}  { loc.step(); }

{REGEX_C_INT} {
  AstNode *node = MakeConstNode(loc, INT_TYPE, YYText());
  return yy::parser::make_CONST(node, loc);
}

{REGEX_C_FLOAT} {
  AstNode *node = MakeConstNode(loc, FLOAT_TYPE, YYText());
  return yy::parser::make_CONST(node, loc);
}

{REGEX_C_STRING} {
  AstNode *node = MakeConstNode(loc, CONST_STRING_TYPE, YYText());
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

{NEWLINE}                    { loc.step(); }
{COMMENT} {
  loc.step();
#ifdef DEBUG
  printf("Get comment: [%s]\n", YYText());
#endif
}

{REGEX_IDENTIFIER} {
  std::string text = YYText();
  auto reserved_it = kReservedWords.find(text);
  if (reserved_it != kReservedWords.end()) {
    RETURN_RESERVED(reserved_it->second);
  }
#ifdef DEBUG
  printf("Get identifier: [%s]\n", YYText());
#endif
  return yy::parser::make_IDENTIFIER(YYText(), loc);
}

{ERROR} {
  throw yy::parser::syntax_error(loc,
      "invalid character: " + std::string(YYText()));
}

<<EOF>> {
  return yy::parser::make_END(loc);
}

%%
