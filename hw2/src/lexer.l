%option noyywrap
%{
#include <fstream>
#include <algorithm>
#include "header.h"

int linenumber;
SymTab symtab;
std::vector<std::string> comments;

/* You need to define for all tokens in C--, here are some examples */
#define INT       10
#define FLOAT     11
#define OP_ASSIGN 12
#define OP_OR     13
#define MK_LPAREN 14
#define MK_RPAREN 15
#define ERROR     100

%}

comment \/\*([^/*]+\/*\**)*\*\/
letter   [A-Za-z]
digit    [0-9]
ID   {letter}({letter}|{digit}|"_")*
WS   [ \t]+
Int_constant {digit}+
RWInt {digit}+
RWFloat {digit}+

/* You need to define the following RE's
Float_constant
String_constant
comment
*/

OP_assign        "="
OP_or   "||"

/* Other operators appear here */


newline  "\n"

DL_lparen "("
DL_rparen ")"
DL_lbrace "{"
DL_rbrace "}"
DL_comma  ","
DL_semicol ";"
DL_dot  "."

/* Other separators appear here */

error    .

%%

{WS}            {}
{RWInt}         {/* return INT; */ }
{RWFloat}       {/* return FLOAT; */}
{ID}            {
      auto it = symtab.insert({std::string(yytext), {linenumber, 0}});
      it.first->second.counter++;
    }

{OP_assign}     /* return OP_ASSIGN; */ 
{OP_or}         /* return OP_OR; */
{DL_lparen}     /* return MK_LPAREN; */
{DL_rparen}     /* return MK_RPAREN; */
{DL_lbrace}     /* return MK_LBRACE; */
{DL_rbrace}     /* return MK_RBRACE; */
{DL_comma}      {}
{DL_semicol}    {}
{newline}       linenumber += 1;
{comment}       {
      comments.push_back(yytext);
      linenumber += std::count(comments.back().begin(), comments.back().end(), '\n');
    }
{error}         printf("ERR \n");/* return ERROR; */


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

