%require "3.2"
%language "c++"
%{
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cstdarg>
#include <vector>

#include "header.h"

int linenumber = 1;
AstNode *prog;

extern int g_anyErrorOccur;

namespace {

inline AstNode* MakeSibling(AstNode * a, AstNode * b) {
  while (a->rightSibling) {
    a = a->rightSibling;
  }
  if (b == NULL) {
    return a;
  }
  b = b->leftmostSibling;
  a->rightSibling = b;

  b->leftmostSibling = a->leftmostSibling;
  b->parent = a->parent;
  while (b->rightSibling) {
    b = b->rightSibling;
    b->leftmostSibling = a->leftmostSibling;
    b->parent = a->parent;
  }
  return b;
}

inline AstNode* MakeChild(AstNode * parent, AstNode * child) {
  if (child == NULL) {
    return parent;
  }
  if (parent->child) {
    MakeSibling(parent->child, child);
  } else {
    child = child->leftmostSibling;
    parent->child = child;
    while (child) {
      child->parent = parent;
      child = child->rightSibling;
    }
  }
  return parent;
}

AstNode* MakeFamily(AstNode * parent, const std::vector<AstNode *> &children) {
  AstNode *child = children[0];
  MakeChild(parent, child);
  AstNode *tmp = child;
  for (size_t i = 1; i < children.size(); ++i) {
    child = children[i];
    tmp = MakeSibling(tmp, child);
  }
  return parent;
}

inline AstNode* MakeIDNode(char* lexeme, IDENTIFIER_KIND idKind) {
  AstNode* identifier = Allocate(IDENTIFIER_NODE);
  identifier->semantic_value.identifierSemanticValue.identifierName = lexeme;
  identifier->semantic_value.identifierSemanticValue.kind = idKind;
  identifier->semantic_value.identifierSemanticValue.symbolTableEntry = NULL;
  return identifier;
}

inline AstNode* MakeStmtNode(STMT_KIND stmtKind) {
  AstNode* stmtNode = Allocate(STMT_NODE);
  stmtNode->semantic_value.stmtSemanticValue.kind = stmtKind;
  return stmtNode;
}

inline AstNode* MakeDeclNode(DECL_KIND declKind) {
  AstNode* declNode = Allocate(DECLARATION_NODE);
  declNode->semantic_value.declSemanticValue.kind = declKind;
  return declNode;
}

inline AstNode* MakeExprNode(EXPR_KIND exprKind,
                                     int operationEnumValue) {
  AstNode* exprNode = Allocate(EXPR_NODE);
  exprNode->semantic_value.exprSemanticValue.isConstEval = 0;
  exprNode->semantic_value.exprSemanticValue.kind = exprKind;
  if (exprKind == BINARY_OPERATION) {
    exprNode->semantic_value.exprSemanticValue.op.binaryOp = operationEnumValue;
  } else if (exprKind == UNARY_OPERATION) {
    exprNode->semantic_value.exprSemanticValue.op.unaryOp = operationEnumValue;
  } else {
    printf(
        "Error in static inline AstNode* MakeExprNode(EXPR_KIND exprKind, int "
        "operationEnumValue)\n");
  }
  return exprNode;
}

}  // namespace

%}

%union {
  char *lexeme;
  CON_Type *const1;
  AstNode *node;
};

%token <lexeme> IDENTIFIER
%token <const1> CONST

%token R_RETURN
%token R_TYPEDEF
%token R_IF
%token R_ELSE
%token R_INT
%token R_FLOAT
%token R_FOR
%token R_VOID
%token R_WHILE

%token O_ADDITION
%token O_SUBTRACTION
%token O_DIVISION
%token O_MULTIPLICATION

%token O_LESS_THAN
%token O_GREATER_THAN
%token O_LESS_THAN_OR_EQ
%token O_GREATER_THAN_OR_EQ
%token O_NOT_EQ
%token O_EQ

%token O_LOGICAL_OR
%token O_LOGICAL_AND
%token O_LOGICAL_NOT

%token O_ASSIGN

%token C_INT
%token C_FLOAT
%token C_STRING

%token S_L_BRACE
%token S_R_BRACE
%token S_L_BRACKET
%token S_R_BRACKET
%token S_L_PAREN
%token S_R_PAREN
%token S_SEMICOLON
%token S_COMMA
%token S_PERIOD

%type <node> program global_decl_list global_decl function_decl block stmt_list decl_list decl var_decl type init_id_list init_id  stmt relop_expr relop_term relop_factor expr term factor var_ref
%type <node> param_list param dim_fn expr_null id_list dim_decl cexpr mcexpr cfactor assign_expr_list assign_expr rel_op relop_expr_list nonempty_relop_expr_list
%type <node> add_op mul_op dim_list type_decl nonempty_assign_expr_list

%start program

%%

program: global_decl_list { $$ = Allocate(PROGRAM_NODE); MakeChild($$, $1); prog = $$; }
       | { $$ = Allocate(PROGRAM_NODE); prog = $$; }
       ;

global_decl_list: global_decl_list global_decl { $$ = MakeSibling($1, $2); }
                | global_decl { $$ = $1; }
                ;

global_decl: decl_list function_decl { 
               $$ = MakeSibling(MakeChild(Allocate(VARIABLE_DECL_LIST_NODE), $1), $2); 
             }
           | function_decl { $$ = $1; }
           ;

function_decl: type IDENTIFIER S_L_PAREN param_list S_R_PAREN S_L_BRACE block S_R_BRACE {
                 $$ = MakeDeclNode(FUNCTION_DECL);
                 AstNode *param = Allocate(PARAM_LIST_NODE);
                 MakeChild(param, $4);
                 MakeFamily($$, {$1, MakeIDNode($2, NORMAL_ID), param, $7});
               }
             | R_VOID IDENTIFIER S_L_PAREN param_list S_R_PAREN S_L_BRACE block S_R_BRACE { /* TODO */ }
             | type IDENTIFIER S_L_PAREN S_R_PAREN S_L_BRACE block S_R_BRACE {
                 $$ = MakeDeclNode(FUNCTION_DECL);
                 AstNode *empty_param = Allocate(PARAM_LIST_NODE);
                 MakeFamily($$, {$1, MakeIDNode($2, NORMAL_ID), empty_param, $6});
               }
             | R_VOID IDENTIFIER S_L_PAREN S_R_PAREN S_L_BRACE block S_R_BRACE { /* TODO */ }
             ;

param_list: param_list S_COMMA param { $$ = MakeSibling($1, $3); }
          | param { /* TODO */ }
          ;

param: type IDENTIFIER {
         $$ = MakeDeclNode(FUNCTION_PARAMETER_DECL);
         MakeFamily($$, {$1, MakeIDNode($2, NORMAL_ID)});
       }
     | type IDENTIFIER dim_fn { /* TODO */ }
     ;

dim_fn: S_L_BRACKET expr_null S_R_BRACKET { $$ = $2; }
      | dim_fn S_L_BRACKET expr S_R_BRACKET { $$ = MakeSibling($1, $3); }
      ;

expr_null: expr { /* TODO */ }
         | { $$ = Allocate(NULL_NODE); }
         ;

block: decl_list stmt_list { /* TODO */ }
     | stmt_list {
         $$ = Allocate(BLOCK_NODE);
         MakeChild($$, MakeChild(Allocate(STMT_LIST_NODE), $1));
       }
     | decl_list {
         $$ = Allocate(BLOCK_NODE);
         MakeChild($$, MakeChild(Allocate(VARIABLE_DECL_LIST_NODE), $1));
       }
     | { /* TODO */ }
     ;

decl_list: decl_list decl { /* TODO */ }
         | decl { /* TODO */ }
         ;

decl: type_decl { $$ = $1; }
    | var_decl { $$ = $1; }
    ;

type_decl: R_TYPEDEF type id_list S_SEMICOLON { /* TODO */ }
         | R_TYPEDEF R_VOID id_list S_SEMICOLON { /* TODO */ }
         ;

var_decl: type init_id_list S_SEMICOLON { /* TODO */ }
        | IDENTIFIER id_list S_SEMICOLON { /* TODO */ }
        ;

type: R_INT { $$ = MakeIDNode("int", NORMAL_ID); }
    | R_FLOAT { $$ = MakeIDNode("float", NORMAL_ID); }
    ;

id_list: IDENTIFIER { $$ = MakeIDNode($1, NORMAL_ID); }
       | id_list S_COMMA IDENTIFIER { /* TODO */ }
       | id_list S_COMMA IDENTIFIER dim_decl { /* TODO */ }
       | IDENTIFIER dim_decl { /* TODO */ }
       ;

dim_decl: S_L_BRACKET cexpr S_R_BRACKET { /* TODO */ }
        /* TODO
        |
        */
        ;

cexpr: cexpr O_ADDITION mcexpr {
         $$ = MakeExprNode(BINARY_OPERATION, BINARY_OP_ADD);
         MakeFamily($$, {$1, $3});
       }
     | cexpr O_SUBTRACTION mcexpr { /* TODO */ }
     | mcexpr { /* TODO */ }
     ;

mcexpr: mcexpr O_MULTIPLICATION cfactor { /* TODO */ }
      | mcexpr O_DIVISION cfactor { /* TODO */ }
      | cfactor { /* TODO */ }
      ;

cfactor: CONST { /* TODO */ }
       | S_L_PAREN cexpr S_R_PAREN { /* TODO */ }
       ;

init_id_list: init_id { /* TODO */ }
            | init_id_list S_COMMA init_id { /* TODO */ }
            ;

init_id: IDENTIFIER { $$ = MakeIDNode($1, NORMAL_ID); }
       | IDENTIFIER dim_decl { /* TODO */ }
       | IDENTIFIER O_ASSIGN relop_expr { /* TODO */ }
       ;

stmt_list: stmt_list stmt { /* TODO */ }
         | stmt { /* TODO */ }
         ;

stmt: S_L_BRACE block S_R_BRACE { /* TODO */ }
    /* TODO: while statement */
    | R_FOR S_L_PAREN assign_expr_list S_SEMICOLON relop_expr_list S_SEMICOLON assign_expr_list S_R_PAREN stmt { /* TODO */ }
    | var_ref O_ASSIGN relop_expr S_SEMICOLON { /* TODO */ }
    /* TODO: if statement */
    /* TODO: if then else */
    /* TODO: function call */
    | S_SEMICOLON { /* TODO */ }
    | R_RETURN S_SEMICOLON { /* TODO */ }
    | R_RETURN relop_expr S_SEMICOLON { /* TODO */ }
    ;

assign_expr_list: nonempty_assign_expr_list { /* TODO */ }
                | { $$ = Allocate(NULL_NODE); }
                ;

nonempty_assign_expr_list: nonempty_assign_expr_list S_COMMA assign_expr { /* TODO */ }
                         | assign_expr { /* TODO */ }
                         ;

assign_expr: IDENTIFIER O_ASSIGN relop_expr { /* TODO */ }
           | relop_expr { /* TODO */ }
           ;

relop_expr: relop_term { $$ = $1; }
          | relop_expr O_LOGICAL_OR relop_term {
              $$ = MakeExprNode(BINARY_OPERATION, BINARY_OP_OR);
              MakeFamily($$, {$1, $3});
            }
          ;

relop_term: relop_factor { /* TODO */ }
          | relop_term O_LOGICAL_AND relop_factor { /* TODO */ }
          ;

relop_factor: expr { /* TODO */ }
            | expr rel_op expr { /* TODO */ }
            ;

rel_op: O_EQ { /* TODO */ }
      | O_GREATER_THAN_OR_EQ { /* TODO */ }
      | O_LESS_THAN_OR_EQ { /* TODO */ }
      | O_NOT_EQ { /* TODO */ }
      | O_GREATER_THAN { /* TODO */ }
      | O_LESS_THAN { /* TODO */ }
      ;

relop_expr_list: nonempty_relop_expr_list { /* TODO */ }
               | { $$ = Allocate(NULL_NODE); }
               ;

nonempty_relop_expr_list: nonempty_relop_expr_list S_COMMA relop_expr { /* TODO */ }
                        | relop_expr { /* TODO */ }
                        ;

expr: expr add_op term { /* TODO */ }
    | term { /* TODO */ }
    ;

add_op: O_ADDITION { $$ = MakeExprNode(BINARY_OPERATION, BINARY_OP_ADD); }
      | O_SUBTRACTION { $$ = MakeExprNode(BINARY_OPERATION, BINARY_OP_SUB); }
      ;

term: term mul_op factor { /* TODO */ }
    | factor { /* TODO */ }
    ;

mul_op: O_MULTIPLICATION { /* TODO */ }
      | O_DIVISION { /* TODO */ }
      ;

factor: S_L_PAREN relop_expr S_R_PAREN { /* TODO */ }
      /* TODO: | -(<relop_expr>) e.g. -(4) */
      | O_LOGICAL_NOT S_L_PAREN relop_expr S_R_PAREN { /* TODO */ }
      | CONST {
          $$ = Allocate(CONST_VALUE_NODE);
          $$->semantic_value.const1 = $1;
        }
      /* TODO: | -<constant> e.g., -4 */
      | O_LOGICAL_NOT CONST { /* TODO */ }
      | IDENTIFIER S_L_PAREN relop_expr_list S_R_PAREN { /* TODO */ }
      /* TODO: | -<function call> e.g. -f(4) */
      | O_LOGICAL_NOT IDENTIFIER S_L_PAREN relop_expr_list S_R_PAREN { /* TODO */ }
      | var_ref { /* TODO */ }
      /* TODO: | -<var_ref> e.g. -var */
      | O_LOGICAL_NOT var_ref { /* TODO */ }
      ;

var_ref: IDENTIFIER { /* TODO */ }
       | IDENTIFIER dim_list { /* TODO */ }
       ;

dim_list: dim_list S_L_BRACKET expr S_R_BRACKET { /* TODO */ }
        | S_L_BRACKET expr S_R_BRACKET { /* TODO */ }
        ;

%%

#include "lex.yy.c"

int main(int argc, char* argv[]) {
  yyin = fopen(argv[1], "r");
  yyparse();
  printf("%s\n", "Parsing completed. No errors found.");
  printGV(prog, NULL);
}

int yyerror(char* mesg) {
  printf("%s\t%d\t%s\t%s\n", "Error found in Line ", linenumber,
         "next token: ", yytext);
  exit(1);
}
