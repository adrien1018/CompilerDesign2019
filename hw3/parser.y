%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "header.h"

int linenumber = 1;
AST_NODE *prog;

extern int g_anyErrorOccur;

static inline AST_NODE* makeSibling(AST_NODE * a, AST_NODE * b) {
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

static inline AST_NODE* makeChild(AST_NODE * parent, AST_NODE * child) {
  if (child == NULL) {
    return parent;
  }
  if (parent->child) {
    makeSibling(parent->child, child);
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

static AST_NODE* makeFamily(AST_NODE * parent, int childrenCount, ...) {
  va_list childrenList;
  va_start(childrenList, childrenCount);
  AST_NODE* child = va_arg(childrenList, AST_NODE*);
  makeChild(parent, child);
  AST_NODE* tmp = child;
  int index = 1;
  for (index = 1; index < childrenCount; ++index) {
    child = va_arg(childrenList, AST_NODE*);
    tmp = makeSibling(tmp, child);
  }
  va_end(childrenList);
  return parent;
}

static inline AST_NODE* makeIDNode(char* lexeme, IDENTIFIER_KIND idKind) {
  AST_NODE* identifier = Allocate(IDENTIFIER_NODE);
  identifier->semantic_value.identifierSemanticValue.identifierName = lexeme;
  identifier->semantic_value.identifierSemanticValue.kind = idKind;
  identifier->semantic_value.identifierSemanticValue.symbolTableEntry = NULL;
  return identifier;
}

static inline AST_NODE* makeStmtNode(STMT_KIND stmtKind) {
  AST_NODE* stmtNode = Allocate(STMT_NODE);
  stmtNode->semantic_value.stmtSemanticValue.kind = stmtKind;
  return stmtNode;
}

static inline AST_NODE* makeDeclNode(DECL_KIND declKind) {
  AST_NODE* declNode = Allocate(DECLARATION_NODE);
  declNode->semantic_value.declSemanticValue.kind = declKind;
  return declNode;
}

static inline AST_NODE* makeExprNode(EXPR_KIND exprKind,
                                     int operationEnumValue) {
  AST_NODE* exprNode = Allocate(EXPR_NODE);
  exprNode->semantic_value.exprSemanticValue.isConstEval = 0;
  exprNode->semantic_value.exprSemanticValue.kind = exprKind;
  if (exprKind == BINARY_OPERATION) {
    exprNode->semantic_value.exprSemanticValue.op.binaryOp = operationEnumValue;
  } else if (exprKind == UNARY_OPERATION) {
    exprNode->semantic_value.exprSemanticValue.op.unaryOp = operationEnumValue;
  } else {
    printf(
        "Error in static inline AST_NODE* makeExprNode(EXPR_KIND exprKind, int "
        "operationEnumValue)\n");
  }
  return exprNode;
}

%}

%union {
  char *lexeme;
  CON_Type *const1;
  AST_NODE *node;
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

program: global_decl_list {}
       | {}
       ;

global_decl_list: global_decl_list global_decl {}
                | global_decl {}
                ;

global_decl: decl_list function_decl {}
           | function_decl {}
           ;

function_decl: type IDENTIFIER S_L_PAREN param_list S_R_PAREN S_L_BRACE block S_R_BRACE {}
             | R_VOID IDENTIFIER S_L_PAREN param_list S_R_PAREN S_L_BRACE block S_R_BRACE {}
             | type IDENTIFIER S_L_PAREN S_R_PAREN S_L_BRACE block S_R_BRACE {}
             | R_VOID IDENTIFIER S_L_PAREN S_R_PAREN S_L_BRACE block S_R_BRACE {}
             ;

param_list: param_list S_COMMA param {}
          | param {}
          ;

param: type IDENTIFIER {}
     | type IDENTIFIER dim_fn {}
     ;

dim_fn: S_L_BRACKET expr_null S_R_BRACKET {}
      | dim_fn S_L_BRACKET expr S_R_BRACKET {}
      ;

expr_null: expr {}
         | {}
         ;

block: decl_list stmt_list {}
     | stmt_list {}
     | decl_list {}
     ;

decl_list: decl_list decl {}
         | decl {}
         ;

decl: type_decl {}
    | var_decl {}
    ;

type_decl: R_TYPEDEF type id_list S_SEMICOLON {}
         | R_TYPEDEF R_VOID id_list S_SEMICOLON {}
         ;

var_decl: type init_id_list S_SEMICOLON {}
        | IDENTIFIER id_list S_SEMICOLON {}
        ;

type: R_INT {}
    | R_FLOAT {}
    ;

id_list: IDENTIFIER {}
       | id_list S_COMMA IDENTIFIER {}
       | id_list S_COMMA IDENTIFIER dim_decl {}
       | IDENTIFIER dim_decl {}
       ;

dim_decl: S_L_BRACKET cexpr S_R_BRACKET {}
        /* TODO
        |
        */
        ;

cexpr: cexpr O_ADDITION mcexpr {}
     | cexpr O_SUBTRACTION mcexpr {}
     | mcexpr {}
     ;

mcexpr: mcexpr O_MULTIPLICATION cfactor {}
      | mcexpr O_DIVISION cfactor {}
      | cfactor {}
      ;

cfactor: CONST {}
       | S_L_PAREN cexpr S_R_PAREN {}
       ;

init_id_list: init_id {}
            | init_id_list S_COMMA init_id {}
            ;

init_id: IDENTIFIER {}
       | IDENTIFIER dim_decl {}
       | IDENTIFIER O_ASSIGN relop_expr {}
       ;

stmt_list: stmt_list stmt {}
         | stmt {}
         ;

stmt: S_L_BRACE block S_R_BRACE {}
    /* TODO: while statement */
    | R_FOR S_L_PAREN assign_expr_list S_SEMICOLON relop_expr_list S_SEMICOLON assign_expr_list S_R_PAREN stmt {}
    | var_ref O_ASSIGN relop_expr S_SEMICOLON {}
    /* TODO: if statement */
    /* TODO: if then else */
    /* TODO: function call */
    | S_SEMICOLON {}
    | R_RETURN S_SEMICOLON {}
    | R_RETURN relop_expr S_SEMICOLON {}
    ;

assign_expr_list: nonempty_assign_expr_list {}
                | {}
                ;

nonempty_assign_expr_list: nonempty_assign_expr_list S_COMMA assign_expr {}
                         | assign_expr {}
                         ;

assign_expr: IDENTIFIER O_ASSIGN relop_expr {}
           | relop_expr {}
           ;

relop_expr: relop_term {}
          | relop_expr O_LOGICAL_OR relop_term {}
          ;

relop_term: relop_factor {}
          | relop_term O_LOGICAL_AND relop_factor {}
          ;

relop_factor: expr {}
            | expr rel_op expr {}
            ;

rel_op: O_EQ {}
      | O_GREATER_THAN_OR_EQ {}
      | O_LESS_THAN_OR_EQ {}
      | O_NOT_EQ {}
      | O_GREATER_THAN {}
      | O_LESS_THAN {}
      ;

relop_expr_list: nonempty_relop_expr_list {}
               | {}
               ;

nonempty_relop_expr_list: nonempty_relop_expr_list S_COMMA relop_expr {}
                        | relop_expr {}
                        ;

expr: expr add_op term {}
    | term {}
    ;

add_op: O_ADDITION {}
      | O_SUBTRACTION {}
      ;

term: term mul_op factor {}
    | factor {}
    ;

mul_op: O_MULTIPLICATION {}
      | O_DIVISION {}
      ;

factor: S_L_PAREN relop_expr S_R_PAREN {}
      /* TODO */
      | O_LOGICAL_NOT S_L_PAREN relop_expr S_R_PAREN {}
      | CONST {}
      | O_LOGICAL_NOT CONST {}
      | IDENTIFIER S_L_PAREN relop_expr_list S_R_PAREN {}
      | O_LOGICAL_NOT IDENTIFIER S_L_PAREN relop_expr_list S_R_PAREN {}
      | var_ref {}
      | O_LOGICAL_NOT var_ref {}
      ;

var_ref: IDENTIFIER {}
       | IDENTIFIER dim_list {}
       ;

dim_list: dim_list S_L_BRACKET expr S_R_BRACKET {} 
        | S_L_BRACKET expr S_R_BRACKET {}
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
