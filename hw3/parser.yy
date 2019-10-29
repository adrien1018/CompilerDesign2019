%require "3.4"
%language "c++"
%define api.token.constructor
%define api.value.type variant
%defines
%code requires {
  #include <string>
  class Driver;
}
%param { Driver& drv }
%locations
%code {
#include "driver.h"
}

%{
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cstdarg>
#include <vector>

#include "header.h"
#include "header_lex.h"
#include "driver.h"

int linenumber = 1;
AstNode *prog;

extern int g_anyErrorOccur;

namespace {

inline AstNode* MakeSibling(AstNode* a, AstNode* b) {
  while (a->right_sibling) {
    a = a->right_sibling;
  }
  if (b == nullptr) {
    return a;
  }
  b = b->leftmost_sibling;
  a->right_sibling = b;

  b->leftmost_sibling = a->leftmost_sibling;
  b->parent = a->parent;
  while (b->right_sibling) {
    b = b->right_sibling;
    b->leftmost_sibling = a->leftmost_sibling;
    b->parent = a->parent;
  }
  return b;
}

inline AstNode* MakeChild(AstNode* parent, AstNode* child) {
  if (child == nullptr) {
    return parent;
  }
  if (parent->child) {
    MakeSibling(parent->child, child);
  } else {
    child = child->leftmost_sibling;
    parent->child = child;
    while (child) {
      child->parent = parent;
      child = child->right_sibling;
    }
  }
  return parent;
}

AstNode* MakeFamily(AstNode* parent, const std::vector<AstNode*>& children) {
  AstNode* child = children[0];
  MakeChild(parent, child);
  AstNode* tmp = child;
  for (size_t i = 1; i < children.size(); ++i) {
    child = children[i];
    tmp = MakeSibling(tmp, child);
  }
  return parent;
}

inline AstNode* MakeIDNode(const std::string& lexeme, IdentifierKind idKind) {
  AstNode* identifier = Allocate(IDENTIFIER_NODE);
  identifier->semantic_value.identifier_semantic_value.identifier_name = lexeme;
  identifier->semantic_value.identifier_semantic_value.kind = idKind;
  identifier->semantic_value.identifier_semantic_value.symboltable_entry = NULL;
  return identifier;
}

inline AstNode* MakeStmtNode(StmtKind stmtKind) {
  AstNode* stmtNode = Allocate(STMT_NODE);
  stmtNode->semantic_value.stmt_semantic_value.kind = stmtKind;
  return stmtNode;
}

inline AstNode* MakeDeclNode(DeclKind declKind) {
  AstNode* declNode = Allocate(DECLARATION_NODE);
  declNode->semantic_value.decl_semantic_value.kind = declKind;
  return declNode;
}

inline AstNode* MakeExprNode(ExprKind exprKind, int operationEnumValue) {
  AstNode* exprNode = Allocate(EXPR_NODE);
  exprNode->semantic_value.expr_semantic_value.is_const_eval = 0;
  exprNode->semantic_value.expr_semantic_value.kind = exprKind;
  if (exprKind == BINARY_OPERATION) {
    exprNode->semantic_value.expr_semantic_value.op.binary_op =
        BinaryOperator(operationEnumValue);
  } else if (exprKind == UNARY_OPERATION) {
    exprNode->semantic_value.expr_semantic_value.op.unary_op =
        UnaryOperator(operationEnumValue);
  } else {
    printf(
        "Error in static inline AstNode* MakeExprNode(EXPR_KIND exprKind, int "
        "operationEnumValue)\n");
  }
  return exprNode;
}

}  // namespace

%}

%token <std::string> IDENTIFIER
%token <AstNode*> CONST
//%token <ConstType*> CONST

%token END 0

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

%type <AstNode*> program global_decl_list global_decl function_decl block stmt_list decl_list decl var_decl type init_id_list init_id  stmt relop_expr relop_term relop_factor expr term factor var_ref
%type <AstNode*> param_list param dim_fn expr_null id_list dim_decl cexpr mcexpr cfactor assign_expr_list assign_expr rel_op relop_expr_list nonempty_relop_expr_list
%type <AstNode*> add_op mul_op dim_list type_decl nonempty_assign_expr_list


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
             | R_VOID IDENTIFIER S_L_PAREN param_list S_R_PAREN S_L_BRACE block S_R_BRACE {
                 $$ = MakeDeclNode(FUNCTION_DECL);
                 AstNode *param = Allocate(PARAM_LIST_NODE);
                 MakeChild(param, $4);
                 MakeFamily($$, {MakeIDNode("void", NORMAL_ID), MakeIDNode($2, NORMAL_ID), param, $7});
               }
             | type IDENTIFIER S_L_PAREN S_R_PAREN S_L_BRACE block S_R_BRACE {
                 $$ = MakeDeclNode(FUNCTION_DECL);
                 AstNode *empty_param = Allocate(PARAM_LIST_NODE);
                 MakeFamily($$, {$1, MakeIDNode($2, NORMAL_ID), empty_param, $6});
               }
             | R_VOID IDENTIFIER S_L_PAREN S_R_PAREN S_L_BRACE block S_R_BRACE {
                 $$ = MakeDeclNode(FUNCTION_DECL);
                 AstNode *empty_param = Allocate(PARAM_LIST_NODE);
                 MakeFamily($$, {MakeIDNode("void", NORMAL_ID), MakeIDNode($2, NORMAL_ID), empty_param, $6});
               }
             ;

param_list: param_list S_COMMA param { $$ = MakeSibling($1, $3); }
          | param { $$ = $1; }
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

expr_null: expr { $$ = $1; }
         | { $$ = Allocate(NULL_NODE); }
         ;

block: decl_list stmt_list {
         $$ = Allocate(BLOCK_NODE);
         AstNode *decl = MakeChild(Allocate(VARIABLE_DECL_LIST_NODE), $1);
         AstNode *stmt = MakeChild(Allocate(STMT_LIST_NODE), $2);
         MakeFamily($$, {decl, stmt});
       }
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

decl_list: decl_list decl { $$ = MakeSibling($1, $2); }
         | decl { $$ = $1; }
         ;

decl: type_decl { $$ = $1; }
    | var_decl { $$ = $1; }
    ;

type_decl: R_TYPEDEF type id_list S_SEMICOLON { 
             $$ = MakeDeclNode(TYPE_DECL);
             MakeFamily($$, {$2, $3});
           }
         | R_TYPEDEF R_VOID id_list S_SEMICOLON {
             $$ = MakeDeclNode(TYPE_DECL);
             MakeFamily($$, {MakeIDNode("void", NORMAL_ID), $3});
           }
         ;

var_decl: type init_id_list S_SEMICOLON { 
            $$ = MakeDeclNode(VARIABLE_DECL);
            MakeFamily($$, {$1, $2});
          }
        | IDENTIFIER id_list S_SEMICOLON {
            $$ = MakeDeclNode(VARIABLE_DECL);
            MakeFamily($$, {MakeIDNode($1, NORMAL_ID), $2});
          }
        ;

type: R_INT { $$ = MakeIDNode("int", NORMAL_ID); }
    | R_FLOAT { $$ = MakeIDNode("float", NORMAL_ID); }
    ;

id_list: IDENTIFIER { $$ = MakeIDNode($1, NORMAL_ID); }
       | id_list S_COMMA IDENTIFIER { $$ = MakeSibling($1, MakeIDNode($3, NORMAL_ID)); }
       | id_list S_COMMA IDENTIFIER dim_decl {
           AstNode *identifier = MakeIDNode($3, ARRAY_ID);
           MakeChild(identifier, $4);
           $$ = MakeSibling($1, identifier);
         }
       | IDENTIFIER dim_decl {
           $$ = MakeIDNode($1, ARRAY_ID);
           MakeChild($$, $2);
         }
       ;

dim_decl: S_L_BRACKET cexpr S_R_BRACKET { $$ = $2; }
        /* TODO: recursive grammar rule */
        ;

cexpr: cexpr O_ADDITION mcexpr {
         $$ = MakeExprNode(BINARY_OPERATION, BINARY_OP_ADD);
         MakeFamily($$, {$1, $3});
       }
     | cexpr O_SUBTRACTION mcexpr {
         $$ = MakeExprNode(BINARY_OPERATION, BINARY_OP_SUB);
         MakeFamily($$, {$1, $3});
       }
     | mcexpr { $$ = $1; }
     ;

mcexpr: mcexpr O_MULTIPLICATION cfactor {
          $$ = MakeExprNode(BINARY_OPERATION, BINARY_OP_MUL);
          MakeFamily($$, {$1, $3});
        }
      | mcexpr O_DIVISION cfactor {
          $$ = MakeExprNode(BINARY_OPERATION, BINARY_OP_DIV);
          MakeFamily($$, {$1, $3});
        }
      | cfactor { $$ = $1; }
      ;

cfactor: CONST { 
           $$ = Allocate(CONST_VALUE_NODE);
           $$->semantic_value.const1 = (ConstType*)$1;
         }
       | S_L_PAREN cexpr S_R_PAREN { $$ = $2; }
       ;

init_id_list: init_id { $$ = $1; }
            | init_id_list S_COMMA init_id { $$ = MakeSibling($1, $3); }
            ;

init_id: IDENTIFIER { $$ = MakeIDNode($1, NORMAL_ID); }
       | IDENTIFIER dim_decl {
           $$ = MakeIDNode($1, ARRAY_ID);
           MakeChild($$, $2);
         }
       | IDENTIFIER O_ASSIGN relop_expr {
           AstNode *identifier = MakeIDNode($1, NORMAL_ID);
           $$ = MakeStmtNode(ASSIGN_STMT);
           MakeFamily($$, {identifier, $3});
         }
       ;

stmt_list: stmt_list stmt { $$ = MakeSibling($1, $2); }
         | stmt { $$ = $1; }
         ;

stmt: S_L_BRACE block S_R_BRACE { /* TODO */ }
    | R_WHILE S_L_PAREN relop_expr_list S_R_PAREN stmt { 
        $$ = MakeStmtNode(WHILE_STMT);
        MakeFamily($$, {$3, $5});
      }
    | R_FOR S_L_PAREN assign_expr_list S_SEMICOLON relop_expr_list S_SEMICOLON assign_expr_list S_R_PAREN stmt {
        $$ = MakeStmtNode(FOR_STMT);
        MakeFamily($$, {$3, $5, $7, $9});
      }
    | var_ref O_ASSIGN relop_expr S_SEMICOLON { /* TODO */ }
    | R_IF S_L_PAREN relop_expr_list S_R_PAREN stmt { /* TODO */ }
    /* TODO: if then else */
    /* TODO: function call */
    | S_SEMICOLON { $$ = Allocate(NULL_NODE); }
    | R_RETURN S_SEMICOLON { $$ = MakeStmtNode(RETURN_STMT); }
    | R_RETURN relop_expr S_SEMICOLON {
        $$ = MakeStmtNode(RETURN_STMT);
        MakeChild($$, $2);
      }
    ;

assign_expr_list: nonempty_assign_expr_list { $$ = $1; }
                | { $$ = Allocate(NULL_NODE); }
                ;

nonempty_assign_expr_list: nonempty_assign_expr_list S_COMMA assign_expr { $$ = MakeSibling($1, $3); }
                         | assign_expr { $$ = $1; }
                         ;

assign_expr: IDENTIFIER O_ASSIGN relop_expr {
               $$ = MakeStmtNode(ASSIGN_STMT);
               MakeFamily($$, {MakeIDNode($1, NORMAL_ID), $3});
             }
           | relop_expr { $$ = $1; }
           ;

relop_expr: relop_term { $$ = $1; }
          | relop_expr O_LOGICAL_OR relop_term {
              $$ = MakeExprNode(BINARY_OPERATION, BINARY_OP_OR);
              MakeFamily($$, {$1, $3});
            }
          ;

relop_term: relop_factor { $$ = $1; }
          | relop_term O_LOGICAL_AND relop_factor { 
              $$ = MakeExprNode(BINARY_OPERATION, BINARY_OP_AND);
              MakeFamily($$, {$1, $3});
            }
          ;

relop_factor: expr { $$ = $1; }
            | expr rel_op expr { $$ = MakeFamily($2, {$1, $3}); }
            ;

rel_op: O_EQ { $$ = MakeExprNode(BINARY_OPERATION, BINARY_OP_EQ); }
      | O_GREATER_THAN_OR_EQ { $$ = MakeExprNode(BINARY_OPERATION, BINARY_OP_GE); }
      | O_LESS_THAN_OR_EQ { $$ = MakeExprNode(BINARY_OPERATION, BINARY_OP_LE); }
      | O_NOT_EQ { $$ = MakeExprNode(BINARY_OPERATION, BINARY_OP_NE); }
      | O_GREATER_THAN { $$ = MakeExprNode(BINARY_OPERATION, BINARY_OP_GT); }
      | O_LESS_THAN { $$ = MakeExprNode(BINARY_OPERATION, BINARY_OP_LT); }
      ;

relop_expr_list: nonempty_relop_expr_list { $$ = $1; }
               | { $$ = Allocate(NULL_NODE); }
               ;

nonempty_relop_expr_list: nonempty_relop_expr_list S_COMMA relop_expr { $$ = MakeSibling($1, $3); }
                        | relop_expr { $$ = $1; }
                        ;

expr: expr add_op term { $$ = MakeFamily($2, {$1, $3}); }
    | term { $$ = $1; }
    ;

add_op: O_ADDITION { $$ = MakeExprNode(BINARY_OPERATION, BINARY_OP_ADD); }
      | O_SUBTRACTION { $$ = MakeExprNode(BINARY_OPERATION, BINARY_OP_SUB); }
      ;

term: term mul_op factor { $$ = MakeFamily($2, {$1, $3}); }
    | factor { $$ = $1; }
    ;

mul_op: O_MULTIPLICATION { $$ = MakeExprNode(BINARY_OPERATION, BINARY_OP_MUL); }
      | O_DIVISION { $$ = MakeExprNode(BINARY_OPERATION, BINARY_OP_DIV); }
      ;

factor: S_L_PAREN relop_expr S_R_PAREN { $$ = $2; }
      | O_SUBTRACTION S_L_PAREN relop_expr S_R_PAREN {
          $$ = MakeExprNode(UNARY_OPERATION, UNARY_OP_NEGATIVE);
          MakeChild($$, $3);
        }
      | O_LOGICAL_NOT S_L_PAREN relop_expr S_R_PAREN { 
          $$ = MakeExprNode(UNARY_OPERATION, UNARY_OP_LOGICAL_NEGATION);
          MakeChild($$, $3);
        }
      | CONST {
          $$ = Allocate(CONST_VALUE_NODE);
          $$->semantic_value.const1 = (ConstType*)$1;
        }
      | O_SUBTRACTION CONST {
          $$ = MakeExprNode(UNARY_OPERATION, UNARY_OP_NEGATIVE);
          MakeChild($$, $2);
        }
      | O_LOGICAL_NOT CONST {
          $$ = MakeExprNode(UNARY_OPERATION, UNARY_OP_LOGICAL_NEGATION);
          MakeChild($$, $2);
        }
      | IDENTIFIER S_L_PAREN relop_expr_list S_R_PAREN { /* TODO */ }
      | O_SUBTRACTION IDENTIFIER S_L_PAREN relop_expr_list S_R_PAREN { /* TODO */ }
      | O_LOGICAL_NOT IDENTIFIER S_L_PAREN relop_expr_list S_R_PAREN { /* TODO */ }
      | var_ref { $$ = $1; }
      | O_SUBTRACTION var_ref {
          $$ = MakeExprNode(UNARY_OPERATION, UNARY_OP_NEGATIVE);
          MakeChild($$, $2);
        }
      | O_LOGICAL_NOT var_ref { 
          $$ = MakeExprNode(UNARY_OPERATION, UNARY_OP_LOGICAL_NEGATION);
          MakeChild($$, $2);
        }
      ;

var_ref: IDENTIFIER { $$ = MakeIDNode($1, NORMAL_ID); }
       | IDENTIFIER dim_list {
           $$ = MakeIDNode($1, ARRAY_ID);
           MakeChild($$, $2);
         }
       ;

dim_list: dim_list S_L_BRACKET expr S_R_BRACKET { /* TODO */ }
        | S_L_BRACKET expr S_R_BRACKET { /* TODO */ }
        ;


%%

//#include "lex.yy.cc"
//
//
//int main(int argc, char* argv[]) {
//  /*yyFlexLexer* lexer;
//  std::ifstream input;
//  if (argc > 1) {
//    input.open(argv[1]);
//    lexer = new yyFlexLexer(&input, &std::cout);
//  } else {
//    lexer = new yyFlexLexer();
//  }
//  while (lexer->yylex() != 0);
//  delete lexer;
//  PrintStrings(comments);
//  PrintSymTab(symtab);*/
//  yyin = fopen(argv[1], "r");
//  yyparse();
//  printf("%s\n", "Parsing completed. No errors found.");
//  printGV(prog, NULL);
//}

void yy::parser::error (const location_type& l, const std::string& m) {
  std::cerr << l << ": " << m << '\n';
}
