%require "3.4"
%language "c++"
%define api.token.constructor
%define api.value.type variant
%define parse.error verbose
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
#include "driver.h"

namespace {

DataType GetDataType(AstNode *a, AstNode *b) {
  if (a->data_type == UNKNOWN_TYPE && b->data_type == UNKNOWN_TYPE) return UNKNOWN_TYPE;
  if (a->data_type == INT_TYPE && b->data_type == INT_TYPE) return INT_TYPE;
  return FLOAT_TYPE;
}

DataType GetTypedefValue(const std::string &s) {
  if (s == "int") return INT_TYPE;
  if (s == "float") return FLOAT_TYPE;
  if (s == "void") return VOID_TYPE;
  return UNKNOWN_TYPE;
}

AstNode *MakeTypeNode(DataType type, const yy::location& loc) {
  AstNode *type_node = new AstNode(TYPE_NODE, loc);
  type_node->data_type = type;
  return type_node;
}

void Merge(std::list<AstNode*>& a, std::list<AstNode*>& b) {
  a.splice(a.end(), b);
}

AstNode* MakeChild(AstNode* parent, std::list<AstNode*>&& children) {
  parent->child = std::move(children);
  for (AstNode* i : parent->child) i->parent = parent;
  return parent;
}
AstNode* MakeChild(AstNode* parent, std::list<AstNode*>& children) {
  parent->child = std::move(children);
  for (AstNode* i : parent->child) i->parent = parent;
  return parent;
}

AstNode* MakeIDNode(const std::string& lexeme, IdentifierKind id_kind,
                    const yy::location& loc) {
  AstNode* identifier = new AstNode(IDENTIFIER_NODE, loc);
  identifier->semantic_value.identifier_semantic_value.identifier_name = lexeme;
  identifier->semantic_value.identifier_semantic_value.kind = id_kind;
  identifier->semantic_value.identifier_semantic_value.symboltable_entry = NULL;
  return identifier;
}

AstNode* MakeStmtNode(StmtKind stmt_kind, const yy::location& loc) {
  AstNode* stmt_node = new AstNode(STMT_NODE, loc);
  stmt_node->semantic_value.stmt_semantic_value.kind = stmt_kind;
  return stmt_node;
}

AstNode* MakeDeclNode(DeclKind decl_kind, const yy::location& loc) {
  AstNode* decl_node = new AstNode(DECLARATION_NODE, loc);
  decl_node->semantic_value.decl_semantic_value.kind = decl_kind;
  return decl_node;
}

AstNode* MakeExprNode(ExprKind expr_kind, DataType data_type,
                      int operation_enum_value, const yy::location& loc,
                      std::list<AstNode*>&& ch) {
  AstNode* expr_node = new AstNode(EXPR_NODE, loc);
  expr_node->data_type = data_type;
  expr_node->semantic_value.expr_semantic_value.is_const_eval = 0;
  expr_node->semantic_value.expr_semantic_value.kind = expr_kind;
  MakeChild(expr_node, ch);
  if (expr_kind == BINARY_OPERATION) {
    expr_node->semantic_value.expr_semantic_value.op.binary_op =
        BinaryOperator(operation_enum_value);
  } else if (expr_kind == UNARY_OPERATION) {
    expr_node->semantic_value.expr_semantic_value.op.unary_op =
        UnaryOperator(operation_enum_value);
  } else {
    printf(
        "Error in static inline AstNode* MakeExprNode(EXPR_KIND exprKind, int "
        "operationEnumValue)\n");
  }
  return expr_node;
}

}  // namespace

%}

// TODO: constant folding -> move to sematics??

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

%left O_LOGICAL_OR
%left O_LOGICAL_AND
%left O_LESS_THAN O_LESS_THAN_OR_EQ O_GREATER_THAN O_GREATER_THAN_OR_EQ O_EQ O_NOT_EQ
%left O_ADDITION O_SUBTRACTION
%left O_MULTIPLICATION O_DIVISION

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

%nonassoc LOWER_THAN_ELSE
%nonassoc R_ELSE

%type <AstNode*> program function_decl block decl var_decl init_id stmt
%type <AstNode*> relop_expr var_ref
%type <AstNode*> param cexpr assign_expr assign_expr_list
%type <AstNode*> type_decl id_item relop_expr_list unifact
%type <std::list<AstNode*>> global_decl_list stmt_list decl_list init_id_list
%type <std::list<AstNode*>> param_list id_list dim_list
%type <std::list<AstNode*>> nonempty_relop_expr_list
%type <std::list<AstNode*>> nonempty_assign_expr_list
%type <std::list<AstNode*>> global_decl dim_fn dim_decl


%start program

%%

program:
  global_decl_list {
    $$ = new AstNode(PROGRAM_NODE, @$);
    MakeChild($$, $1);
    drv.prog = $$;
  } |
  /* null */ {
    $$ = new AstNode(PROGRAM_NODE, @$);
    drv.prog = $$;
  };

global_decl_list:
  global_decl_list global_decl {
    $$ = std::move($1);
    Merge($$, $2);
  } |
  global_decl {
    $$ = std::move($1);
  };

global_decl:
  decl_list function_decl {
    $$ = {MakeChild(new AstNode(VARIABLE_DECL_LIST_NODE, @1), $1), $2};
  } |
  function_decl {
    $$ = {$1};
  };

/* function declaration */
function_decl:
  IDENTIFIER IDENTIFIER S_L_PAREN param_list S_R_PAREN S_L_BRACE block S_R_BRACE {
    /* e.g., int f(float a, int b) {} / my_type g(int k[]) {} */
    $$ = MakeDeclNode(FUNCTION_DECL, @$);
    AstNode *param = MakeChild(new AstNode(PARAM_LIST_NODE, @4), $4);
    DataType type = GetTypedefValue($1);
    MakeChild($$, {MakeTypeNode(type, @1), MakeIDNode($2, NORMAL_ID, @2), param, $7});
  };

param_list:
  param_list S_COMMA param {
    $$ = std::move($1);
    $$.push_back($3);
  } |
  param {
    $$ = {$1};
  } |
  /* null */ {
  };

/* Function parameter */
param:
  IDENTIFIER IDENTIFIER {
    /* e.g., int a, float b, my_type c */
    $$ = MakeDeclNode(FUNCTION_PARAMETER_DECL, @$);
    DataType type = GetTypedefValue($1);
    if (type == VOID_TYPE) {
      throw yy::parser::syntax_error(@$, "variable cannot be declared void");
    }
    MakeChild($$, {MakeTypeNode(type, @1), MakeIDNode($2, NORMAL_ID, @2)});
  } |
  IDENTIFIER IDENTIFIER dim_fn {
    /* e.g., int a[3], float b[][5], my_type c[][12][34] */
    $$ = MakeDeclNode(FUNCTION_PARAMETER_DECL, @$);
    AstNode *identifier = MakeIDNode($2, ARRAY_ID, @2);
    MakeChild(identifier, $3);
    MakeChild($$, {MakeTypeNode(GetTypedefValue($1), @1), identifier});
  };

dim_fn:
  S_L_BRACKET S_R_BRACKET {
    $$ = {new AstNode(NULL_NODE, @$)};
  } |
  S_L_BRACKET cexpr S_R_BRACKET {
    $$ = {$2};
  } |
  dim_fn S_L_BRACKET cexpr S_R_BRACKET {
    $$ = std::move($1);
    $$.push_back($3);
  };

block:
  decl_list stmt_list {
    $$ = new AstNode(BLOCK_NODE, @$);
    AstNode *decl = MakeChild(new AstNode(VARIABLE_DECL_LIST_NODE, @1), $1);
    AstNode *stmt = MakeChild(new AstNode(STMT_LIST_NODE, @2), $2);
    MakeChild($$, {decl, stmt});
  } |
  stmt_list {
    $$ = new AstNode(BLOCK_NODE, @$);
    MakeChild($$, {MakeChild(new AstNode(STMT_LIST_NODE, @1), $1)});
  } |
  decl_list {
    $$ = new AstNode(BLOCK_NODE, @$);
    MakeChild($$, {MakeChild(new AstNode(VARIABLE_DECL_LIST_NODE, @1), $1)});
  } |
  /* null */ {
    $$ = new AstNode(BLOCK_NODE, @$);
    //MakeChild($$, new AstNode(NULL_NODE));
  };

decl_list:
  decl_list decl {
    $$ = std::move($1);
    $$.push_back($2);
  } |
  decl {
    $$ = {$1};
  };

decl:
  type_decl {
    $$ = $1;
  } |
  var_decl {
    $$ = $1;
  };

/* typedef declaration */
type_decl:
  R_TYPEDEF IDENTIFIER id_list S_SEMICOLON {
    $$ = MakeDeclNode(TYPE_DECL, @$);
    $3.push_front(MakeTypeNode(GetTypedefValue($2), @2));
    MakeChild($$, std::move($3));
  };

var_decl:
  IDENTIFIER init_id_list S_SEMICOLON {
    $$ = MakeDeclNode(VARIABLE_DECL, @$);
    DataType type = GetTypedefValue($1);
    if (type == VOID_TYPE) {
      throw yy::parser::syntax_error(@$, "variable cannot be declared void");
    }
    $2.push_front(MakeTypeNode(GetTypedefValue($1), @1));
    MakeChild($$, std::move($2));
  };

id_item:
  IDENTIFIER {
    $$ = {MakeIDNode($1, NORMAL_ID, @1)};
  } |
  IDENTIFIER dim_decl {
    $$ = {MakeIDNode($1, ARRAY_ID, @1)};
    MakeChild($$, $2);
  };

id_list:
  id_item {
    $$ = {$1};
  } |
  id_list S_COMMA id_item {
    $$ = std::move($1);
    $$.push_back($3);
  };

dim_decl:
  dim_decl S_L_BRACKET cexpr S_R_BRACKET {
    $$ = std::move($1);
    $$.push_back($3);
  } |
  S_L_BRACKET cexpr S_R_BRACKET {
    $$ = {$2};
  };

cexpr:
  cexpr O_ADDITION cexpr {
    $$ = MakeExprNode(BINARY_OPERATION, GetDataType($1, $3), BINARY_OP_ADD, @$, {$1, $3});
  } |
  cexpr O_SUBTRACTION cexpr {
    $$ = MakeExprNode(BINARY_OPERATION, GetDataType($1, $3), BINARY_OP_SUB, @$, {$1, $3});
  } |
  cexpr O_MULTIPLICATION cexpr {
    $$ = MakeExprNode(BINARY_OPERATION, GetDataType($1, $3), BINARY_OP_MUL, @$, {$1, $3});
  } |
  cexpr O_DIVISION cexpr {
    $$ = MakeExprNode(BINARY_OPERATION, GetDataType($1, $3), BINARY_OP_DIV, @$, {$1, $3});
  } |
  CONST {
    $$ = $1;
  } |
  S_L_PAREN cexpr S_R_PAREN {
    $$ = $2;
  };

init_id_list:
  init_id {
    $$ = {$1};
  } |
  init_id_list S_COMMA init_id {
    $$ = std::move($1);
    $$.push_back($3);
  };

init_id:
  IDENTIFIER {
    $$ = MakeIDNode($1, NORMAL_ID, @$);
  } |
  IDENTIFIER dim_decl {
    $$ = MakeIDNode($1, ARRAY_ID, @$);
    MakeChild($$, $2);
  } |
  IDENTIFIER O_ASSIGN relop_expr {
    $$ = MakeIDNode($1, WITH_INIT_ID, @$);
    MakeChild($$, {$3});
  };

stmt_list:
  stmt_list stmt {
    $$ = std::move($1);
    $$.push_back($2);
  } |
  stmt {
    $$ = {$1};
  };

stmt:
  S_L_BRACE block S_R_BRACE {
    $$ = $2;
  } |
  R_WHILE S_L_PAREN relop_expr S_R_PAREN stmt {
    $$ = MakeStmtNode(WHILE_STMT, @$);
    MakeChild($$, {$3, $5});
  } |
  R_FOR S_L_PAREN assign_expr_list S_SEMICOLON relop_expr_list S_SEMICOLON assign_expr_list S_R_PAREN stmt {
    $$ = MakeStmtNode(FOR_STMT, @$);
    MakeChild($$, {$3, $5, $7, $9});
  } |
  var_ref O_ASSIGN relop_expr S_SEMICOLON {
    $$ = MakeStmtNode(ASSIGN_STMT, @$);
    MakeChild($$, {$1, $3});
  } |
  R_IF S_L_PAREN relop_expr S_R_PAREN stmt %prec LOWER_THAN_ELSE {
    $$ = MakeStmtNode(IF_STMT, @$);
    MakeChild($$, {$3, $5});
  } |
  R_IF S_L_PAREN relop_expr S_R_PAREN stmt R_ELSE stmt {
    $$ = MakeStmtNode(IF_ELSE_STMT, @$);
    MakeChild($$, {$3, $5, $7});
  } |
  IDENTIFIER S_L_PAREN relop_expr_list S_R_PAREN S_SEMICOLON {
    $$ = MakeStmtNode(FUNCTION_CALL_STMT, @$);
    MakeChild($$, {MakeIDNode($1, NORMAL_ID, @1), $3});
  } |
  S_SEMICOLON {
    $$ = new AstNode(NULL_NODE, @$);
  } |
  R_RETURN S_SEMICOLON {
    $$ = MakeStmtNode(RETURN_STMT, @$);
  } |
  R_RETURN relop_expr S_SEMICOLON {
    $$ = MakeStmtNode(RETURN_STMT, @$);
    MakeChild($$, {$2});
  };

assign_expr_list:
  nonempty_assign_expr_list {
    $$ = new AstNode(NONEMPTY_ASSIGN_EXPR_LIST_NODE, @$);
    MakeChild($$, $1);
  } |
  /* null */ {
    $$ = new AstNode(NULL_NODE, @$);
  };

nonempty_assign_expr_list:
  nonempty_assign_expr_list S_COMMA assign_expr {
    $$ = std::move($1);
    $$.push_back($3);
  } |
  assign_expr {
    $$ = {$1};
  };

assign_expr:
  IDENTIFIER O_ASSIGN relop_expr {
    $$ = MakeStmtNode(ASSIGN_STMT, @$);
    MakeChild($$, {MakeIDNode($1, NORMAL_ID, @1), $3});
  } |
  relop_expr {
    $$ = $1;
  };

relop_expr:
  relop_expr O_LOGICAL_OR relop_expr {
    $$ = MakeExprNode(BINARY_OPERATION, INT_TYPE, BINARY_OP_OR, @$, {$1, $3});
  } |
  relop_expr O_LOGICAL_AND relop_expr {
    $$ = MakeExprNode(BINARY_OPERATION, INT_TYPE, BINARY_OP_AND, @$, {$1, $3});
  } |
  relop_expr O_LESS_THAN relop_expr {
    $$ = MakeExprNode(BINARY_OPERATION, INT_TYPE, BINARY_OP_LT, @$, {$1, $3});
  } |
  relop_expr O_LESS_THAN_OR_EQ relop_expr {
    $$ = MakeExprNode(BINARY_OPERATION, INT_TYPE, BINARY_OP_LE, @$, {$1, $3});
  } |
  relop_expr O_GREATER_THAN relop_expr {
    $$ = MakeExprNode(BINARY_OPERATION, INT_TYPE, BINARY_OP_GT, @$, {$1, $3});
  } |
  relop_expr O_GREATER_THAN_OR_EQ relop_expr {
    $$ = MakeExprNode(BINARY_OPERATION, INT_TYPE, BINARY_OP_GE, @$, {$1, $3});
  } |
  relop_expr O_EQ relop_expr {
    $$ = MakeExprNode(BINARY_OPERATION, INT_TYPE, BINARY_OP_EQ, @$, {$1, $3});
  } |
  relop_expr O_NOT_EQ relop_expr {
    $$ = MakeExprNode(BINARY_OPERATION, INT_TYPE, BINARY_OP_NE, @$, {$1, $3});
  } |
  relop_expr O_ADDITION relop_expr {
    $$ = MakeExprNode(BINARY_OPERATION, GetDataType($1, $3), BINARY_OP_ADD, @$, {$1, $3});
  } |
  relop_expr O_SUBTRACTION relop_expr {
    $$ = MakeExprNode(BINARY_OPERATION, GetDataType($1, $3), BINARY_OP_SUB, @$, {$1, $3});
  } |
  relop_expr O_MULTIPLICATION relop_expr {
    $$ = MakeExprNode(BINARY_OPERATION, GetDataType($1, $3), BINARY_OP_MUL, @$, {$1, $3});
  } |
  relop_expr O_DIVISION relop_expr {
    $$ = MakeExprNode(BINARY_OPERATION, GetDataType($1, $3), BINARY_OP_DIV, @$, {$1, $3});
  } |
  unifact {
    $$ = $1;
  } |
  O_SUBTRACTION unifact {
    $$ = MakeExprNode(UNARY_OPERATION, $2->data_type, UNARY_OP_NEGATIVE, @$, {$2});
  } |
  O_LOGICAL_NOT unifact {
    $$ = MakeExprNode(UNARY_OPERATION, INT_TYPE, UNARY_OP_LOGICAL_NEGATION, @$, {$2});
  };

relop_expr_list:
  nonempty_relop_expr_list {
    $$ = new AstNode(NONEMPTY_RELOP_EXPR_LIST_NODE, @$);
    MakeChild($$, $1);
  } |
  /* null */ {
    $$ = new AstNode(NULL_NODE, @$);
  };

nonempty_relop_expr_list:
  nonempty_relop_expr_list S_COMMA relop_expr {
    $$ = std::move($1);
    $$.push_back($3);
  } |
  relop_expr {
    $$ = {$1};
  };

unifact:
  var_ref {
    $$ = $1;
  } |
  CONST {
    $$ = $1;
  } |
  S_L_PAREN relop_expr S_R_PAREN {
    $$ = $2;
  } |
  IDENTIFIER S_L_PAREN relop_expr_list S_R_PAREN {
    $$ = MakeStmtNode(FUNCTION_CALL_STMT, @$);
    MakeChild($$, {MakeIDNode($1, NORMAL_ID, @1), $3});
  };

var_ref:
  IDENTIFIER {
    $$ = MakeIDNode($1, NORMAL_ID, @$);
  } |
  IDENTIFIER dim_list {
    $$ = MakeIDNode($1, ARRAY_ID, @$);
    MakeChild($$, {$2});
  };

dim_list:
  dim_list S_L_BRACKET cexpr S_R_BRACKET {
    $$ = std::move($1);
    $$.push_back($3);
  } |
  S_L_BRACKET cexpr S_R_BRACKET {
    $$ = {$2};
  };

%%

void yy::parser::error (const location_type& l, const std::string& m) {
  std::cerr << l << ": " << m << '\n';
}
