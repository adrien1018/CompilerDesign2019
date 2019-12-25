%require "3.4"
%language "c++"
%define api.token.constructor
%define api.value.type variant
%define api.location.type {Location}
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
#include <variant>
#include <vector>

#include "ast.h"
#include "driver.h"

namespace {

std::string GetTypeName(DataType type) {
  switch (type) {
    case INT_TYPE: return "int";
    case FLOAT_TYPE: return "float";
    case VOID_TYPE: return "void";
    case CONST_STRING_TYPE: return "char *";
    default: return "";
  }
  return "";
}

std::string GetOpName(BinaryOperator op) {
  switch (op) {
    case BINARY_OP_ADD: return "+";
    case BINARY_OP_SUB: return "-";
    case BINARY_OP_MUL: return "*";
    case BINARY_OP_DIV: return "/";
    case BINARY_OP_AND: return "&&";
    case BINARY_OP_OR: return "||";
    case BINARY_OP_EQ: return "==";
    case BINARY_OP_NE: return "!=";
    case BINARY_OP_LT: return "<";
    case BINARY_OP_GT: return ">";
    case BINARY_OP_LE: return "<=";
    case BINARY_OP_GE: return ">=";
  }
  return "";
}

// TODO: Optimize this stuff
constexpr bool IsLogicalOp(BinaryOperator op) {
  return op == BINARY_OP_EQ || op == BINARY_OP_GE || op == BINARY_OP_LE ||
         op == BINARY_OP_NE || op == BINARY_OP_GT || op == BINARY_OP_LT ||
         op == BINARY_OP_AND || op == BINARY_OP_OR;
}

constexpr bool IsArithmaticOp(BinaryOperator op) {
  return !IsLogicalOp(op);
}

inline DataType MixDataType(AstNode *a, AstNode *b, BinaryOperator op) {
  if (a->data_type == CONST_STRING_TYPE || b->data_type == CONST_STRING_TYPE) {
    std::string x = GetTypeName(a->data_type);
    std::string y = GetTypeName(b->data_type);
    std::string z = GetOpName(op);
    throw std::invalid_argument("invalid operands to binary " + z + " (have '" + x + "' and '" + y + "')");
  }
  if (IsLogicalOp(op)) return BOOLEAN_TYPE;
  if (a->data_type == UNKNOWN_TYPE || b->data_type == UNKNOWN_TYPE) return UNKNOWN_TYPE;
  if (a->data_type == FLOAT_TYPE || b->data_type == FLOAT_TYPE) return FLOAT_TYPE;
  return INT_TYPE;
}

DataType GetTypedefValue(const std::string &s) noexcept {
  if (s == "int") return INT_TYPE;
  if (s == "float") return FLOAT_TYPE;
  if (s == "void") return VOID_TYPE;
  return UNKNOWN_TYPE;
}

template <typename T, typename U>
struct Wider {
  using type = FloatType;
};

template <>
struct Wider<int32_t, int32_t> {
  using type = int32_t;
};

template <typename T, typename U>
typename Wider<T, U>::type DoArithmaticOperation(BinaryOperator op, T x, U y) {
  using ReturnType = typename Wider<T, U>::type;
  ReturnType lhs(x), rhs(y);
  switch (op) {
    case BINARY_OP_ADD:
      return lhs + rhs;
    case BINARY_OP_SUB:
      return lhs - rhs;
    case BINARY_OP_MUL:
      return lhs * rhs;
    case BINARY_OP_DIV:
      return lhs / rhs;
  }
  return 0;
}

template <typename T, typename U>
int32_t DoLogicalOperation(BinaryOperator op, T x, U y) {
  switch (op) {
    case BINARY_OP_EQ:
      return x == y;
    case BINARY_OP_GE:
      return x >= y;
    case BINARY_OP_LE:
      return x <= y;
    case BINARY_OP_NE:
      return x != y;
    case BINARY_OP_GT:
      return x > y;
    case BINARY_OP_LT:
      return x < y;
    case BINARY_OP_AND:
      return x && y;
    case BINARY_OP_OR:
      return x || y;
    default: throw;
  }
}

AstNode* UnaryConstNode(UnaryOperator op, AstNode* nd, const Location& loc) {
  DataType& type = nd->data_type;
  if (op == UNARY_OP_NEGATIVE && type == CONST_STRING_TYPE) {
    throw yy::parser::syntax_error(loc, "wrong type argument to unary minus");
  }
  ConstValue& val = std::get<ConstValue>(nd->semantic_value);
  switch (op) {
    case UNARY_OP_NEGATIVE: {
      if (type == INT_TYPE) {
        val = -std::get<int32_t>(val);
      } else {
        val = -std::get<FloatType>(val);
      }
      break;
    }
    case UNARY_OP_LOGICAL_NEGATION: {
      if (type == INT_TYPE) {
        val = !std::get<int32_t>(val);
      } else if (type == FLOAT_TYPE) {
        val = (int32_t)!std::get<FloatType>(val);
      } else {
        val = 1;
      }
      type = INT_TYPE;
    }
  }
  return nd;
}

AstNode* MergeConstNode(BinaryOperator op, AstNode* lhs, AstNode* rhs,
                        const Location& loc) {
  DataType ltype = lhs->data_type;
  DataType rtype = rhs->data_type;
  if (ltype == CONST_STRING_TYPE || rtype == CONST_STRING_TYPE) {
    std::string x = GetTypeName(ltype);
    std::string y = GetTypeName(rtype);
    std::string z = GetOpName(op);
    throw yy::parser::syntax_error(loc, "invalid operands to binary " + z + " (have '" + x + "' and '" + y + "')");
  }
  AstNode* node = new AstNode(CONST_VALUE_NODE, loc);
  try {
    node->data_type = MixDataType(lhs, rhs, op);
  } catch (const std::exception& e) {
    throw yy::parser::syntax_error(loc, e.what());
  }
  ConstValue& lcv = std::get<ConstValue>(lhs->semantic_value);
  ConstValue& rcv = std::get<ConstValue>(rhs->semantic_value);
  ConstValue cv{};
  bool b = IsLogicalOp(op);
  if (lhs->data_type == INT_TYPE) {
    if (rhs->data_type == INT_TYPE) {
      if (!b) {
        cv = DoArithmaticOperation(
            op, std::get<int32_t>(lcv), std::get<int32_t>(rcv));
      } else {
        cv = DoLogicalOperation(
            op, std::get<int32_t>(lcv), std::get<int32_t>(rcv));
      }
    } else {
      if (!b) {
        cv = DoArithmaticOperation(
            op, std::get<int32_t>(lcv), std::get<FloatType>(rcv));
      } else {
        cv = DoLogicalOperation(op, std::get<int32_t>(lcv),
                                std::get<FloatType>(rcv));
      }
    }
  } else {
    if (rhs->data_type == INT_TYPE) {
      if (!b) {
        cv = DoArithmaticOperation(op, std::get<FloatType>(lcv),
                                   std::get<int32_t>(rcv));
      } else {
        cv = DoLogicalOperation(op, std::get<FloatType>(lcv),
                                std::get<int32_t>(rcv));
      }
    } else {
      if (!b) {
        cv = DoArithmaticOperation(op, std::get<FloatType>(lcv),
                                   std::get<FloatType>(rcv));
      } else {
        cv = DoLogicalOperation(op, std::get<FloatType>(lcv),
                                std::get<FloatType>(rcv));
      }
    }
  }
  delete lhs;
  delete rhs;
  node->semantic_value = cv;
  return node;
}

AstNode* MakeTypeNode(DataType type, std::string&& s, const Location& loc) {
  AstNode* type_node = new AstNode(TYPE_NODE, loc);
  type_node->data_type = type;
  if (type == UNKNOWN_TYPE) {
    type_node->semantic_value = TypeSpecSemanticValue{s};
  } else {
    type_node->semantic_value = TypeSpecSemanticValue{type};
  }
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
                    const Location& loc) {
  AstNode* identifier = new AstNode(IDENTIFIER_NODE, loc);
  identifier->semantic_value = IdentifierSemanticValue{lexeme, id_kind};
  return identifier;
}

AstNode* MakeStmtNode(StmtKind stmt_kind, const Location& loc) {
  AstNode* stmt_node = new AstNode(STMT_NODE, loc);
  stmt_node->semantic_value = StmtSemanticValue{stmt_kind};
  return stmt_node;
}

AstNode* MakeDeclNode(DeclKind decl_kind, const Location& loc) {
  AstNode* decl_node = new AstNode(DECLARATION_NODE, loc);
  decl_node->semantic_value = DeclSemanticValue{decl_kind};
  return decl_node;
}

AstNode* MakeExprNode(ExprKind expr_kind, DataType data_type,
                      int operation_enum_value, const Location& loc,
                      std::list<AstNode*>&& ch) {
  bool const_eval = true;
  for (AstNode* nd : ch) const_eval &= nd->node_type == CONST_VALUE_NODE;
  if (const_eval) {
    if (ch.size() == 2) {
      return MergeConstNode(BinaryOperator(operation_enum_value), *ch.begin(),
                            *std::next(ch.begin()), loc);
    } else {
      return UnaryConstNode(UnaryOperator(operation_enum_value), *ch.begin(),
                            loc);
    }
  }
  AstNode* expr_node = new AstNode(EXPR_NODE, loc);
  expr_node->data_type = data_type;
  ExprSemanticValue expr{expr_kind};
  MakeChild(expr_node, ch);
  if (expr_kind == BINARY_OPERATION) {
    expr.op = BinaryOperator(operation_enum_value);
  } else {
    expr.op = UnaryOperator(operation_enum_value);
  }
  expr_node->semantic_value = expr;
  return expr_node;
}

}  // namespace

%}

// TODO: constant folding -> move to sematics??

%token <std::string> IDENTIFIER
%token <AstNode*> CONST

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
%type <AstNode*> relop_expr var_ref const_value
%type <AstNode*> param assign_expr cexpr cexpr_uni assign_expr_list
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
    MakeChild($$, {MakeTypeNode(type, std::move($1), @1), MakeIDNode($2, NORMAL_ID, @2), param, $7});
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
      throw yy::parser::syntax_error(@$, "parameter \'" + $2 + "\' has incomplete type");
    }
    MakeChild($$, {MakeTypeNode(type, std::move($1), @1), MakeIDNode($2, NORMAL_ID, @2)});
  } |
  IDENTIFIER IDENTIFIER dim_fn {
    /* e.g., int a[3], float b[][5], my_type c[][12][34] */
    $$ = MakeDeclNode(FUNCTION_PARAMETER_DECL, @$);
    AstNode *identifier = MakeIDNode($2, ARRAY_ID, @2);
    MakeChild(identifier, $3);
    MakeChild($$, {MakeTypeNode(GetTypedefValue($1), std::move($1), @1), identifier});
  };

dim_fn:
  S_L_BRACKET S_R_BRACKET { /* the first dimension can be empty */
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
    $3.push_front(MakeTypeNode(GetTypedefValue($2), std::move($2), @2));
    MakeChild($$, std::move($3));
  };

var_decl:
  IDENTIFIER init_id_list S_SEMICOLON {
    $$ = MakeDeclNode(VARIABLE_DECL, @$);
    DataType type = GetTypedefValue($1);
    if (type == VOID_TYPE) {
      AstNode* var = *$2.begin();
      const std::string& identifier_name =
          std::get<std::string>(std::get<IdentifierSemanticValue>(var->semantic_value)
              .identifier);
      throw yy::parser::syntax_error(
          @$, "variable or field \'" + identifier_name + "\' declared void");
    }
    $2.push_front(MakeTypeNode(GetTypedefValue($1), std::move($1), @1));
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
    $$ = MergeConstNode(BINARY_OP_ADD, $1, $3, @$);
  } |
  cexpr O_SUBTRACTION cexpr {
    $$ = MergeConstNode(BINARY_OP_SUB, $1, $3, @$);
  } |
  cexpr O_MULTIPLICATION cexpr {
    $$ = MergeConstNode(BINARY_OP_MUL, $1, $3, @$);
  } |
  cexpr O_DIVISION cexpr {
    $$ = MergeConstNode(BINARY_OP_DIV, $1, $3, @$);
  } |
  cexpr O_LOGICAL_OR cexpr {
    $$ = MergeConstNode(BINARY_OP_OR, $1, $3, @$);
  } |
  cexpr O_LOGICAL_AND cexpr {
    $$ = MergeConstNode(BINARY_OP_AND, $1, $3, @$);
  } |
  cexpr O_LESS_THAN cexpr {
    $$ = MergeConstNode(BINARY_OP_LT, $1, $3, @$);
  } |
  cexpr O_LESS_THAN_OR_EQ cexpr {
    $$ = MergeConstNode(BINARY_OP_LE, $1, $3, @$);
  } |
  cexpr O_GREATER_THAN cexpr {
    $$ = MergeConstNode(BINARY_OP_GT, $1, $3, @$);
  } |
  cexpr O_GREATER_THAN_OR_EQ cexpr {
    $$ = MergeConstNode(BINARY_OP_GE, $1, $3, @$);
  } |
  cexpr_uni {
    $$ = $1;
  };

cexpr_uni:
  O_SUBTRACTION cexpr_uni {
    $$ = UnaryConstNode(UNARY_OP_NEGATIVE, $2, @$);
  } |
  O_LOGICAL_NOT cexpr_uni {
    $$ = UnaryConstNode(UNARY_OP_LOGICAL_NEGATION, $2, @$);
  } |
  S_L_PAREN cexpr S_R_PAREN {
    $$ = $2;
  } |
  const_value {
    $$ = $1;
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
    $$ = MakeStmtNode(RETURN_STMT, @1);
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
  var_ref O_ASSIGN relop_expr {
    $$ = MakeStmtNode(ASSIGN_STMT, @$);
    MakeChild($$, {$1, $3});
  } |
  relop_expr {
    $$ = $1;
  };

relop_expr:
  relop_expr O_LOGICAL_OR relop_expr {
    $$ = MakeExprNode(BINARY_OPERATION, BOOLEAN_TYPE, BINARY_OP_OR, @$, {$1, $3});
  } |
  relop_expr O_LOGICAL_AND relop_expr {
    $$ = MakeExprNode(BINARY_OPERATION, BOOLEAN_TYPE, BINARY_OP_AND, @$, {$1, $3});
  } |
  relop_expr O_LESS_THAN relop_expr {
    $$ = MakeExprNode(BINARY_OPERATION, BOOLEAN_TYPE, BINARY_OP_LT, @$, {$1, $3});
  } |
  relop_expr O_LESS_THAN_OR_EQ relop_expr {
    $$ = MakeExprNode(BINARY_OPERATION, BOOLEAN_TYPE, BINARY_OP_LE, @$, {$1, $3});
  } |
  relop_expr O_GREATER_THAN relop_expr {
    $$ = MakeExprNode(BINARY_OPERATION, BOOLEAN_TYPE, BINARY_OP_GT, @$, {$1, $3});
  } |
  relop_expr O_GREATER_THAN_OR_EQ relop_expr {
    $$ = MakeExprNode(BINARY_OPERATION, BOOLEAN_TYPE, BINARY_OP_GE, @$, {$1, $3});
  } |
  relop_expr O_EQ relop_expr {
    $$ = MakeExprNode(BINARY_OPERATION, BOOLEAN_TYPE, BINARY_OP_EQ, @$, {$1, $3});
  } |
  relop_expr O_NOT_EQ relop_expr {
    $$ = MakeExprNode(BINARY_OPERATION, BOOLEAN_TYPE, BINARY_OP_NE, @$, {$1, $3});
  } |
  relop_expr O_ADDITION relop_expr {
    try {
      auto type = MixDataType($1, $3, BINARY_OP_ADD);
      $$ = MakeExprNode(BINARY_OPERATION, type, BINARY_OP_ADD, @$, {$1, $3});
    } catch (const std::exception &e) {
      throw yy::parser::syntax_error(@$, e.what());
    }
  } |
  relop_expr O_SUBTRACTION relop_expr {
    try {
      auto type = MixDataType($1, $3, BINARY_OP_SUB);
      $$ = MakeExprNode(BINARY_OPERATION, type, BINARY_OP_SUB, @$, {$1, $3});
    } catch (const std::exception &e) {
      throw yy::parser::syntax_error(@$, e.what());
    }
  } |
  relop_expr O_MULTIPLICATION relop_expr {
    try {
      auto type = MixDataType($1, $3, BINARY_OP_MUL);
      $$ = MakeExprNode(BINARY_OPERATION, type, BINARY_OP_MUL, @$, {$1, $3});
    } catch (const std::exception &e) {
      throw yy::parser::syntax_error(@$, e.what());
    }
  } |
  relop_expr O_DIVISION relop_expr {
    try {
      auto type = MixDataType($1, $3, BINARY_OP_DIV);
      $$ = MakeExprNode(BINARY_OPERATION, type, BINARY_OP_DIV, @$, {$1, $3});
    } catch (const std::exception &e) {
      throw yy::parser::syntax_error(@$, e.what());
    }
  } |
  unifact {
    $$ = $1;
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
  O_SUBTRACTION unifact {
    $$ = MakeExprNode(UNARY_OPERATION, $2->data_type, UNARY_OP_NEGATIVE, @$, {$2});
  } |
  O_LOGICAL_NOT unifact {
    $$ = MakeExprNode(UNARY_OPERATION, INT_TYPE, UNARY_OP_LOGICAL_NEGATION, @$, {$2});
  } |
  var_ref {
    $$ = $1;
  } |
  const_value {
    $$ = $1;
  } |
  S_L_PAREN relop_expr S_R_PAREN {
    $$ = $2;
  } |
  IDENTIFIER S_L_PAREN relop_expr_list S_R_PAREN {
    $$ = MakeStmtNode(FUNCTION_CALL_STMT, @$);
    MakeChild($$, {MakeIDNode($1, NORMAL_ID, @1), $3});
  };

const_value:
  O_ADDITION CONST {
    $$ = $2;
    assert($$->node_type == CONST_VALUE_NODE); 
    if ($$->data_type == CONST_STRING_TYPE) {
      throw yy::parser::syntax_error(@$, "wrong type argument to unary plus");
    }
  } | 
  CONST {
    $$ = $1;
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
  dim_list S_L_BRACKET relop_expr S_R_BRACKET {
    $$ = std::move($1);
    $$.push_back($3);
  } |
  S_L_BRACKET relop_expr S_R_BRACKET {
    $$ = {$2};
  };

%%

void yy::parser::error(const Location& l, const std::string& m) {
  drv.PrintError(l, m);
}
