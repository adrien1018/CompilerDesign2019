#ifndef AST_H_
#define AST_H_

#include <cstring>
#include <list>
#include <string>
#include <variant>

#include "symtab.h"

using FloatType = double;

struct Location {
  struct Position {
    size_t line, column, offset;
    Position() : line(1), column(1), offset(0) {}
  } begin, end;
  void step() { begin = end; }
};

template <typename YYChar>
std::basic_ostream<YYChar>& operator<<(std::basic_ostream<YYChar>& ostr,
                                       const Location::Position& pos) {
  return ostr << pos.line << '.' << pos.column;
}

template <typename YYChar>
std::basic_ostream<YYChar>& operator<<(std::basic_ostream<YYChar>& ostr,
                                       const Location& loc) {
  unsigned end_col = 0 < loc.end.column ? loc.end.column - 1 : 0;
  ostr << loc.begin;
  if (loc.begin.line < loc.end.line) {
    ostr << '-' << loc.end.line << '.' << end_col;
  } else if (loc.begin.column < end_col) {
    ostr << '-' << end_col;
  }
  return ostr;
}

enum DataType {
  INT_TYPE,
  FLOAT_TYPE,
  VOID_TYPE,
  INT_PTR_TYPE,       // for parameter passing
  FLOAT_PTR_TYPE,     // for parameter passing
  CONST_STRING_TYPE,  // for "const string"
  NONE_TYPE,          // for nodes like PROGRAM_NODE which has no type
  UNKNOWN_TYPE        // typedef; leave it to sematics stage
};

enum IdentifierKind {
  NORMAL_ID,     // function Name, uninitialized scalar variable
  ARRAY_ID,      // ID_NODE->child = dim
  WITH_INIT_ID,  // ID_NODE->child = initial value
};

enum BinaryOperator {
  BINARY_OP_ADD,
  BINARY_OP_SUB,
  BINARY_OP_MUL,
  BINARY_OP_DIV,
  BINARY_OP_EQ,
  BINARY_OP_GE,
  BINARY_OP_LE,
  BINARY_OP_NE,
  BINARY_OP_GT,
  BINARY_OP_LT,
  BINARY_OP_AND,
  BINARY_OP_OR
};

enum UnaryOperator {
  UNARY_OP_POSITIVE,
  UNARY_OP_NEGATIVE,
  UNARY_OP_LOGICAL_NEGATION
};

enum StmtKind {
  WHILE_STMT,
  FOR_STMT,
  ASSIGN_STMT,  // TODO: for simpler implementation, assign_expr also uses this
  IF_STMT,
  IF_ELSE_STMT,
  FUNCTION_CALL_STMT,
  RETURN_STMT,
};

enum ExprKind { BINARY_OPERATION, UNARY_OPERATION };

enum DeclKind {
  VARIABLE_DECL,
  TYPE_DECL,
  FUNCTION_DECL,
  FUNCTION_PARAMETER_DECL
};

enum AstType {
  PROGRAM_NODE,
  DECLARATION_NODE,
  IDENTIFIER_NODE,
  PARAM_LIST_NODE,
  NULL_NODE,
  TYPE_NODE,
  BLOCK_NODE,
  VARIABLE_DECL_LIST_NODE,
  STMT_LIST_NODE,
  STMT_NODE,
  EXPR_NODE,
  CONST_VALUE_NODE,  // ex:1, 2, "constant string"
  NONEMPTY_ASSIGN_EXPR_LIST_NODE,
  NONEMPTY_RELOP_EXPR_LIST_NODE
};

struct StmtSemanticValue {
  StmtKind kind;
};

struct ExprSemanticValue {
  ExprKind kind;
  bool is_const_eval;
  std::variant<BinaryOperator, UnaryOperator> op;
  std::variant<int, FloatType> const_eval;
};

struct DeclSemanticValue {
  DeclKind kind;
};

using Identifier =
    std::pair<size_t, SymbolMap<std::string::value_type>::StringRef>;

struct IdentifierSemanticValue {
  std::variant<std::string, Identifier> identifier;
  IdentifierKind kind;
};

struct TypeSpecSemanticValue {
  std::variant<std::string, DataType, size_t> type;
};

using ConstValue = std::variant<int, FloatType, std::string>;

struct AstNode {
  std::list<AstNode*> child;
  AstNode* parent;
  AstType node_type;
  DataType data_type;
  Location loc;
  std::variant<IdentifierSemanticValue, StmtSemanticValue, DeclSemanticValue,
               ExprSemanticValue, ConstValue, TypeSpecSemanticValue>
      semantic_value;

  AstNode() : parent(nullptr), data_type(NONE_TYPE) {}
  AstNode(AstType type, const Location& loc)
      : parent(nullptr), node_type(type), data_type(NONE_TYPE), loc(loc) {}
};

#endif  // AST_H_
