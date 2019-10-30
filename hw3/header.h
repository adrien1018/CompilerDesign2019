#ifndef HEADER_H_
#define HEADER_H_

#define MAX_ARRAY_DIMENSION 7

#include <string>

enum DataType {
  INT_TYPE,
  FLOAT_TYPE,
  VOID_TYPE,
  INT_PTR_TYPE,       // for parameter passing
  FLOAT_PTR_TYPE,     // for parameter passing
  CONST_STRING_TYPE,  // for "const string"
  NONE_TYPE,          // for nodes like PROGRAM_NODE which has no type
  ERROR_TYPE
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

// C_type = type of constant ex: 1, 3.3, "const string"
// do not modify, or lexer might break
enum CType { INTEGERC, FLOATC, STRINGC };

enum StmtKind {
  WHILE_STMT,
  FOR_STMT,
  ASSIGN_STMT,  // TODO: for simpler implementation, assign_expr also uses this
  IF_STMT,
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
  BLOCK_NODE,
  VARIABLE_DECL_LIST_NODE,
  STMT_LIST_NODE,
  STMT_NODE,
  EXPR_NODE,
  CONST_VALUE_NODE,  // ex:1, 2, "constant string"
  NONEMPTY_ASSIGN_EXPR_LIST_NODE,
  NONEMPTY_RELOP_EXPR_LIST_NODE
};

//*************************
// AstNode's semantic value
//*************************

struct StmtSemanticValue {
  StmtKind kind;
};

struct ExprSemanticValue {
  ExprKind kind;

  int is_const_eval;

  union {
    int ivalue;
    float fvalue;
  } const_eval_value;

  union {
    BinaryOperator binary_op;
    UnaryOperator unary_op;
  } op;
};

struct DeclSemanticValue {
  DeclKind kind;
};

struct SymbolAttribute;

struct IdentifierSemanticValue {
  std::string identifier_name;
  struct SymbolTableEntry *symboltable_entry;
  IdentifierKind kind;
};

struct TypeSpecSemanticValue {
  char *typeName;
};

// don't modify or lexer may break
struct ConstType {
  CType const_type;
  union {
    int intval;
    double fval;
    char *sc;
  } const_u;
};

struct AstNode {
  struct AstNode *child;
  struct AstNode *parent;
  struct AstNode *right_sibling;
  struct AstNode *leftmost_sibling;
  AstType node_type;
  DataType data_type;
  int linenumber;
  struct {
    IdentifierSemanticValue identifier_semantic_value;
    StmtSemanticValue stmt_semantic_value;
    DeclSemanticValue decl_semantic_value;
    ExprSemanticValue expr_semantic_value;
    ConstType *const1;
  } semantic_value;
};

AstNode *Allocate(AstType type);
void semanticAnalysis(AstNode *root);

#endif  // HEADER_H_
