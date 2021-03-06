#include "gv.h"

#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <string>
#include <unordered_map>
#include <variant>

namespace {

#define MD_PAIR(x) \
  { x, #x }

static const std::unordered_map<AstType, std::string> kNodeTypeMap = {
    MD_PAIR(PROGRAM_NODE),
    MD_PAIR(DECLARATION_NODE),
    MD_PAIR(IDENTIFIER_NODE),
    MD_PAIR(PARAM_LIST_NODE),
    MD_PAIR(NULL_NODE),
    MD_PAIR(TYPE_NODE),
    MD_PAIR(BLOCK_NODE),
    MD_PAIR(VARIABLE_DECL_LIST_NODE),
    MD_PAIR(STMT_LIST_NODE),
    MD_PAIR(STMT_NODE),
    MD_PAIR(EXPR_NODE),
    MD_PAIR(CONST_VALUE_NODE),
    MD_PAIR(ASSIGN_EXPR_LIST_NODE),
    MD_PAIR(RELOP_EXPR_LIST_NODE),
    MD_PAIR(CONVERSION_NODE)};

static const std::unordered_map<DeclKind, std::string> kDeclTypeMap = {
    MD_PAIR(VARIABLE_DECL), MD_PAIR(TYPE_DECL), MD_PAIR(FUNCTION_DECL),
    MD_PAIR(FUNCTION_PARAMETER_DECL)};

static const std::unordered_map<IdentifierKind, std::string> kIdTypeMap = {
    MD_PAIR(NORMAL_ID), MD_PAIR(ARRAY_ID), MD_PAIR(WITH_INIT_ID)};

static const std::unordered_map<DataType, std::string> kDataTypeMap = {
    MD_PAIR(INT_TYPE),     MD_PAIR(FLOAT_TYPE),     MD_PAIR(VOID_TYPE),
    MD_PAIR(INT_PTR_TYPE), MD_PAIR(FLOAT_PTR_TYPE), MD_PAIR(CONST_STRING_TYPE),
    MD_PAIR(BOOLEAN_TYPE), MD_PAIR(NONE_TYPE),      MD_PAIR(UNKNOWN_TYPE)};

static const std::unordered_map<StmtKind, std::string> kStmtTypeMap = {
    MD_PAIR(WHILE_STMT), MD_PAIR(FOR_STMT),     MD_PAIR(ASSIGN_STMT),
    MD_PAIR(IF_STMT),    MD_PAIR(IF_ELSE_STMT), MD_PAIR(FUNCTION_CALL_STMT),
    MD_PAIR(RETURN_STMT)};

#undef MD_PAIR

void PrintLabelString(std::ostream &ofs, AstNode *node) {
  static const std::string kBinaryOpString[] = {
      "+", "-", "*", "/", "==", ">=", "<=", "!=", ">", "<", "&&", "||"};
  static const std::string kUnaryOpString[] = {"+", "-", "!"};
  ofs << kNodeTypeMap.at(node->node_type);
  switch (node->node_type) {
    case PROGRAM_NODE:
      break;
    case DECLARATION_NODE:
      ofs << ' '
          << kDeclTypeMap.at(
                 std::get<DeclSemanticValue>(node->semantic_value).kind);
      break;
    case IDENTIFIER_NODE:
      ofs << ' ';
      try {
        ofs << std::get<std::string>(
            std::get<IdentifierSemanticValue>(node->semantic_value).identifier);
      } catch (const std::bad_variant_access &) {
        ofs << std::get<Identifier>(
                   std::get<IdentifierSemanticValue>(node->semantic_value)
                       .identifier)
                   .first;
      }
      ofs << ' '
          << kIdTypeMap.at(
                 std::get<IdentifierSemanticValue>(node->semantic_value).kind);
      break;
    case PARAM_LIST_NODE:
    case NULL_NODE:
      break;
    case TYPE_NODE:
      ofs << ' ' << kDataTypeMap.at(node->data_type);
      break;
    case BLOCK_NODE:
    case VARIABLE_DECL_LIST_NODE:
    case STMT_LIST_NODE:
      break;
    case STMT_NODE:
      ofs << ' '
          << kStmtTypeMap.at(
                 std::get<StmtSemanticValue>(node->semantic_value).kind);
      break;
    case EXPR_NODE: {
      ofs << ' ';
      auto &op = std::get<ExprSemanticValue>(node->semantic_value).op;
      switch (std::get<ExprSemanticValue>(node->semantic_value).kind) {
        case BINARY_OPERATION:
          ofs << kBinaryOpString[std::get<BinaryOperator>(op)];
          break;
        case UNARY_OPERATION:
          ofs << kUnaryOpString[std::get<UnaryOperator>(op)];
          break;
      }
      break;
    }
    case CONST_VALUE_NODE: {
      ofs << ' ';
      ConstValue &cv = std::get<ConstValue>(node->semantic_value);
      switch (node->data_type) {
        case INT_TYPE:
          ofs << std::get<int>(cv);
          break;
        case FLOAT_TYPE:
          ofs << std::get<FloatType>(cv);
          break;
        case CONST_STRING_TYPE:
          ofs << "\\\"" << std::get<std::string>(cv) << "\\\"";
          break;
        default:
          throw std::invalid_argument("node->data_type");
      }
      break;
    }
    case ASSIGN_EXPR_LIST_NODE:
    case RELOP_EXPR_LIST_NODE:
      break;
    case CONVERSION_NODE: {
      auto &value = std::get<ConversionSemanticValue>(node->semantic_value);
      ofs << ' ' << kDataTypeMap.at(value.from) << ' '
          << kDataTypeMap.at(value.to);
      break;
    }
    default:
      ofs << "default case in char *getLabelString(AST_TYPE astType)";
      break;
  }
}

int PrintGVNode(
    std::ostream &ofs, AstNode *node, int count,
    std::list<AstNode *>::iterator pos = std::list<AstNode *>::iterator()) {
  int current_node_count = count;

  ofs << "node" << count << " [label =\"";
  PrintLabelString(ofs, node);
  ofs << "\"]\n";
  ++count;
  if (!node->child.empty()) {
    int tmp = count;
    count = PrintGVNode(ofs, *node->child.begin(), count, node->child.begin());
    ofs << "node" << current_node_count << " -> node" << tmp
        << " [style = bold]\n";
  }
  if (!node->parent) return count;

  if (++pos != node->parent->child.end()) {
    int tmp = count;
    count = PrintGVNode(ofs, *pos, count, pos);
    ofs << "node" << current_node_count << " -> node" << tmp
        << " [style = dashed]\n";
  }
  return count;
}

#ifndef NDEBUG
void Debug(AstNode *x) {
  PrintLabelString(std::cout, x);
  if (!x->child.size()) return;
  std::cout << " (\n";
  for (auto &i : x->child) {
    Debug(i);
    std::cout << ',';
  }
  std::cout << ')';
}
#endif  // NDEBUG

}  // namespace

void PrintGV(AstNode *root, std::string filename) {
  std::ofstream ofs(filename);
  if (!ofs) {
    std::cout << "Cannot open file \"" << filename << "\"" << std::endl;
    return;
  }
  ofs << "Digraph AST\n";
  ofs << "{\n";
  ofs << "label = \"" << filename << "\"\n";

  int node_count = 0;
  PrintGVNode(ofs, root, node_count);
  ofs << "}\n";
  // Debug(root); std::cout << '\n';
}
