#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <string>
#include <unordered_map>

#include "header.h"

namespace {

#define MD_PAIR(x) {x, #x}

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
  MD_PAIR(NONEMPTY_ASSIGN_EXPR_LIST_NODE),
  MD_PAIR(NONEMPTY_RELOP_EXPR_LIST_NODE)
};
static const std::unordered_map<DeclKind, std::string> kDeclTypeMap = {
  MD_PAIR(VARIABLE_DECL),
  MD_PAIR(TYPE_DECL),
  MD_PAIR(FUNCTION_DECL),
  MD_PAIR(FUNCTION_PARAMETER_DECL)
};
static const std::unordered_map<IdentifierKind, std::string> kIdTypeMap = {
  MD_PAIR(NORMAL_ID),
  MD_PAIR(ARRAY_ID),
  MD_PAIR(WITH_INIT_ID)
};
static const std::unordered_map<DataType, std::string> kDataTypeMap = {
  MD_PAIR(INT_TYPE),
  MD_PAIR(FLOAT_TYPE),
  MD_PAIR(VOID_TYPE),
  MD_PAIR(INT_PTR_TYPE),
  MD_PAIR(FLOAT_PTR_TYPE),
  MD_PAIR(CONST_STRING_TYPE),
  MD_PAIR(NONE_TYPE),
  MD_PAIR(UNKNOWN_TYPE)
};
static const std::unordered_map<StmtKind, std::string> kStmtTypeMap = {
  MD_PAIR(WHILE_STMT),
  MD_PAIR(FOR_STMT),
  MD_PAIR(ASSIGN_STMT),
  MD_PAIR(IF_STMT),
  MD_PAIR(IF_ELSE_STMT),
  MD_PAIR(FUNCTION_CALL_STMT),
  MD_PAIR(RETURN_STMT)
};

#undef MD_PAIR

void PrintLabelString(std::ostream &ofs, AstNode *node) {
  static const std::string kBinaryOpString[] = {
      "+", "-", "*", "/", "==", ">=", "<=", "!=", ">", "<", "&&", "||"};
  static const std::string kUnaryOpString[] = {"+", "-", "!"};
  ofs << kNodeTypeMap.at(node->node_type);
  switch (node->node_type) {
    case PROGRAM_NODE: break;
    case DECLARATION_NODE:
      ofs << ' ' << kDeclTypeMap.at(node->semantic_value.decl_semantic_value.kind);
      break;
    case IDENTIFIER_NODE:
      ofs << ' '
          << node->semantic_value.identifier_semantic_value.identifier_name
          << ' '
          << kIdTypeMap.at(node->semantic_value.identifier_semantic_value.kind);
      break;
    case PARAM_LIST_NODE: break;
    case NULL_NODE: break;
    case TYPE_NODE:
      ofs << ' ' << kDataTypeMap.at(node->data_type);
      break;
    case BLOCK_NODE: break;
    case VARIABLE_DECL_LIST_NODE: break;
    case STMT_LIST_NODE: break;
    case STMT_NODE:
      ofs << ' ' << kStmtTypeMap.at(node->semantic_value.stmt_semantic_value.kind);
      break;
    case EXPR_NODE:
      ofs << ' ';
      switch (node->semantic_value.expr_semantic_value.kind) {
        case BINARY_OPERATION:
          ofs << kBinaryOpString[node->semantic_value.expr_semantic_value.op
                                     .binary_op];
          break;
        case UNARY_OPERATION:
          ofs << kUnaryOpString[node->semantic_value.expr_semantic_value.op
                                    .unary_op];
          break;
      }
      break;
    case CONST_VALUE_NODE:
      ofs << ' ';
      switch (node->semantic_value.const1->const_type) {
        case INTEGERC:
          ofs << node->semantic_value.const1->const_u.intval;
          break;
        case FLOATC:
          ofs << node->semantic_value.const1->const_u.fval;
          break;
        case STRINGC:
          node->semantic_value.const1->const_u
              .sc[strlen(node->semantic_value.const1->const_u.sc) - 1] = 0;
          ofs << "\\\"" << (node->semantic_value.const1->const_u.sc + 1)
              << "\\\"";
          node->semantic_value.const1->const_u
              .sc[strlen(node->semantic_value.const1->const_u.sc)] = '"';
          node->semantic_value.const1->const_u
              .sc[strlen(node->semantic_value.const1->const_u.sc) + 1] = 0;
          break;
      }
      break;
    case NONEMPTY_ASSIGN_EXPR_LIST_NODE: break;
    case NONEMPTY_RELOP_EXPR_LIST_NODE: break;
    default:
      ofs << "default case in char *getLabelString(AST_TYPE astType)";
      break;
  }
}

int PrintGVNode(std::ostream &ofs, AstNode *node, int count,
    std::list<AstNode*>::iterator pos = std::list<AstNode*>::iterator()) {
  int current_node_count = count;

  ofs << "node" << count << " [label =\"";
  PrintLabelString(ofs, node);
  ofs << "\"]\n";
  ++count;
  if (node->child.size()) {
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

void Debug(AstNode* x) {
  PrintLabelString(std::cout, x);
  if (!x->child.size()) return;
  std::cout << " (\n";
  for (auto& i : x->child) {
    Debug(i); std::cout << ',';
  }
  std::cout << ')';
}

}  // namespace

void PrintGV(AstNode *root, std::string filename) {
  if (filename == "") filename = "AST_Graph.gv";
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
