#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <string>

#include "header.h"

void PrintLabelString(std::ofstream &ofs, AstNode *node) {
  static const std::string kBinaryOpString[] = {
      "+", "-", "*", "/", "==", ">=", "<=", "!=", ">", "<", "&&", "||"};
  static const std::string kUnaryOpString[] = {"+", "-", "!"};
  switch (node->node_type) {
    case PROGRAM_NODE:
      ofs << "PROGRAM_NODE";
      break;
    case DECLARATION_NODE:
      ofs << "DECLARATION_NODE ";
      switch (node->semantic_value.decl_semantic_value.kind) {
        case VARIABLE_DECL:
          ofs << "VARIABLE_DECL";
          break;
        case TYPE_DECL:
          ofs << "TYPE_DECL";
          break;
        case FUNCTION_DECL:
          ofs << "FUNCTION_DECL";
          break;
        case FUNCTION_PARAMETER_DECL:
          ofs << "FUNCTION_PARAMETER_DECL";
          break;
      }
      break;
    case IDENTIFIER_NODE:
      ofs << "IDENTIFIER_NODE ";
      ofs << node->semantic_value.identifier_semantic_value.identifier_name
          << ' ';
      switch (node->semantic_value.identifier_semantic_value.kind) {
        case NORMAL_ID:
          ofs << "NORMAL_ID";
          break;
        case ARRAY_ID:
          ofs << "ARRAY_ID";
          break;
        case WITH_INIT_ID:
          ofs << "WITH_INIT_ID";
          break;
      }
      break;
    case PARAM_LIST_NODE:
      ofs << "PARAM_LIST_NODE";
      break;
    case NULL_NODE:
      ofs << "NULL_NODE";
      break;
    case BLOCK_NODE:
      ofs << "BLOCK_NODE";
      break;
    case VARIABLE_DECL_LIST_NODE:
      ofs << "VARIABLE_DECL_LIST_NODE";
      break;
    case STMT_LIST_NODE:
      ofs << "STMT_LIST_NODE";
      break;
    case STMT_NODE:
      ofs << "STMT_NODE ";
      switch (node->semantic_value.stmt_semantic_value.kind) {
        case WHILE_STMT:
          ofs << "WHILE_STMT";
          break;
        case FOR_STMT:
          ofs << "FOR_STMT";
          break;
        case ASSIGN_STMT:
          ofs << "ASSIGN_STMT";
          break;
        case IF_STMT:
          ofs << "IF_STMT";
          break;
        case FUNCTION_CALL_STMT:
          ofs << "FUNCTION_CALL_STMT";
          break;
        case RETURN_STMT:
          ofs << "RETURN_STMT";
          break;
      }
      break;
    case EXPR_NODE:
      ofs << "EXPR_NODE ";
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
      ofs << "CONST_VALUE_NODE ";
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
    case NONEMPTY_ASSIGN_EXPR_LIST_NODE:
      ofs << "NONEMPTY_ASSIGN_EXPR_LIST_NODE";
      break;
    case NONEMPTY_RELOP_EXPR_LIST_NODE:
      ofs << "NONEMPTY_RELOP_EXPR_LIST_NODE";
      break;
    default:
      ofs << "default case in char *getLabelString(AST_TYPE astType)";
      break;
  }
}

int PrintGVNode(std::ofstream &ofs, AstNode *node, int count) {
  if (node == NULL) return count;
  int current_node_count = count;

  ofs << "node" << count << " [label =\"";
  PrintLabelString(ofs, node);
  ofs << "\"]\n";
  ++count;
  int count_after_check_children = count;
  if (node->child) {
    count_after_check_children = PrintGVNode(ofs, node->child, count);
    ofs << "node" << current_node_count << " -> node" << count
        << " [style = bold]\n";
  }

  int count_after_check_sibling = count_after_check_children;
  if (node->right_sibling) {
    count_after_check_sibling =
        PrintGVNode(ofs, node->right_sibling, count_after_check_children);
    ofs << "node" << current_node_count << " -> node"
        << count_after_check_children << " [style = dashed]\n";
  }

  return count_after_check_sibling;
}

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
}
