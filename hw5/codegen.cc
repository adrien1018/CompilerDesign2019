#include "codegen.h"

#include <cassert>
#include <iostream>
#include <variant>

#include "ast.h"

// FIXME: temporarily ignore the allocations of the intermediates.
// TODO: the stack can be reused between blocks without nested relationships.

namespace {

template <class T>
T &GetAttribute(AstNode *id, std::vector<TableEntry> &tab) {
  assert(id->node_type == IDENTIFIER_NODE);
  auto &value = std::get<IdentifierSemanticValue>(id->semantic_value);
  return tab[std::get<Identifier>(value.identifier).first].GetValue<T>();
}

}  // namespace

void CodeGen::VisitStatement(AstNode *stmt, size_t &offset) {
  if (stmt->node_type == BLOCK_NODE) return VisitBlock(stmt, offset);
  if (stmt->node_type != STMT_NODE) return;
  auto &value = std::get<StmtSemanticValue>(stmt->semantic_value);
  switch (value.kind) {
    case WHILE_STMT:
    case FOR_STMT:
    case IF_STMT:
      VisitStatement(*std::prev(stmt->child.end()), offset);
      break;
    case IF_ELSE_STMT:
      VisitStatement(*std::prev(stmt->child.end(), 2), offset);
      VisitStatement(*std::prev(stmt->child.end()), offset);
      break;
  }
}

void CodeGen::VisitStmtList(AstNode *stmt_list, size_t &offset) {
  for (AstNode *stmt : stmt_list->child) VisitStatement(stmt, offset);
}

void CodeGen::VisitVariableDecl(AstNode *decl, size_t &offset) {
  for (auto it = std::next(decl->child.begin()); it != decl->child.end();
       it++) {
    const VariableAttr &attr = GetAttribute<VariableAttr>(*it, tab_);
    offset += attr.size;
  }
}

void CodeGen::VisitDeclList(AstNode *decl_list, size_t &offset) {
  for (AstNode *decl : decl_list->child) {
    DeclKind kind = std::get<DeclSemanticValue>(decl->semantic_value).kind;
    if (kind == VARIABLE_DECL) VisitVariableDecl(decl, offset);
  }
}

void CodeGen::VisitBlock(AstNode *block, size_t &offset) {
  for (AstNode *nd : block->child) {
    switch (nd->node_type) {
      case STMT_LIST_NODE:
        VisitStmtList(nd, offset);
        break;
      case VARIABLE_DECL_LIST_NODE:
        VisitDeclList(nd, offset);
        break;
    }
  }
}

void CodeGen::VisitFunctionDecl(AstNode *decl) {
  AstNode *id = *std::next(decl->child.begin());
  FunctionAttr &attr = GetAttribute<FunctionAttr>(id, tab_);
  AstNode *block = *std::prev(decl->child.end());
  size_t offset = 0;
  VisitBlock(block, offset);
  attr.sp_offset += offset;
  std::cerr << "fp_offset = " << attr.fp_offset
            << " sp_offset = " << attr.sp_offset << '\n';
}

void CodeGen::VisitGlobalDecl(AstNode *decl) {
  if (decl->node_type == VARIABLE_DECL_LIST_NODE) {
    for (AstNode *child : decl->child) VisitGlobalDecl(child);
    return;
  }
  DeclKind kind = std::get<DeclSemanticValue>(decl->semantic_value).kind;
  if (kind == FUNCTION_DECL) VisitFunctionDecl(decl);
}

void CodeGen::VisitProgram(AstNode *prog) {
  for (AstNode *decl : prog->child) VisitGlobalDecl(decl);
}

void CodeGen::GenerateVariableDecl(AstNode *var_decl, bool global) {
  if (global) {
    if (section_ != DATA_SECTION) {
      // TODO: generate `.data` marker
      ofs_ << ".data\n";
    }
    for (auto it = std::next(var_decl->child.begin());
         it != var_decl->child.end(); ++it) {
      AstNode *nd = *it;
      assert(nd->node_type == IDENTIFIER_NODE);
      auto &value = std::get<IdentifierSemanticValue>(nd->semantic_value);
      const TableEntry &entry =
          tab_[std::get<Identifier>(value.identifier).first];
      const VariableAttr &attr = entry.GetValue<VariableAttr>();
      if (attr.IsArray()) {
        // size_t sz = attr.GetSize();
        // TODO: generate `<id>: .space 4 * sz`
      } else {
        // TODO: generate `<id>: .word`
      }
    }
  } else {
    // TODO: declaring local variables
  }
}

void CodeGen::GenerateFunctionDecl(AstNode *func_decl) {
  if (section_ != TEXT_SECTION) {
    // TODO: generate `.text` marker
    ofs_ << ".text\n";
  }
}

void CodeGen::GenerateGlobalDecl(AstNode *decl) {
  if (decl->node_type == VARIABLE_DECL_LIST_NODE) {
    for (AstNode *child : decl->child) GenerateGlobalDecl(child);
    return;
  }
  DeclKind kind = std::get<DeclSemanticValue>(decl->semantic_value).kind;
  switch (kind) {
    case VARIABLE_DECL:
      GenerateVariableDecl(decl, true);
      break;
    case FUNCTION_DECL:
      GenerateFunctionDecl(decl);
      break;
  }
}

void CodeGen::GenerateProgram(AstNode *prog) {
  for (AstNode *decl : prog->child) GenerateGlobalDecl(decl);
}
