#include "analyze.h"

#include <cassert>
#include <stdexcept>
#include <variant>

#include "entry.h"

namespace {

std::vector<size_t> ParseDimDecl(const std::list<AstNode *> &dim_decl) {
  std::vector<size_t> dims;
  for (auto cexpr : dim_decl) {
    if (cexpr->node_type == NULL_NODE) {
      dims.push_back(0);
      continue;
    }
    assert(cexpr->node_type == CONST_VALUE_NODE);
    if (cexpr->data_type != INT_TYPE) {
      // TODO: Throw error - size of array has non-integer type.
    }
    ConstValue &cv = std::get<ConstValue>(cexpr->semantic_value);
    int size = std::get<int>(cv);
    if (size < 0) {
      // TODO: Throw error - size of array is negative.
    }
    dims.push_back(size);
  }
  return dims;
}

void BuildInitID(AstNode *init_id, DataType type, SymbolTable *tab) {
  assert(init_id->node_type == IDENTIFIER_NODE);
  auto &value = std::get<IdentifierSemanticValue>(init_id->semantic_value);
  if (value.kind == NORMAL_ID || value.kind == WITH_INIT_ID) {
    TableEntry entry = BuildEntry<VARIABLE>(type);
    // TODO: insert variable declaration into the symbol table
  } else {
    std::vector<size_t> dims = ParseDimDecl(init_id->child);
    TableEntry entry = BuildEntry<VARIABLE>(type, std::move(dims));
    // TODO: insert array declaration into the symbol table
  }
}

void BuildVariableDecl(AstNode *var_decl, SymbolTable *tab) {
  assert(!var_decl->child.empty());
  AstNode *type_node = *var_decl->child.begin();
  DataType type = QueryType(tab, type_node);
  for (auto it = std::next(var_decl->child.begin());
       it != var_decl->child.end(); it++) {
    AstNode *init_id = *it;
    BuildInitID(init_id, type, tab);
  }
}

void BuildTypedefID(AstNode *id_item, DataType type) {
  assert(id_item->node_type == IDENTIFIER_NODE);
  auto &value = std::get<IdentifierSemanticValue>(id_item->semantic_value);
  if (value.kind == ARRAY_ID) {
    // TODO
  } else {
    TableEntry entry = BuildEntry<TYPE_ALIAS>(type);
    // TODO: insert type declaration into the symbol table
  }
}

void BuildTypeDecl(AstNode *type_decl, SymbolTable *tab) {
  assert(!type_decl->child.empty());
  AstNode *type_node = *type_decl->child.begin();
  DataType type = QueryType(tab, type_node);
  for (auto it = std::next(type_decl->child.begin());
       it != type_decl->child.end(); it++) {
    AstNode *id_item = *it;
    BuildTypedefID(id_item, type);
  }
}

VariableType ParseParam(AstNode *param, SymbolTabol *tab) {
  DataType type = QueryType(tab, *param->child.begin());
  AstNode *identifier = *std::next(param->child.begin());
  if (std::get<IdentifierSemanticValue>(identifier->semantic_value).kind ==
      NORMAL_ID) {
    return VariableType(type);
  }
  auto dims = ParseDimDecl(identifier->child);
  return VariableType(type, std::move(dims));
}

void BuildFunctionDecl(AstNode *func_decl, SymbolTable *tab) {
  auto it = func_decl->child.begin();
  AstNode *type_node = *it++;
  DataType type = QueryType(tab, type_node);
  AstNode *id_node = *it++;
  std::string &func_name =
      std::get<IdentifierSemanticValue>(id_node->semantic_value)
          .identifier_name;
  std::vector<VariableType> param_list;
  while (it != std::prev(func_decl->child.end())) {
    AstNode *param = *it;
    param_list.push_back(ParseParam(param, tab));
  }
  // TODO: insert function declaration into the symbol table
}

void BuildGlobalDecl(AstNode *decl, SymbolTable *tab) {
  for (auto it = decl->child.begin(); it != decl->child.end(); ++it) {
    AstNode *child = *it;
    try {
      DeclKind kind = std::get<DeclSemanticValue>(child->semantic_value).kind;
      if (kind == VARIABLE_DECL) {
        BuildVariableDecl(child, tab);
      } else if (kind == TYPE_DECL) {
        BuildTypeDecl(child, tab);
      } else if (kind == FUNCTION_DECL) {
        BuildFunctionDecl(child, tab);
      } else {
        throw std::invalid_argument(
            "Function parameter declaration in global decl list");
      }
    } catch (...) {
      throw;
    }
  }
}

void BuildProgram(AstNode *prog, SymbolTable *tab) {
  for (auto it = prog->child.begin(); it != prog->child.end(); ++it) {
    BuildGlobalDecl(*it, tab);
  }
}

}  // namespace

SymbolTable *BuildSymbolTable(AstNode *prog) {
  SymbolTable *tab = new SymbolTable();
  BuildProgram(prog, tab);
}
