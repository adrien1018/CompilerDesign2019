#include "analyze.h"

#include <cassert>
#include <stdexcept>

namespace {

void BuildBlock(AstNode* block, SymTab& tab, SymMap& mp, ErrList& err);

// TODO: Parse all assignment to convert identifier name to ID
// Note that in initialized declaration, value must be parsed before declaration

inline DataType QueryType(AstNode* nd, const SymTab& tab, const SymMap& mp,
                          ErrList& err) {
  size_t id = mp.Query(std::get<std::string>(
      std::get<TypeSpecSemanticValue>(nd->semantic_value).type));
  if (id == SymMap::npos) {
    err.emplace_back(/* TODO: undeclared */);
    return UNKNOWN_TYPE;
  } else {
    return tab[id].GetValue<AliasType>().canonical_type;
  }
}

inline void InsertSymTab(std::variant<std::string, size_t> id,
                         TableEntry&& entry, SymTab& tab, SymMap& mp,
                         ErrList& err) {
  auto id_num = mp.Insert(std::move(std::get<std::string>(id)));
  if (id_num.second) {
    tab.emplace_back(std::move(entry));
    id = id_num.first;
  } else {
    err.emplace_back(/* TODO: redeclared error */);
  }
}

std::vector<size_t> ParseDimDecl(const std::list<AstNode*>& dim_decl) {
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
    ConstValue& cv = std::get<ConstValue>(cexpr->semantic_value);
    int size = std::get<int>(cv);
    if (size < 0) {
      // TODO: Throw error - size of array is negative.
    }
    dims.push_back(size);
  }
  return dims;
}

void BuildInitID(AstNode* init_id, DataType type, SymTab& tab, SymMap& mp,
                 ErrList& err) {
  assert(init_id->node_type == IDENTIFIER_NODE);
  auto& value = std::get<IdentifierSemanticValue>(init_id->semantic_value);
  if (value.kind == NORMAL_ID || value.kind == WITH_INIT_ID) {
    InsertSymTab(value.identifier, BuildEntry<VARIABLE>(type), tab, mp, err);
  } else {
    std::vector<size_t> dims = ParseDimDecl(init_id->child);
    InsertSymTab(value.identifier, BuildEntry<VARIABLE>(type, std::move(dims)),
                 tab, mp, err);
  }
}

void BuildVariableDecl(AstNode* var_decl, SymTab& tab, SymMap& mp,
                       ErrList& err) {
  assert(!var_decl->child.empty());
  AstNode* type_node = *var_decl->child.begin();
  DataType type = QueryType(type_node, tab, mp, err);
  for (auto it = std::next(var_decl->child.begin());
       it != var_decl->child.end(); it++) {
    AstNode* init_id = *it;
    BuildInitID(init_id, type, tab, mp, err);
  }
}

void BuildTypedefID(AstNode* id_item, DataType type, SymTab& tab, SymMap& mp,
                    ErrList& err) {
  assert(id_item->node_type == IDENTIFIER_NODE);
  auto& value = std::get<IdentifierSemanticValue>(id_item->semantic_value);
  if (value.kind == ARRAY_ID) {
    // TODO
  } else {
    InsertSymTab(value.identifier, BuildEntry<TYPE_ALIAS>(type), tab, mp, err);
  }
}

void BuildTypeDecl(AstNode* type_decl, SymTab& tab, SymMap& mp, ErrList& err) {
  assert(!type_decl->child.empty());
  AstNode* type_node = *type_decl->child.begin();
  DataType type = QueryType(type_node, tab, mp, err);
  for (auto it = std::next(type_decl->child.begin());
       it != type_decl->child.end(); it++) {
    AstNode* id_item = *it;
    BuildTypedefID(id_item, type, tab, mp, err);
  }
}

VariableType ParseParam(AstNode* param, SymTab& tab, SymMap& mp, ErrList& err) {
  DataType type = QueryType(*param->child.begin(), tab, mp, err);
  AstNode* identifier = *std::next(param->child.begin());
  if (std::get<IdentifierSemanticValue>(identifier->semantic_value).kind ==
      NORMAL_ID) {
    return VariableType(type);
  }
  auto dims = ParseDimDecl(identifier->child);
  return VariableType(type, std::move(dims));
}

void BuildStatement(AstNode* stmt, SymTab& tab, SymMap& mp, ErrList& err) {
  if (stmt->node_type == STMT_NODE) {
    auto& value = std::get<StmtSemanticValue>(stmt->semantic_value);
    AstNode* sub_stmt = nullptr;
    switch (value.kind) {
      case WHILE_STMT:
      case FOR_STMT:
      case IF_STMT:
        sub_stmt = *std::prev(stmt->child.end());
        BuildStatement(sub_stmt, tab, mp, err);
        break;
      case IF_ELSE_STMT:
        sub_stmt = *std::prev(std::prev(stmt->child.end()));
        BuildStatement(sub_stmt, tab, mp, err);
        sub_stmt = *std::prev(stmt->child.end());
        BuildStatement(sub_stmt, tab, mp, err);
        break;
      case ASSIGN_STMT:
      case FUNCTION_CALL_STMT:
      case RETURN_STMT:
        break;
    }
  } else {
    if (stmt->node_type == BLOCK_NODE) BuildBlock(stmt, tab, mp, err);
  }
}

void BuildStmtList(AstNode* stmt_list, SymTab& tab, SymMap& mp, ErrList& err) {
  for (AstNode* stmt : stmt_list->child) BuildStatement(stmt, tab, mp, err);
}

void BuildDeclList(AstNode* decl_list, SymTab& tab, SymMap& mp, ErrList& err) {
  for (AstNode* child : decl_list->child) {
    DeclKind kind = std::get<DeclSemanticValue>(child->semantic_value).kind;
    if (kind == VARIABLE_DECL) {
      BuildVariableDecl(child, tab, mp, err);
    } else if (kind == TYPE_DECL) {
      BuildTypeDecl(child, tab, mp, err);
    }
  }
}

void BuildBlock(AstNode* block, SymTab& tab, SymMap& mp, ErrList& err) {
  mp.PushScope();
  for (AstNode* node : block->child) {
    switch (node->node_type) {
      case STMT_LIST_NODE:
        BuildStmtList(node, tab, mp, err);
        break;
      case VARIABLE_DECL_LIST_NODE:
        BuildDeclList(node, tab, mp, err);
        break;
      default:;
    }
  }
  mp.PopScope();
}

void BuildFunctionDecl(AstNode* func_decl, SymTab& tab, SymMap& mp,
                       ErrList& err) {
  auto it = func_decl->child.begin();
  AstNode* type_node = *it++;
  DataType type = QueryType(type_node, tab, mp, err);
  AstNode* id_node = *it++;
  auto& func_name =
      std::get<IdentifierSemanticValue>(id_node->semantic_value).identifier;
  std::vector<VariableType> param_list;
  for (; it != std::prev(func_decl->child.end()); ++it) {
    param_list.push_back(ParseParam(*it, tab, mp, err));
  }
  InsertSymTab(func_name, BuildEntry<FUNCTION>(type, std::move(param_list)),
               tab, mp, err);
  BuildBlock(*it, tab, mp, err);
}

void BuildGlobalDecl(AstNode* decl, SymTab& tab, SymMap& mp, ErrList& err) {
  for (auto it = decl->child.begin(); it != decl->child.end(); ++it) {
    AstNode* child = *it;
    DeclKind kind = std::get<DeclSemanticValue>(child->semantic_value).kind;
    switch (kind) {
      case VARIABLE_DECL:
        BuildVariableDecl(child, tab, mp, err);
        break;
      case TYPE_DECL:
        BuildTypeDecl(child, tab, mp, err);
        break;
      case FUNCTION_DECL:
        BuildFunctionDecl(child, tab, mp, err);
        break;
      default:;
    }
  }
}

void BuildProgram(AstNode* prog, SymTab& tab, SymMap& mp, ErrList& err) {
  for (auto it = prog->child.begin(); it != prog->child.end(); ++it) {
    BuildGlobalDecl(*it, tab, mp, err);
  }
}

void AnalyzeBlock(AstNode* block, const SymTab& tab, ErrList& err);
void AnalyzeStatement(AstNode* stmt, const SymTab& tab, ErrList& err);

void AnalyzeVarRef(AstNode* var, const SymTab& tab, ErrList& err) {
  auto& value = std::get<IdentifierSemanticValue>(var->semantic_value);
  switch (value.kind) {
    case NORMAL_ID:
    case ARRAY_ID:
  }
}

void AnalyzeRelopExpr(AstNode* expr, const SymTab& tab, ErrList& err) {
  switch (expr->node_type) {
    case EXPR_NODE: {
      auto& value = std::get<ExprSemanticValue>(expr->semantic_value);
      switch (value.kind) {
        case BINARY_OPERATION:
          AnalyzeRelopExpr(*(expr->child.begin()), tab, err);
          AnalyzeRelopExpr(*std::next(expr->child.begin()), tab, err);
          break;
        case UNARY_OPERATION:
          AnalyzeRelopExpr(*(expr->child.begin()), tab, err);
          break;
      }
      break;
    }
    case CONST_VALUE_NODE:
      break;
    case STMT_NODE:
      AnalyzeStatement(expr, tab, err);
      break;
    case IDENTIFIER_NODE:
      AnalyzeVarRef(expr, tab, err);
      break;
  }
}

void AnalyzeWhileStmt(AstNode* stmt, const SymTab& tab, ErrList& err) {
  AstNode* relop_expr = *stmt->child.begin();
  AnalyzeRelopExpr(relop_expr, tab, err);
  AnalyzeStatement(*std::next(stmt->child.begin()), tab, err);
}

void AnalyzeStatement(AstNode* stmt, const SymTab& tab, ErrList& err) {
  if (stmt->node_type == STMT_NODE) {
    auto& value = std::get<StmtSemanticValue>(stmt->semantic_value);
    AstNode* sub_stmt = nullptr;
    switch (value.kind) {
      case WHILE_STMT:
        AnalzeWhileStmt(stmt, tab, err);
        break;
      case FOR_STMT:
        AnalzeForStmt(stmt, tab, err);
        break;
      case IF_STMT:
        sub_stmt = *std::prev(stmt->child.end());
        BuildStatement(sub_stmt, tab, mp, err);
        break;
      case IF_ELSE_STMT:
        sub_stmt = *std::prev(std::prev(stmt->child.end()));
        BuildStatement(sub_stmt, tab, mp, err);
        sub_stmt = *std::prev(stmt->child.end());
        BuildStatement(sub_stmt, tab, mp, err);
        break;
      case ASSIGN_STMT:
      case FUNCTION_CALL_STMT:
      case RETURN_STMT:
        break;
    }
  } else {
    if (stmt->node_type == BLOCK_NODE) AnalyzeBlock(stmt, tab, mp, err);
  }
}

void AnalyzeStmtList(AstNode* stmt_list, const SymTab& tab, ErrList& err) {
  for (AstNode* stmt : stmt_list->child) AnalyzeStatement(stmt, tab, err);
}

void AnalyzeBlock(AstNode* block, const SymTab& tab, ErrList& err) {
  for (AstNode* node : block->child) {
    switch (node->node_type) {
      case STMT_LIST_NODE:
        AnalyzeStmtList(node, tab, err);
        break;
      case VARIABLE_DECL_LIST_NODE:
        AnalyzeDeclList(node, tab, err);
        break;
      default:;
    }
  }
}

void AnalyzeFunctionDecl(AstNode* func, const SymTab& tab, ErrList& err) {
  AstNode* id_node = *std::next(func->child.begin());
  size_t func_id = std::get<size_t>(
      std::get<IdentifierSemanticValue>(id_node->semantic_value).identifier);
  FunctionType entry = tab[func_id].GetValue<FunctionType>();
  AnalyzeBlock(*std::prev(func->child.end()), tab, err);
  // TODO: Pop scope of the function parameters
}

void AnalyzeGlobalDecl(AstNode* decl, const SymTab& tab, ErrList& err) {
  for (auto it = decl->child.begin(); it != decl->child.end(); ++it) {
    AstNode* child = *it;
    DeclKind kind = std::get<DeclSemanticValue>(child->semantic_value).kind;
    if (kind == FUNCTION_DECL) {
      AnalyzeFunctionDecl(child, tab, err);
    }
  }
}

void AnalyzeProgram(AstNode* prog, const SymTab& tab, ErrList& err) {
  for (auto it = prog->child.begin(); it != prog->child.end(); ++it) {
    AnalyzeGlobalDecl(*it, tab, err);
  }
}

}  // namespace

std::pair<SymTab, ErrList> BuildSymbolTable(AstNode* prog) {
  SymTab tab;
  SymMap mp;
  ErrList err;
  BuildProgram(prog, tab, mp, err);
  return std::make_pair(std::move(tab), std::move(err));
}

void SemanticAnalyze(AstNode* prog, const SymTab& tab, ErrList& err) {
  AnalyzeProgram(prog, tab, err);
}
