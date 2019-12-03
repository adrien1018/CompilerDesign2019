#include "analysis.h"

#include <cassert>
#include <iostream>
#include <stdexcept>
#include <utility>

/*** Note
 * Errors being caught in the first pass:
 *  - 1.a ID <name> undeclared.
 *  - 1.b ID <name> redeclared.
 *  - 2.a too few (many) arguments to function <name>
 *  - 3.a Incompatible array dimensions.
 *
 * Errors that should be caught in the second pass:
 *  - 2.b Incompatible return type (warning).
 *  - 3.b Array subscript is not an integer
 *  - 3.c Array <name> passed to scalar parameter <name>. / Scalar <name> passed
 * to array parameter <name>.
 */

// TODO: Catch errors in the second pass.
// TODO: Catch more errors that requested in the spec.
// TODO: Under what conditions shall we ignore the errors and keep analyzing?

namespace {

void BuildBlock(AstNode* block, SymTab& tab, SymMap& mp, ErrList& err);
void BuildRelopExpr(AstNode* expr, SymTab& tab, SymMap& mp, ErrList& err);
void BuildStatement(AstNode* stmt, SymTab& tab, SymMap& mp, ErrList& err);
void BuildRelopExprList(AstNode* relop_expr_list, SymTab& tab, SymMap& mp,
                        ErrList& err);

// TODO: Parse all assignment to convert identifier name to ID

inline DataType QueryType(AstNode* nd, const SymTab& tab, const SymMap& mp,
                          ErrList& err) {
  auto& value = std::get<TypeSpecSemanticValue>(nd->semantic_value);
  try {
    std::string type_name = std::get<std::string>(value.type);
    size_t id = mp.Query(type_name);
    if (id == SymMap::npos) {
      std::cerr << "[Error] type " << type_name << " undeclared" << std::endl;
      err.emplace_back(/* TODO: undeclared */);
      return UNKNOWN_TYPE;
    } else {
      value.type = tab[id].GetValue<AliasType>().canonical_type;
      return std::get<DataType>(value.type);
    }
  } catch (const std::bad_variant_access& e) {
    return std::get<DataType>(value.type);
  }
}

inline void InsertSymTab(std::variant<std::string, size_t>& id,
                         TableEntry&& entry, SymTab& tab, SymMap& mp,
                         ErrList& err) {
  auto id_num = mp.Insert(std::move(std::get<std::string>(id)));
  if (id_num.second) {
    tab.emplace_back(std::move(entry));
    id = id_num.first;
  } else {
    std::cerr << "[Error] Redeclaration" << std::endl;
    err.emplace_back(/* TODO: redeclared error */);
  }
}

std::vector<size_t> ParseDimDecl(const std::list<AstNode*>& dim_decl,
                                 ErrList& err) {
  std::vector<size_t> dims;
  for (auto cexpr : dim_decl) {
    if (cexpr->node_type == NULL_NODE) {
      dims.push_back(0);
      continue;
    }
    assert(cexpr->node_type == CONST_VALUE_NODE);
    if (cexpr->data_type != INT_TYPE) {
      std::cerr << "[Error] size of array has non-integer type" << std::endl;
      // TODO: Throw error - size of array has non-integer type.
    }
    ConstValue& cv = std::get<ConstValue>(cexpr->semantic_value);
    int size = std::get<int>(cv);
    if (size < 0) {
      std::cerr << "[Error] size of array is negative" << std::endl;
      // TODO: Throw error - size of array is negative.
    }
    dims.push_back(size);
  }
  return dims;
}

void BuildInitID(AstNode* init_id, DataType type, SymTab& tab, SymMap& mp,
                 ErrList& err) {
  std::cerr << "BuildInitID" << std::endl;
  assert(init_id->node_type == IDENTIFIER_NODE);
  auto& value = std::get<IdentifierSemanticValue>(init_id->semantic_value);
  if (value.kind == NORMAL_ID || value.kind == WITH_INIT_ID) {
    if (value.kind == WITH_INIT_ID) {
      AstNode* init_val = *init_id->child.begin();
      BuildRelopExpr(init_val, tab, mp, err);
    }
    InsertSymTab(value.identifier, BuildEntry<VARIABLE>(type), tab, mp, err);
  } else {
    std::vector<size_t> dims = ParseDimDecl(init_id->child, err);
    InsertSymTab(value.identifier, BuildEntry<VARIABLE>(type, std::move(dims)),
                 tab, mp, err);
  }
}

void BuildVariableDecl(AstNode* var_decl, SymTab& tab, SymMap& mp,
                       ErrList& err) {
  std::cerr << "BuildVariableDecl" << std::endl;
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
  auto& value = std::get<IdentifierSemanticValue>(identifier->semantic_value);
  if (value.kind == NORMAL_ID) {
    InsertSymTab(value.identifier, BuildEntry<VARIABLE>(type), tab, mp, err);
    return VariableType(type);
  } else {
    auto dims = ParseDimDecl(identifier->child, err);
    VariableType res(type, std::move(dims));
    InsertSymTab(value.identifier, BuildEntry<ARRAY>(res), tab, mp, err);
    return res;
  }
}

void BuildVarRef(AstNode* node, SymTab& tab, SymMap& mp, ErrList& err) {
  std::cerr << "BuildVarRef" << std::endl;
  auto& value = std::get<IdentifierSemanticValue>(node->semantic_value);
  const std::string& name = std::get<std::string>(value.identifier);
  size_t id = mp.Query(name);
  if (id == SymMap::npos) {
    std::cerr << "[Error] " << name << " undeclared" << std::endl;
    // TODO: Error - `name` undeclared.
  } else {
    const TableEntry& entry = tab[id];
    if (entry.GetType() != VARIABLE && entry.GetType() != ARRAY) {
      std::cerr << "[Error] " << name << " should be declared as variable"
                << std::endl;
      // TODO: Error - `name` should be declared as variable.
    }
    const VariableType& var = entry.GetValue<VariableType>();
    if (node->child.size() != var.GetDimension()) {
      if (var.IsArray())
        std::cerr << "[Error] Incompatible array dimensions" << std::endl;
      else
        std::cerr << "[Error] " << name << " undeclared" << std::endl;
      // TODO: Error
    }
    value.identifier = id;
  }
  for (AstNode* dim : node->child) BuildRelopExpr(dim, tab, mp, err);
}

void BuildFunctionCall(AstNode* node, SymTab& tab, SymMap& mp, ErrList& err) {
  std::cerr << "BuildFunctionCall" << std::endl;
  assert(node->node_type == STMT_NODE);
  AstNode* id_node = *node->child.begin();
  auto& value = std::get<IdentifierSemanticValue>(id_node->semantic_value);
  const std::string& name = std::get<std::string>(value.identifier);
  size_t id = mp.Query(name);
  if (id == SymMap::npos) {
    std::cerr << "[Error] " << name << " undeclared" << std::endl;
    // TODO: Error - `name` undeclared.
  }
  const TableEntry& entry = tab[id];
  if (entry.GetType() != FUNCTION) {
    std::cerr << "[Error] " << name << " should be declared as function"
              << std::endl;
    // TODO: Error - `name` should be declared as function.
  }
  const FunctionType& func = entry.GetValue<FunctionType>();
  AstNode* relop_expr_list = *std::next(node->child.begin());
  if (size_t num_param = relop_expr_list->child.size();
      num_param != func.NumParam()) {
    if (num_param > func.NumParam()) {
      std::cerr << "[Error] too many arguments to function " << name
                << std::endl;
      // TODO: Error - too many arguments to function `name`
    } else {
      std::cerr << "[Error] too few arguments to function " << name
                << std::endl;
      // TODO: Error - too few arguments to function `name`
    }
  }
  BuildRelopExprList(relop_expr_list, tab, mp, err);
}

void BuildRelopExpr(AstNode* expr, SymTab& tab, SymMap& mp, ErrList& err) {
  switch (expr->node_type) {
    case EXPR_NODE:
      for (AstNode* operand : expr->child) {
        BuildRelopExpr(operand, tab, mp, err);
      }
      break;
    case IDENTIFIER_NODE:
      BuildVarRef(expr, tab, mp, err);
      break;
    case STMT_NODE:
      BuildStatement(expr, tab, mp, err);
      break;
  }
}

void BuildAssignExpr(AstNode* expr, SymTab& tab, SymMap& mp, ErrList& err) {
  if (expr->node_type == STMT_NODE &&
      std::get<StmtSemanticValue>(expr->semantic_value).kind == ASSIGN_STMT) {
    AstNode* id_node = *expr->child.begin();
    BuildVarRef(id_node, tab, mp, err);
  } else {
    BuildRelopExpr(expr, tab, mp, err);
  }
}

void BuildRelopExprList(AstNode* relop_expr_list, SymTab& tab, SymMap& mp,
                        ErrList& err) {
  for (AstNode* expr : relop_expr_list->child) {
    BuildRelopExpr(expr, tab, mp, err);
  }
}

void BuildAssignExprList(AstNode* assign_expr_list, SymTab& tab, SymMap& mp,
                         ErrList& err) {
  for (AstNode* expr : assign_expr_list->child) {
    BuildAssignExpr(expr, tab, mp, err);
  }
}

void BuildStatement(AstNode* stmt, SymTab& tab, SymMap& mp, ErrList& err) {
  auto BuildStatementImpl = [&tab, &mp, &err](auto it, auto&... args) {
    (args(*it++, tab, mp, err), ...);
  };

  std::cerr << "BuildStatement" << std::endl;
  if (stmt->node_type == STMT_NODE) {
    auto& value = std::get<StmtSemanticValue>(stmt->semantic_value);
    switch (value.kind) {
      case WHILE_STMT:
      case IF_STMT:
        BuildStatementImpl(stmt->child.begin(), BuildRelopExpr, BuildStatement);
        break;
      case FOR_STMT:
        BuildStatementImpl(stmt->child.begin(), BuildAssignExprList,
                           BuildRelopExprList, BuildAssignExprList,
                           BuildStatement);
        break;
      case IF_ELSE_STMT:
        BuildStatementImpl(stmt->child.begin(), BuildRelopExpr, BuildStatement,
                           BuildStatement);
        break;
      case ASSIGN_STMT:
        BuildAssignExpr(stmt, tab, mp, err);
        break;
      case RETURN_STMT:
        BuildRelopExpr(*stmt->child.begin(), tab, mp, err);
        break;
      case FUNCTION_CALL_STMT:
        BuildFunctionCall(stmt, tab, mp, err);
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
  std::cerr << "BuildDeclList" << std::endl;
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
  std::cerr << "BuildBlock" << std::endl;
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
  std::cerr << "BuildFunctionDecl" << std::endl;
  auto it = func_decl->child.begin();
  AstNode* type_node = *it++;
  DataType type = QueryType(type_node, tab, mp, err);
  AstNode* id_node = *it++;
  assert(id_node && id_node->node_type == IDENTIFIER_NODE);
  auto& func_name =
      std::get<IdentifierSemanticValue>(id_node->semantic_value).identifier;
  std::cerr << "func_name = " << std::get<std::string>(func_name) << std::endl;
  std::vector<VariableType> param_list;
  mp.PushScope();  // push scope for the function parameters
  AstNode* param_list_node = *it++;
  for (AstNode* param : param_list_node->child) {
    std::cerr << "ParseParam" << std::endl;
    param_list.push_back(ParseParam(param, tab, mp, err));
  }
  BuildBlock(*it, tab, mp, err);
  mp.PopScope();  // pop scope
  InsertSymTab(func_name, BuildEntry<FUNCTION>(type, std::move(param_list)),
               tab, mp, err);
}

void BuildGlobalDecl(AstNode* decl, SymTab& tab, SymMap& mp, ErrList& err) {
  std::cerr << "BuildGlobalDecl" << std::endl;
  if (decl->node_type == VARIABLE_DECL_LIST_NODE) {
    for (AstNode* child : decl->child) BuildGlobalDecl(child, tab, mp, err);
    return;
  }
  assert(decl->node_type == DECLARATION_NODE);
  DeclKind kind = std::get<DeclSemanticValue>(decl->semantic_value).kind;
  switch (kind) {
    case VARIABLE_DECL:
      std::cerr << "VARIABLE_DECL" << std::endl;
      BuildVariableDecl(decl, tab, mp, err);
      break;
    case TYPE_DECL:
      std::cerr << "TYPE_DECL" << std::endl;
      BuildTypeDecl(decl, tab, mp, err);
      break;
    case FUNCTION_DECL:
      std::cerr << "FUNCTION_DECL" << std::endl;
      BuildFunctionDecl(decl, tab, mp, err);
      break;
    default:
      std::cerr << "Wtf" << std::endl;
      exit(1);
  }
}

void BuildProgram(AstNode* prog, SymTab& tab, SymMap& mp, ErrList& err) {
  assert(prog->node_type == PROGRAM_NODE);
  for (AstNode* decl : prog->child) BuildGlobalDecl(decl, tab, mp, err);
  /* if (!prog->child.empty()) {
    AstNode* decl_list = *prog->child.begin();
    for (AstNode* decl : prog->child) BuildGlobalDecl(decl, tab, mp, err);
  } */
}

void AnalyzeBlock(AstNode* block, const SymTab& tab, ErrList& err);
void AnalyzeStatement(AstNode* stmt, const SymTab& tab, ErrList& err);
void AnalyzeRelopExpr(AstNode* expr, const SymTab& tab, ErrList& err);

void AnalyzeVarRef(AstNode* var, const SymTab& tab, ErrList& err) {
  auto& value = std::get<IdentifierSemanticValue>(var->semantic_value);
  switch (value.kind) {
    case NORMAL_ID:
    case ARRAY_ID: {
      for (AstNode* expr : var->child) {
        AnalyzeRelopExpr(expr, tab, err);
        if (expr->data_type != INT_TYPE) {
          std::cerr << "[Error] array subscript is not an integer" << std::endl;
          // TODO: Error - array subscript is not an integer.
        }
      }
      break;
    }
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

void AnalyzeForStmt(AstNode* stmt, const SymTab& tab, ErrList& err) {}

void AnalyzeStatement(AstNode* stmt, const SymTab& tab, ErrList& err) {
  if (stmt->node_type == STMT_NODE) {
    auto& value = std::get<StmtSemanticValue>(stmt->semantic_value);
    AstNode* sub_stmt = nullptr;
    switch (value.kind) {
      case WHILE_STMT:
        AnalyzeWhileStmt(stmt, tab, err);
        break;
      case FOR_STMT:
        AnalyzeForStmt(stmt, tab, err);
        break;
      case IF_STMT:
        sub_stmt = *std::prev(stmt->child.end());
        // BuildStatement(sub_stmt, tab, mp, err);
        break;
      case IF_ELSE_STMT:
        sub_stmt = *std::prev(std::prev(stmt->child.end()));
        // BuildStatement(sub_stmt, tab, mp, err);
        sub_stmt = *std::prev(stmt->child.end());
        // BuildStatement(sub_stmt, tab, mp, err);
        break;
      case ASSIGN_STMT:
      case FUNCTION_CALL_STMT:
      case RETURN_STMT:
        break;
    }
  } else {
    if (stmt->node_type == BLOCK_NODE) AnalyzeBlock(stmt, tab, err);
  }
}

void AnalyzeStmtList(AstNode* stmt_list, const SymTab& tab, ErrList& err) {
  for (AstNode* stmt : stmt_list->child) AnalyzeStatement(stmt, tab, err);
}

void AnalyzeDeclList(AstNode* decl_list, const SymTab& tab, ErrList& err) {}

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
  std::cerr << "BuildSymbolTable" << std::endl;
  BuildProgram(prog, tab, mp, err);
  return std::make_pair(std::move(tab), std::move(err));
}

void SemanticAnalysis(AstNode* prog, const SymTab& tab, ErrList& err) {
  AnalyzeProgram(prog, tab, err);
}
