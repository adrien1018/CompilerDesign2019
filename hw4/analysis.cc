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
// TODO: Catch more errors than requested in the spec.
// TODO: Under what conditions shall we ignore the errors and keep analyzing?

DataType Analyzer::BuildType(AstNode* nd) {
  auto& value = std::get<TypeSpecSemanticValue>(nd->semantic_value);
  try {
    std::string type_name = std::get<std::string>(value.type);
    size_t id = mp_.Query(type_name);
    if (id == SymbolMap<std::string>::npos) {
      std::cerr << "[Error] type " << type_name << " undeclared" << std::endl;
      err_.emplace_back(/* TODO: undeclared */);
      return UNKNOWN_TYPE;
    } else {
      value.type = tab_[id].GetValue<AliasType>().canonical_type;
      return std::get<DataType>(value.type);
    }
  } catch (const std::bad_variant_access& e) {
    return std::get<DataType>(value.type);
  } catch (...) {
    throw;
  }
}

void Analyzer::InsertSymTab(std::variant<std::string, size_t>& id,
                            TableEntry&& entry) {
  auto id_num = mp_.Insert(std::move(std::get<std::string>(id)));
  if (id_num.second) {
    tab_.emplace_back(std::move(entry));
    id = id_num.first;
  } else {
    std::cerr << "[Error] Redeclaration" << std::endl;
    err_.emplace_back(/* TODO: redeclared error */);
  }
}

std::vector<size_t> Analyzer::ParseDimDecl(
    const std::list<AstNode*>& dim_decl) {
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

void Analyzer::BuildInitID(AstNode* init_id, DataType type) {
  std::cerr << "BuildInitID" << std::endl;
  assert(init_id->node_type == IDENTIFIER_NODE);
  auto& value = std::get<IdentifierSemanticValue>(init_id->semantic_value);
  if (value.kind == NORMAL_ID || value.kind == WITH_INIT_ID) {
    if (value.kind == WITH_INIT_ID) {
      AstNode* init_val = *init_id->child.begin();
      BuildRelopExpr(init_val);
    }
    InsertSymTab(value.identifier, BuildEntry<VARIABLE>(type));
  } else {
    std::vector<size_t> dims = ParseDimDecl(init_id->child);
    InsertSymTab(value.identifier, BuildEntry<VARIABLE>(type, std::move(dims)));
  }
}

void Analyzer::BuildVariableDecl(AstNode* var_decl) {
  std::cerr << "BuildVariableDecl" << std::endl;
  assert(!var_decl->child.empty());
  AstNode* type_node = *var_decl->child.begin();
  DataType type = BuildType(type_node);
  for (auto it = std::next(var_decl->child.begin());
       it != var_decl->child.end(); it++) {
    AstNode* init_id = *it;
    BuildInitID(init_id, type);
  }
}

void Analyzer::BuildTypedefID(AstNode* id_item, DataType type) {
  assert(id_item->node_type == IDENTIFIER_NODE);
  auto& value = std::get<IdentifierSemanticValue>(id_item->semantic_value);
  if (value.kind == ARRAY_ID) {
    // TODO
  } else {
    InsertSymTab(value.identifier, BuildEntry<TYPE_ALIAS>(type));
  }
}

void Analyzer::BuildTypeDecl(AstNode* type_decl) {
  assert(!type_decl->child.empty());
  AstNode* type_node = *type_decl->child.begin();
  DataType type = BuildType(type_node);
  for (auto it = std::next(type_decl->child.begin());
       it != type_decl->child.end(); it++) {
    AstNode* id_item = *it;
    BuildTypedefID(id_item, type);
  }
}

VariableType Analyzer::BuildParam(AstNode* param) {
  DataType type = BuildType(*param->child.begin());
  AstNode* identifier = *std::next(param->child.begin());
  auto& value = std::get<IdentifierSemanticValue>(identifier->semantic_value);
  if (value.kind == NORMAL_ID) {
    InsertSymTab(value.identifier, BuildEntry<VARIABLE>(type));
    return VariableType(type);
  } else {
    auto dims = ParseDimDecl(identifier->child);
    VariableType res(type, std::move(dims));
    InsertSymTab(value.identifier, BuildEntry<ARRAY>(res));
    return res;
  }
}

void Analyzer::BuildVarRef(AstNode* node) {
  std::cerr << "BuildVarRef" << std::endl;
  auto& value = std::get<IdentifierSemanticValue>(node->semantic_value);
  const std::string& name = std::get<std::string>(value.identifier);
  size_t id = mp_.Query(name);
  if (id == SymbolMap<std::string>::npos) {
    std::cerr << "[Error] " << name << " undeclared" << std::endl;
    // TODO: Error - `name` undeclared.
  } else {
    const TableEntry& entry = tab_[id];
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
  for (AstNode* dim : node->child) BuildRelopExpr(dim);
}

void Analyzer::BuildFunctionCall(AstNode* node) {
  std::cerr << "BuildFunctionCall" << std::endl;
  assert(node->node_type == STMT_NODE);
  AstNode* id_node = *node->child.begin();
  auto& value = std::get<IdentifierSemanticValue>(id_node->semantic_value);
  const std::string& name = std::get<std::string>(value.identifier);
  size_t id = mp_.Query(name);
  if (id == SymbolMap<std::string>::npos) {
    std::cerr << "[Error] " << name << " undeclared" << std::endl;
    // TODO: Error - `name` undeclared.
  }
  const TableEntry& entry = tab_[id];
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
  BuildRelopExprList(relop_expr_list);
}

void Analyzer::BuildRelopExpr(AstNode* expr) {
  switch (expr->node_type) {
    case EXPR_NODE:
      for (AstNode* operand : expr->child) {
        BuildRelopExpr(operand);
      }
      break;
    case IDENTIFIER_NODE:
      BuildVarRef(expr);
      break;
    case STMT_NODE:
      BuildStatement(expr);
      break;
  }
}

void Analyzer::BuildAssignExpr(AstNode* expr) {
  if (expr->node_type == STMT_NODE &&
      std::get<StmtSemanticValue>(expr->semantic_value).kind == ASSIGN_STMT) {
    AstNode* id_node = *expr->child.begin();
    AstNode* relop_expr = *std::next(expr->child.begin());
    BuildVarRef(id_node);
    BuildRelopExpr(relop_expr);
  } else {
    BuildRelopExpr(expr);
  }
}

void Analyzer::BuildRelopExprList(AstNode* relop_expr_list) {
  for (AstNode* expr : relop_expr_list->child) BuildRelopExpr(expr);
}

void Analyzer::BuildAssignExprList(AstNode* assign_expr_list) {
  for (AstNode* expr : assign_expr_list->child) BuildAssignExpr(expr);
}

void Analyzer::BuildStatement(AstNode* stmt) {
  auto BuildStatementImpl = [this](auto it, auto&&... args) {
    ((this->*args)(*it++), ...);
  };

  std::cerr << "BuildStatement" << std::endl;
  if (stmt->node_type == STMT_NODE) {
    auto& value = std::get<StmtSemanticValue>(stmt->semantic_value);
    switch (value.kind) {
      case WHILE_STMT:
      case IF_STMT:
        BuildStatementImpl(stmt->child.begin(), &Analyzer::BuildRelopExpr,
                           &Analyzer::BuildStatement);
        break;
      case FOR_STMT:
        BuildStatementImpl(stmt->child.begin(), &Analyzer::BuildAssignExprList,
                           &Analyzer::BuildRelopExprList,
                           &Analyzer::BuildAssignExprList,
                           &Analyzer::BuildStatement);
        break;
      case IF_ELSE_STMT:
        BuildStatementImpl(stmt->child.begin(), &Analyzer::BuildRelopExpr,
                           &Analyzer::BuildStatement,
                           &Analyzer::BuildStatement);
        break;
      case ASSIGN_STMT:
        BuildAssignExpr(stmt);
        break;
      case RETURN_STMT:
        BuildRelopExpr(*stmt->child.begin());
        break;
      case FUNCTION_CALL_STMT:
        BuildFunctionCall(stmt);
        break;
    }
  } else {
    if (stmt->node_type == BLOCK_NODE) BuildBlock(stmt);
  }
}

void Analyzer::BuildStmtList(AstNode* stmt_list) {
  for (AstNode* stmt : stmt_list->child) BuildStatement(stmt);
}

void Analyzer::BuildDeclList(AstNode* decl_list) {
  std::cerr << "BuildDeclList" << std::endl;
  for (AstNode* child : decl_list->child) {
    DeclKind kind = std::get<DeclSemanticValue>(child->semantic_value).kind;
    if (kind == VARIABLE_DECL) {
      BuildVariableDecl(child);
    } else if (kind == TYPE_DECL) {
      BuildTypeDecl(child);
    }
  }
}

void Analyzer::BuildBlock(AstNode* block) {
  std::cerr << "BuildBlock" << std::endl;
  mp_.PushScope();
  for (AstNode* node : block->child) {
    switch (node->node_type) {
      case STMT_LIST_NODE:
        BuildStmtList(node);
        break;
      case VARIABLE_DECL_LIST_NODE:
        BuildDeclList(node);
        break;
    }
  }
  mp_.PopScope();
}

void Analyzer::BuildFunctionDecl(AstNode* func_decl) {
  std::cerr << "BuildFunctionDecl" << std::endl;
  auto it = func_decl->child.begin();
  AstNode* type_node = *it++;
  DataType type = BuildType(type_node);
  AstNode* id_node = *it++;
  assert(id_node && id_node->node_type == IDENTIFIER_NODE);
  auto& func_name =
      std::get<IdentifierSemanticValue>(id_node->semantic_value).identifier;
  std::cerr << "func_name = " << std::get<std::string>(func_name) << std::endl;
  std::vector<VariableType> param_list;
  mp_.PushScope();  // push scope for the function parameters
  AstNode* param_list_node = *it++;
  for (AstNode* param : param_list_node->child) {
    std::cerr << "BuildParam" << std::endl;
    param_list.push_back(BuildParam(param));
  }
  BuildBlock(*it);
  mp_.PopScope();  // pop scope
  InsertSymTab(func_name, BuildEntry<FUNCTION>(type, std::move(param_list)));
}

void Analyzer::BuildGlobalDecl(AstNode* decl) {
  std::cerr << "BuildGlobalDecl" << std::endl;
  if (decl->node_type == VARIABLE_DECL_LIST_NODE) {
    for (AstNode* child : decl->child) BuildGlobalDecl(child);
    return;
  }
  assert(decl->node_type == DECLARATION_NODE);
  DeclKind kind = std::get<DeclSemanticValue>(decl->semantic_value).kind;
  switch (kind) {
    case VARIABLE_DECL:
      std::cerr << "VARIABLE_DECL" << std::endl;
      BuildVariableDecl(decl);
      break;
    case TYPE_DECL:
      std::cerr << "TYPE_DECL" << std::endl;
      BuildTypeDecl(decl);
      break;
    case FUNCTION_DECL:
      std::cerr << "FUNCTION_DECL" << std::endl;
      BuildFunctionDecl(decl);
      break;
    default:
      std::cerr << "Wtf" << std::endl;
      exit(1);
  }
}

void Analyzer::BuildProgram(AstNode* prog) {
  assert(prog->node_type == PROGRAM_NODE);
  for (AstNode* decl : prog->child) BuildGlobalDecl(decl);
}

void Analyzer::BuildSymbolTable(AstNode* prog) {
  std::cerr << "BuildSymbolTable" << std::endl;
  BuildProgram(prog);
}

void Analyzer::AnalyzeVarRef(AstNode* var) {
  auto& value = std::get<IdentifierSemanticValue>(var->semantic_value);
  if (value.kind == ARRAY_ID) {
    for (AstNode* expr : var->child) {
      AnalyzeRelopExpr(expr);
      if (expr->data_type != INT_TYPE) {
        std::cerr << "[Error] array subscript is not an integer" << std::endl;
        // TODO: Error - array subscript is not an integer.
      }
    }
  }
}

void Analyzer::AnalyzeFunctionCall(AstNode* node) {}

void Analyzer::AnalyzeRelopExpr(AstNode* expr) {
  switch (expr->node_type) {
    case EXPR_NODE:
      for (AstNode* operand : expr->child) {
        AnalyzeRelopExpr(operand);
        if (operand->data_type == VOID_TYPE) {
          std::cerr << "[Error] void value not ignored as it ought to be."
                    << std::endl;
          // TODO: void value not ignored as it ought to be.
        }
      }
      break;
    case IDENTIFIER_NODE:
      AnalyzeVarRef(expr);
      break;
    case STMT_NODE:
      AnalyzeStatement(expr);
      break;
  }
}

void Analyzer::AnalyzeAssignExpr(AstNode* expr) {
  if (expr->node_type == STMT_NODE &&
      std::get<StmtSemanticValue>(expr->semantic_value).kind == ASSIGN_STMT) {
    AstNode* id_node = *expr->child.begin();
    AstNode* relop_expr = *std::next(expr->child.begin());
    AnalyzeVarRef(id_node);
    AnalyzeRelopExpr(relop_expr);
    if (relop_expr->data_type == VOID_TYPE) {
      std::cerr << "[Error] void value not ignored as it ought to be."
                << std::endl;
      // TODO: void value not ignored as it ought to be.
    }
  } else {
    AnalyzeRelopExpr(expr);
  }
}

void Analyzer::AnalyzeAssignExprList(AstNode* assign_expr_list) {
  for (AstNode* expr : assign_expr_list->child) AnalyzeAssignExpr(expr);
}

void Analyzer::AnalyzeRelopExprList(AstNode* relop_expr_list) {
  for (AstNode* expr : relop_expr_list->child) AnalyzeRelopExpr(expr);
}

void Analyzer::AnalyzeWhileStmt(AstNode* stmt) {
  AstNode* relop_expr = *stmt->child.begin();
  AnalyzeRelopExpr(relop_expr);
  if (relop_expr->data_type == VOID_TYPE) {
    std::cerr << "[Error] void value not ignored as it ought to be."
              << std::endl;
    // TODO: void value not ignored as it ought to be.
  }
  AnalyzeStatement(*std::next(stmt->child.begin()));
}

void Analyzer::AnalyzeForStmt(AstNode* stmt) {
  auto it = stmt->child.begin();
  AnalyzeAssignExprList(*it++);
  AstNode* relop_expr_list = *it++;
  AnalyzeRelopExprList(relop_expr_list);
  if (!relop_expr_list->child.empty()) {
    AstNode* condition = *std::prev(relop_expr_list->child.end());
    if (condition->data_type == VOID_TYPE) {
      std::cerr << "[Error] void value not ignored as it ought to be."
                << std::endl;
      // TODO: void value not ignored as it ought to be.
    }
  }
  AnalyzeAssignExprList(*it++);
  AnalyzeStatement(*it++);
}

void Analyzer::AnalyzeIfStmt(AstNode* stmt) {
  auto it = stmt->child.begin();
  AstNode* relop_expr = *it++;
  AnalyzeRelopExpr(relop_expr);
  if (relop_expr->data_type == VOID_TYPE) {
    std::cerr << "[Error] void value not ignored as it ought to be."
              << std::endl;
    // TODO: void value not ignored as it ought to be.
  }
  AnalyzeStatement(*it++);
}

void Analyzer::AnalyzeIfElseStmt(AstNode* stmt) {
  auto it = stmt->child.begin();
  AstNode* relop_expr = *it++;
  AnalyzeRelopExpr(relop_expr);
  if (relop_expr->data_type == VOID_TYPE) {
    std::cerr << "[Error] void value not ignored as it ought to be."
              << std::endl;
    // TODO: void value not ignored as it ought to be.
  }
  AnalyzeStatement(*it++);
  AnalyzeStatement(*it++);
}

void Analyzer::AnalyzeStatement(AstNode* stmt) {
  if (stmt->node_type == STMT_NODE) {
    auto& value = std::get<StmtSemanticValue>(stmt->semantic_value);
    switch (value.kind) {
      case WHILE_STMT:
        AnalyzeWhileStmt(stmt);
        break;
      case FOR_STMT:
        AnalyzeForStmt(stmt);
        break;
      case IF_STMT:
        AnalyzeIfStmt(stmt);
        break;
      case IF_ELSE_STMT:
        AnalyzeIfElseStmt(stmt);
        break;
      case ASSIGN_STMT:
        AnalyzeAssignExpr(stmt);
        break;
      case FUNCTION_CALL_STMT:
        AnalyzeFunctionCall(stmt);
        break;
      case RETURN_STMT:
        // TODO
        break;
    }
  } else {
    if (stmt->node_type == BLOCK_NODE) AnalyzeBlock(stmt);
  }
}

void Analyzer::AnalyzeStmtList(AstNode* stmt_list) {
  for (AstNode* stmt : stmt_list->child) AnalyzeStatement(stmt);
}

void Analyzer::AnalyzeDeclList(AstNode* decl_list) {}

void Analyzer::AnalyzeBlock(AstNode* block) {
  for (AstNode* node : block->child) {
    switch (node->node_type) {
      case STMT_LIST_NODE:
        AnalyzeStmtList(node);
        break;
      case VARIABLE_DECL_LIST_NODE:
        AnalyzeDeclList(node);
        break;
    }
  }
}

void Analyzer::AnalyzeFunctionDecl(AstNode* func) {
  AnalyzeBlock(*std::prev(func->child.end()));
}

void Analyzer::AnalyzeGlobalDecl(AstNode* decl) {
  if (decl->node_type == VARIABLE_DECL_LIST_NODE) {
    for (AstNode* child : decl->child) AnalyzeGlobalDecl(child);
    return;
  }
  DeclKind kind = std::get<DeclSemanticValue>(decl->semantic_value).kind;
  if (kind == FUNCTION_DECL) AnalyzeFunctionDecl(decl);
}

void Analyzer::AnalyzeProgram(AstNode* prog) {
  for (AstNode* decl : prog->child) AnalyzeGlobalDecl(decl);
}

void Analyzer::SemanticAnalysis(AstNode* prog) { AnalyzeProgram(prog); }
