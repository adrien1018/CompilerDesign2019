#include "analysis.h"

#include <cassert>
#include <functional>
#include <iostream>
#include <stdexcept>
#include <utility>
#include <variant>

#include "error.h"

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

// TODO: Catch more errors than requested in the spec.
// TODO: Under what conditions shall we ignore the errors and keep analyzing?
// TODO: Store the identifier names to restore error messages in the second
// pass.
// TODO: Does C-- support something like "typedef int INT[3];"?

DataType Analyzer::BuildType(AstNode* nd) {
  auto& value = std::get<TypeSpecSemanticValue>(nd->semantic_value);
  try {
    std::string type_name = std::get<std::string>(value.type);
    std::cerr << "type_name = " << type_name << std::endl;
    size_t id = mp_.Query(type_name);
    if (id == SymbolMap<std::string>::npos) {
      std::cerr << "[Error] type " << type_name << " undeclared" << std::endl;
      //err_.emplace_back(/* TODO */);
      return UNKNOWN_TYPE;
    } else {
      if (tab_[id].GetType() != TYPE_ALIAS) {
        std::cerr << "[Error] " << type_name << " was not declared as type"
                  << std::endl;
        return UNKNOWN_TYPE;
      }
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
    //err_.emplace_back(/* TODO: redeclared error */);
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

std::pair<VariableType, TableEntry> Analyzer::BuildParam(AstNode* param) {
  DataType type = BuildType(*param->child.begin());
  AstNode* identifier = *std::next(param->child.begin());
  auto& value = std::get<IdentifierSemanticValue>(identifier->semantic_value);
  if (value.kind == NORMAL_ID) {
    return std::make_pair(VariableType(type), BuildEntry<VARIABLE>(type));
  }
  auto dims = ParseDimDecl(identifier->child);
  VariableType res(type, std::move(dims));
  return std::make_pair(res, BuildEntry<ARRAY>(res));
}

void Analyzer::BuildVarRef(AstNode* node, bool is_function_arg) {
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
    const auto& var = entry.GetValue<VariableType>();
    if (node->child.size() != var.GetDimension()) {
      if (var.IsArray()) {
        if (!is_function_arg || node->child.size() > var.GetDimension()) {
          std::cerr << "[Error] Incompatible array dimensions" << std::endl;
          // TODO: Error
        }
      } else {
        std::cerr << "[Error] " << name << " undeclared" << std::endl;
        // TODO: Error
      }
    }
    value.identifier = id;
  }
  for (AstNode* dim : node->child) BuildRelopExpr(dim);
}

void Analyzer::BuildWriteCall(AstNode* node) {
  if (node->child.size() == 1) {
    std::cerr << "[Error] too few arguments to function write" << std::endl;
    // TODO: Error - too few arguments to function `name`
    return;
  }
  if (node->child.size() != 2) {
    std::cerr << "[Error] too many arguments to function write" << std::endl;
    // TODO: Error - too many arguments to function `name`
    return;
  }
  if ((*std::next(node->child.begin()))->data_type != CONST_STRING_TYPE) {
    // TODO: Error - argument is not a string
  }
}

void Analyzer::BuildFunctionCall(AstNode* node) {
  std::cerr << "BuildFunctionCall" << std::endl;
  assert(node->node_type == STMT_NODE);
  AstNode* id_node = *node->child.begin();
  auto& value = std::get<IdentifierSemanticValue>(id_node->semantic_value);
  const std::string& name = std::get<std::string>(value.identifier);
  if (name == "write") return BuildWriteCall(node);
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
  value.identifier = id;
  const auto& func = entry.GetValue<FunctionType>();
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
  node->data_type = func.return_type;
  BuildRelopExprList(relop_expr_list, true);
}

void Analyzer::BuildRelopExpr(AstNode* expr, bool is_function_arg) {
  switch (expr->node_type) {
    case EXPR_NODE:
      for (AstNode* operand : expr->child) {
        BuildRelopExpr(operand);
      }
      break;
    case IDENTIFIER_NODE:
      BuildVarRef(expr, is_function_arg);
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

void Analyzer::BuildRelopExprList(AstNode* relop_expr_list,
                                  bool is_function_arg) {
  for (AstNode* expr : relop_expr_list->child)
    BuildRelopExpr(expr, is_function_arg);
}

void Analyzer::BuildAssignExprList(AstNode* assign_expr_list) {
  for (AstNode* expr : assign_expr_list->child) BuildAssignExpr(expr);
}

void Analyzer::BuildStatement(AstNode* stmt) {
  auto BuildStatementImpl = [this](auto it, auto&&... args) {
    (args(*it++), ...);
  };

  std::cerr << "BuildStatement" << std::endl;
  if (stmt->node_type == STMT_NODE) {
    using namespace std::placeholders;
    auto Func = [this](auto x, auto&&... args) {
      return std::bind(x, this, _1, args...);
    };
    auto& value = std::get<StmtSemanticValue>(stmt->semantic_value);
    switch (value.kind) {
      case WHILE_STMT:
      case IF_STMT:
        BuildStatementImpl(stmt->child.begin(),
                           Func(&Analyzer::BuildRelopExpr, false),
                           Func(&Analyzer::BuildStatement));
        break;
      case FOR_STMT:
        BuildStatementImpl(stmt->child.begin(),
                           Func(&Analyzer::BuildAssignExprList),
                           Func(&Analyzer::BuildRelopExprList, false),
                           Func(&Analyzer::BuildAssignExprList),
                           Func(&Analyzer::BuildStatement));
        break;
      case IF_ELSE_STMT:
        BuildStatementImpl(
            stmt->child.begin(), Func(&Analyzer::BuildRelopExpr, false),
            Func(&Analyzer::BuildStatement), Func(&Analyzer::BuildStatement));
        break;
      case ASSIGN_STMT:
        BuildAssignExpr(stmt);
        break;
      case RETURN_STMT:
        if (!stmt->child.empty()) BuildRelopExpr(*stmt->child.begin());
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

void Analyzer::InsertParam(AstNode* param, TableEntry&& entry) {
  AstNode* identifier = *std::next(param->child.begin());
  auto& value = std::get<IdentifierSemanticValue>(identifier->semantic_value);
  InsertSymTab(value.identifier, std::move(entry));
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
  std::vector<TableEntry> entries;
  AstNode* param_list_node = *it++;
  for (AstNode* param : param_list_node->child) {
    std::cerr << "BuildParam" << std::endl;
    auto res = BuildParam(param);
    param_list.push_back(std::move(res.first));
    entries.push_back(std::move(res.second));
  }
  InsertSymTab(func_name, BuildEntry<FUNCTION>(type, std::move(param_list)));
  mp_.PushScope();  // push scope for the function parameters
  size_t i = 0;
  for (AstNode* param : param_list_node->child) {
    auto& entry = entries[i++];
    InsertParam(param, std::move(entry));
  }
  BuildBlock(*it);
  mp_.PopScope();  // pop scope
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
  assert(prog);
  assert(prog->node_type == PROGRAM_NODE);
  for (AstNode* decl : prog->child) BuildGlobalDecl(decl);
}

void Analyzer::BuildSymbolTable(AstNode* prog) {
  std::cerr << "BuildSymbolTable" << std::endl;
  BuildProgram(prog);
}

void Analyzer::AnalyzeVarRef(AstNode* var) {
  auto& value = std::get<IdentifierSemanticValue>(var->semantic_value);
  const TableEntry& entry = tab_[std::get<size_t>(value.identifier)];
  const VariableType& var_type = entry.GetValue<VariableType>();
  std::cerr << "AnalyzeVarRef" << std::endl;
  if (value.kind == ARRAY_ID) {
    std::cerr << "AnalyzeVarRef: value.kind == ARRAY_ID" << std::endl;
    for (AstNode* expr : var->child) {
      AnalyzeRelopExpr(expr);
      if (expr->data_type != INT_TYPE) {
        std::cerr << "[Error] array subscript is not an integer" << std::endl;
        // TODO: Error - array subscript is not an integer.
      }
    }
    if (var->child.size() == var_type.GetDimension()) {
      var->data_type = var_type.data_type;
    } else {
      assert(var->child.size() < var_type.GetDimension());
      var->data_type = NONE_TYPE;
    }
  } else {
    var->data_type = var_type.data_type;
  }
}

namespace {

inline VariableType GetPrototype(AstNode* expr,
                                 const std::vector<TableEntry>& tab) {
  if (expr->node_type == IDENTIFIER_NODE) {
    auto& value = std::get<IdentifierSemanticValue>(expr->semantic_value);
    const TableEntry& entry = tab[std::get<size_t>(value.identifier)];
    const VariableType& type = entry.GetValue<VariableType>();
    return type.Slice(expr->child.size());
  } else {
    return VariableType(expr->data_type);
  }
}

inline std::optional<SemanticError> CheckConvertibility(
    const VariableType& proto, const VariableType& args) {
  // Check whether `b` can be implicitly converted to `a`. Returns an error or
  // a warning if incorrect conversion occurs.
  if (proto.IsArray() && !args.IsArray()) {
    // TODO: Error - Scalar <name> passed to array parameter <name>
    std::cerr << "[Error] Scalar <name> passed to array parameter <name>"
              << std::endl;
    return {};
  }
  if (!proto.IsArray() && args.IsArray()) {
    // TODO: Error - Array <name> passed to scalar parameter <name>
    std::cerr << "[Error] Array <name> passed to scalar parameter <name>"
              << std::endl;
    return {};
  }
  if (proto.GetDimension() != args.GetDimension()) {
    // TODO: Warning - passing argument <name> of <name> from incompatible
    // pointer type
    std::cerr << "[Warning] passing argument <name> of <name> from "
                 "incompatible pointer type"
              << std::endl;
    return {};
  }
  for (size_t i = 0; i < proto.dims.size(); ++i) {
    if (proto.dims[i] > 0 && args.dims[i] > 0 &&
        proto.dims[i] != args.dims[i]) {
      // TODO: Warning - passing argument <name> of <name> from incompatible
      // pointer type
      std::cerr << "[Warning] passing argument <name> of <name> from "
                   "incompatible pointer type"
                << std::endl;
      return {};
    }
  }
  return {};
}

}  // namespace

void Analyzer::AnalyzeFunctionCall(AstNode* node) {
  std::cerr << "AnalyzeFunctionCall" << std::endl;
  AstNode* id_node = *node->child.begin();
  auto& value = std::get<IdentifierSemanticValue>(id_node->semantic_value);
  try {
    const TableEntry& entry = tab_[std::get<size_t>(value.identifier)];
    const FunctionType& func = entry.GetValue<FunctionType>();
    AstNode* relop_expr_list = *std::next(node->child.begin());
    AnalyzeRelopExprList(relop_expr_list);
    size_t i = 0;
    for (AstNode* param : relop_expr_list->child) {
      auto err =
          CheckConvertibility(func.params[i++], GetPrototype(param, tab_));
      if (err.has_value()) {
        std::cerr << "[Error] " << std::endl;
        // TODO: Error
      }
    }
  } catch (...) {
    std::cerr << "Calling write() function" << std::endl;
  }
}

namespace {

inline DataType MixDataType(DataType a, DataType b) noexcept {
  if (a == INT_TYPE && b == INT_TYPE) return INT_TYPE;
  return FLOAT_TYPE;
}

}  // namespace

void Analyzer::AnalyzeRelopExpr(AstNode* expr) {
  std::cerr << "AnalyzeRelopExpr" << std::endl;
  switch (expr->node_type) {
    case EXPR_NODE: {
      std::vector<DataType> types;
      for (AstNode* operand : expr->child) {
        AnalyzeRelopExpr(operand);
        types.push_back(operand->data_type);
        assert(operand->data_type != UNKNOWN_TYPE);
        if (operand->data_type == VOID_TYPE) {
          std::cerr << "[Error] void value not ignored as it ought to be."
                    << std::endl;
          // TODO: void value not ignored as it ought to be.
        }
      }
      auto& value = std::get<ExprSemanticValue>(expr->semantic_value);
      if (value.kind == BINARY_OPERATION) {
        assert(types.size() == 2);
        BinaryOperator op = std::get<BinaryOperator>(value.op);
        switch (op) {
          case BINARY_OP_OR:
          case BINARY_OP_AND:
          case BINARY_OP_LT:
          case BINARY_OP_LE:
          case BINARY_OP_GT:
          case BINARY_OP_GE:
          case BINARY_OP_EQ:
          case BINARY_OP_NE:
            expr->data_type = INT_TYPE;
            break;
          case BINARY_OP_ADD:
          case BINARY_OP_SUB:
          case BINARY_OP_MUL:
          case BINARY_OP_DIV:
            expr->data_type = MixDataType(types[0], types[1]);
            break;
        }
      } else {
        assert(types.size() == 1);
        UnaryOperator op = std::get<UnaryOperator>(value.op);
        if (op == UNARY_OP_NEGATIVE)
          expr->data_type = types[0];
        else
          expr->data_type = INT_TYPE;
      }
      break;
    }
    case IDENTIFIER_NODE:
      AnalyzeVarRef(expr);
      break;
    case STMT_NODE:
      AnalyzeStatement(expr);
      break;
  }
}

void Analyzer::AnalyzeAssignExpr(AstNode* expr) {
  std::cerr << "AnalyzeAssignExpr" << std::endl;
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
  std::cerr << "AnalyzeStatement" << std::endl;
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
        if (!stmt->child.empty()) AnalyzeRelopExpr(*stmt->child.begin());
        DataType type = stmt->child.empty()
                            ? VOID_TYPE
                            : (*(stmt->child.begin()))->data_type;
        if (return_type_ != NONE_TYPE && type != return_type_) {
          std::cerr << "[Error] Incompatible return type" << std::endl;
          // TODO: Error - Incompatible return type.
        }
        break;
    }
  } else {
    if (stmt->node_type == BLOCK_NODE) AnalyzeBlock(stmt);
  }
}

void Analyzer::AnalyzeStmtList(AstNode* stmt_list) {
  for (AstNode* stmt : stmt_list->child) AnalyzeStatement(stmt);
}

void Analyzer::AnalyzeInitID(AstNode* init_id) {
  std::cerr << "AnalyzeInitID" << std::endl;
  auto& value = std::get<IdentifierSemanticValue>(init_id->semantic_value);
  if (value.kind == WITH_INIT_ID) {
    AstNode* init_val = *init_id->child.begin();
    AnalyzeRelopExpr(init_val);
    if (init_val->data_type == VOID_TYPE) {
      std::cerr << "[Error]: void value not ignored as it ought to be"
                << std::endl;
      // TODO
    }
  }
}

void Analyzer::AnalyzeVariableDecl(AstNode* var_decl) {
  std::cerr << "AnalyzeVariableDecl" << std::endl;
  for (auto it = std::next(var_decl->child.begin());
       it != var_decl->child.end(); it++) {
    AstNode* init_id = *it;
    AnalyzeInitID(init_id);
  }
}

void Analyzer::AnalyzeDeclList(AstNode* decl_list) {
  std::cerr << "AnalyzeDeclList" << std::endl;
  for (AstNode* child : decl_list->child) {
    assert(child->node_type == DECLARATION_NODE);
    DeclKind kind = std::get<DeclSemanticValue>(child->semantic_value).kind;
    if (kind == VARIABLE_DECL) {
      AnalyzeVariableDecl(child);
    }
  }
}

void Analyzer::AnalyzeBlock(AstNode* block) {
  std::cerr << "AnalyzeBlock" << std::endl;
  for (AstNode* node : block->child) {
    if (node->node_type == STMT_LIST_NODE) {
      AnalyzeStmtList(node);
    } else {
      AnalyzeDeclList(node);
    }
  }
}

void Analyzer::AnalyzeFunctionDecl(AstNode* func) {
  std::cerr << "AnalyzeFunctionDecl" << std::endl;
  AstNode* type_node = *func->child.begin();
  DataType type = std::get<DataType>(
      std::get<TypeSpecSemanticValue>(type_node->semantic_value).type);
  return_type_ = type;
  AnalyzeBlock(*std::prev(func->child.end()));
  return_type_ = NONE_TYPE;
}

void Analyzer::AnalyzeGlobalDecl(AstNode* decl) {
  std::cerr << "AnalyzeGlobalDecl" << std::endl;
  if (decl->node_type == VARIABLE_DECL_LIST_NODE) {
    for (AstNode* child : decl->child) AnalyzeGlobalDecl(child);
    return;
  }
  DeclKind kind = std::get<DeclSemanticValue>(decl->semantic_value).kind;
  if (kind == FUNCTION_DECL) AnalyzeFunctionDecl(decl);
}

void Analyzer::AnalyzeProgram(AstNode* prog) {
  std::cerr << "AnalyzeProgram" << std::endl;
  for (AstNode* decl : prog->child) AnalyzeGlobalDecl(decl);
}

void Analyzer::SemanticAnalysis(AstNode* prog) { AnalyzeProgram(prog); }
