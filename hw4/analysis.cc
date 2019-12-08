#include "analysis.h"

#include <cassert>
#include <functional>
#include <stdexcept>
#include <utility>
#include <variant>

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
// TODO: Store the identifier names to restore error messages in the second
//       pass.
// TODO: Does C-- support something like "typedef int INT[3];"?
// TODO: Support write()
// TODO: Add declare position note

#ifndef NDEBUG
#include <iostream>
template <class T>
static inline void DebugX_(T&& a) {
  std::cerr << a;
}
template <class U, class... T>
static inline void DebugX_(U&& u, T&&... tail) {
  DebugX_(u);
  DebugX_(std::forward<T>(tail)...);
}
template <class... T>
static inline void Debug_(T&&... args) {
  DebugX_(std::forward<T>(args)...);
  std::cerr << std::flush;
}
#else
template <class... T>
static inline void Debug_(T&&...) {}
#endif

struct StopExpression {};

#define TRY_EXPRESSION(action) \
  try {                        \
    action;                    \
  } catch (StopExpression&) {  \
  }

namespace {

const std::string& GetName(const AstNode* nd) {
  return std::get<std::string>(std::get<IdentifierSemanticValue>(
      nd->semantic_value).identifier);
}
const Identifier& GetIdentifier(const AstNode* nd) {
  return std::get<Identifier>(std::get<IdentifierSemanticValue>(
      nd->semantic_value).identifier);
}

}  // namespace

DataType Analyzer::BuildType(AstNode* nd) {
  auto& value = std::get<TypeSpecSemanticValue>(nd->semantic_value);
  try {
    std::string type_name = std::get<std::string>(value.type);
    Debug_("type_name = ", type_name, '\n');
    size_t id = mp_.Query(type_name);
    if (id == SymMap_::npos) {
      success_ = false;
      PrintMsg(file_, nd->loc, ERR_TYPE_UNDECL, type_name);
      // throw StopExpression();
      return UNKNOWN_TYPE;
    } else {
      if (tab_[id].GetType() != TYPE_ALIAS) {
        success_ = false;
        PrintMsg(file_, nd->loc, ERR_NOT_TYPE, type_name);
        // throw StopExpression();
        return UNKNOWN_TYPE;
      }
      value.type = tab_[id].GetValue<DataType>();
      return std::get<DataType>(value.type);
    }
  } catch (const std::bad_variant_access& e) {
    return std::get<DataType>(value.type);
  } catch (...) {
    throw;
  }
}

void Analyzer::InsertSymTab(std::variant<std::string, Identifier>& id,
                            TableEntry&& entry, AstNode* nd,
                            bool is_param) {
  auto id_num = mp_.Insert(std::get<std::string>(id));
  if (id_num.second) {
    tab_.emplace_back(std::move(entry));
    id = id_num.first;
  } else {
    auto& prev_entry = tab_[id_num.first.first];
    if (entry.GetType() == TYPE_ALIAS && prev_entry.GetType() == TYPE_ALIAS &&
        entry.GetValue<DataType>() == prev_entry.GetValue<DataType>()) {
      // typedef can be redeclared
      id = id_num.first;
      return;
    }
    success_ = false;
    MsgType msg =
        is_param ? ERR_REDECL_PARAM
                 : entry.GetType() != prev_entry.GetType()
                       ? ERR_REDECL_TYPE
                       : ((entry.GetType() == VARIABLE &&
                           entry.GetValue<VariableType>() ==
                               prev_entry.GetValue<VariableType>()) ||
                          (entry.GetType() == FUNCTION &&
                           entry.GetValue<FunctionType>().return_type ==
                               prev_entry.GetValue<FunctionType>().return_type))
                             ? ERR_REDECL
                             : ERR_REDECL_CONFLICT;
    PrintMsg(file_, nd->loc, msg, prev_entry.GetNode()->loc,
             id_num.first.second);
    // throw StopExpression();
  }
}

std::vector<size_t> Analyzer::ParseDimDecl(AstNode* parent) {
  auto& dim_decl = parent->child;
  std::vector<size_t> dims;
  for (auto cexpr : dim_decl) {
    if (cexpr->node_type == NULL_NODE) {
      dims.push_back(0);
      continue;
    }
    assert(cexpr->node_type == CONST_VALUE_NODE);
    if (cexpr->data_type != INT_TYPE) {
      success_ = false;
      PrintMsg(file_, cexpr->loc, ERR_DIMEN_NOT_INT, GetName(parent));
      throw StopExpression();
    }
    ConstValue& cv = std::get<ConstValue>(cexpr->semantic_value);
    int size = std::get<int>(cv);
    if (size < 0) {
      success_ = false;
      PrintMsg(file_, cexpr->loc, ERR_DIMEN_NEG, GetName(parent));
      throw StopExpression();
      return dims;
    }
    dims.push_back(size);
  }
  return dims;
}

void Analyzer::BuildInitID(AstNode* init_id, DataType type) noexcept {
  Debug_("BuildInitID", '\n');
  assert(init_id->node_type == IDENTIFIER_NODE);
  try {
    auto& value = std::get<IdentifierSemanticValue>(init_id->semantic_value);
    if (value.kind == NORMAL_ID || value.kind == WITH_INIT_ID) {
      if (value.kind == WITH_INIT_ID) {
        AstNode* init_val = *init_id->child.begin();
        BuildRelopExpr(init_val);
      }
      InsertSymTab(value.identifier, BuildEntry<VARIABLE>(init_id, type),
                   init_id);
    } else {
      std::vector<size_t> dims = ParseDimDecl(init_id);
      InsertSymTab(value.identifier,
                   BuildEntry<VARIABLE>(init_id, type, std::move(dims)),
                   init_id);
    }
  } catch (StopExpression&) {
    // Ignore this identifier.
  }
}

void Analyzer::BuildVariableDecl(AstNode* var_decl) noexcept {
  Debug_("BuildVariableDecl", '\n');
  assert(!var_decl->child.empty());
  AstNode* type_node = *var_decl->child.begin();
  try {
    DataType type = BuildType(type_node);
    for (auto it = std::next(var_decl->child.begin());
         it != var_decl->child.end(); it++) {
      AstNode* init_id = *it;
      BuildInitID(init_id, type);
    }
  } catch (StopExpression&) {
    // Ignore these identifiers.
  }
}

void Analyzer::BuildTypedefID(AstNode* id_item, DataType type) {
  assert(id_item->node_type == IDENTIFIER_NODE);
  auto& value = std::get<IdentifierSemanticValue>(id_item->semantic_value);
  if (value.kind == ARRAY_ID) {
    // TODO
  } else {
    InsertSymTab(value.identifier, BuildEntry<TYPE_ALIAS>(id_item, type),
                 id_item);
  }
}

void Analyzer::BuildTypeDecl(AstNode* type_decl) noexcept {
  assert(!type_decl->child.empty());
  AstNode* type_node = *type_decl->child.begin();
  try {
    DataType type = BuildType(type_node);
    for (auto it = std::next(type_decl->child.begin());
         it != type_decl->child.end(); it++) {
      AstNode* id_item = *it;
      BuildTypedefID(id_item, type);
    }
  } catch (StopExpression&) {
    // Ignore the type alias.
  }
}

std::pair<VariableType, TableEntry> Analyzer::BuildParam(AstNode* param) {
  try {
    DataType type = BuildType(*param->child.begin());
    AstNode* identifier = *std::next(param->child.begin());
    auto& value = std::get<IdentifierSemanticValue>(identifier->semantic_value);
    if (value.kind == NORMAL_ID) {
      return std::make_pair(VariableType(type),
                            BuildEntry<VARIABLE>(identifier, type));
    }
    auto dims = ParseDimDecl(identifier);
    VariableType res(type, std::move(dims));
    return std::make_pair(res, BuildEntry<VARIABLE>(identifier, res));
  } catch (...) {
    throw; // TODO?
  }
}

void Analyzer::BuildVarRef(AstNode* node, bool is_function_arg) {
  Debug_("BuildVarRef", '\n');
  auto& value = std::get<IdentifierSemanticValue>(node->semantic_value);
  const std::string& name = std::get<std::string>(value.identifier);
  size_t id = mp_.Query(name);
  if (id == SymMap_::npos) {
    success_ = false;
    PrintMsg(file_, node->loc, ERR_UNDECL, name);
    // throw StopExpression();
    return;
  }
  const TableEntry& entry = tab_[id];
  if (entry.GetType() != VARIABLE) {
    success_ = false;
    PrintMsg(file_, node->loc, ERR_NOT_VAR, name);
    // throw StopExpression();
    return;
  }
  const auto& var = entry.GetValue<VariableType>();
  if (node->child.size() != var.GetDimension()) {
    if (var.IsArray()) {
      if (!is_function_arg || node->child.size() > var.GetDimension()) {
        success_ = false;
        PrintMsg(file_, node->loc, ERR_ARR_DIMEN);
        // throw StopExpression();
        return;
      }
    } else {
      success_ = false;
      PrintMsg(file_, node->loc, ERR_UNDECL, name);
      // throw StopExpression();
      return;
    }
  }
  value.identifier = GetIdentifier(entry.GetNode());
  try {
    for (AstNode* dim : node->child) BuildRelopExpr(dim);
  } catch (...) {
    throw;
  }
}

void Analyzer::BuildFunctionCall(AstNode* node) {
  Debug_("BuildFunctionCall", '\n');
  assert(node->node_type == STMT_NODE);
  AstNode* id_node = *node->child.begin();
  auto& value = std::get<IdentifierSemanticValue>(id_node->semantic_value);
  const std::string& name = std::get<std::string>(value.identifier);
  size_t id = mp_.Query(name);
  if (id == SymMap_::npos) {
    success_ = false;
    PrintMsg(file_, id_node->loc, ERR_UNDECL, name);
    // throw StopExpression();
    return;
  }
  const TableEntry& entry = tab_[id];
  if (entry.GetType() != FUNCTION) {
    success_ = false;
    PrintMsg(file_, id_node->loc, ERR_NOT_CALLABLE, name);
    // throw StopExpression();
    return;
  }
  const auto& func = entry.GetValue<FunctionType>();
  AstNode* relop_expr_list = *std::next(node->child.begin());
  if (size_t num_param = relop_expr_list->child.size();
      num_param != func.NumParam()) {
    success_ = false;
    PrintMsg(file_, id_node->loc,
             num_param > func.NumParam() ? ERR_ARGS_TOO_MANY : ERR_ARGS_TOO_FEW,
             entry.GetNode()->loc, name);
    // throw StopExpression();
    return;
  }
  value.identifier = GetIdentifier(entry.GetNode());
  node->data_type = func.return_type;
  try {
    BuildRelopExprList(relop_expr_list, true);
  } catch (...) {
    throw;
  }
}

void Analyzer::BuildRelopExpr(AstNode* expr, bool is_function_arg) noexcept {
  switch (expr->node_type) {
    case EXPR_NODE:
      for (AstNode* operand : expr->child) {
        BuildRelopExpr(operand);
      }
      break;
    case IDENTIFIER_NODE:
      TRY_EXPRESSION(BuildVarRef(expr, is_function_arg));
      break;
    case STMT_NODE:
      BuildStatement(expr);
      break;
  }
}

void Analyzer::BuildAssignExpr(AstNode* expr) noexcept {
  if (expr->node_type == STMT_NODE &&
      std::get<StmtSemanticValue>(expr->semantic_value).kind == ASSIGN_STMT) {
    AstNode* id_node = *expr->child.begin();
    AstNode* relop_expr = *std::next(expr->child.begin());
    TRY_EXPRESSION(BuildVarRef(id_node));
    BuildRelopExpr(relop_expr);
  } else {
    BuildRelopExpr(expr);
  }
}

void Analyzer::BuildRelopExprList(AstNode* relop_expr_list,
                                  bool is_function_arg) noexcept {
  for (AstNode* expr : relop_expr_list->child) {
    BuildRelopExpr(expr, is_function_arg);
  }
}

void Analyzer::BuildAssignExprList(AstNode* assign_expr_list) noexcept {
  for (AstNode* expr : assign_expr_list->child) {
    BuildAssignExpr(expr);
  }
}

void Analyzer::BuildStatement(AstNode* stmt) noexcept {
  auto BuildStatementImpl = [this](auto it, auto&&... args) {
    (args(*it++), ...);
  };

  Debug_("BuildStatement", '\n');
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
        if (!stmt->child.empty()) {
          BuildRelopExpr(*stmt->child.begin());
        }
        break;
      case FUNCTION_CALL_STMT:
        TRY_EXPRESSION(BuildFunctionCall(stmt));
        break;
    }
  } else {
    if (stmt->node_type == BLOCK_NODE) BuildBlock(stmt);
  }
}

void Analyzer::BuildStmtList(AstNode* stmt_list) noexcept {
  for (AstNode* stmt : stmt_list->child) BuildStatement(stmt);
}

void Analyzer::BuildDeclList(AstNode* decl_list) noexcept {
  Debug_("BuildDeclList", '\n');
  for (AstNode* child : decl_list->child) {
    DeclKind kind = std::get<DeclSemanticValue>(child->semantic_value).kind;
    if (kind == VARIABLE_DECL) {
      BuildVariableDecl(child);
    } else if (kind == TYPE_DECL) {
      BuildTypeDecl(child);
    }
  }
}

void Analyzer::BuildBlock(AstNode* block) noexcept {
  Debug_("BuildBlock", '\n');
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
  InsertSymTab(value.identifier, std::move(entry), param, true);
}

void Analyzer::BuildFunctionDecl(AstNode* func_decl) {
  Debug_("BuildFunctionDecl", '\n');
  auto it = func_decl->child.begin();
  AstNode* type_node = *it++;
  DataType type = BuildType(type_node);
  AstNode* id_node = *it++;
  assert(id_node && id_node->node_type == IDENTIFIER_NODE);
  auto& func_name =
      std::get<IdentifierSemanticValue>(id_node->semantic_value).identifier;
  Debug_("func_name = ", std::get<std::string>(func_name), '\n');
  std::vector<VariableType> param_list;
  std::vector<TableEntry> entries;
  AstNode* param_list_node = *it++;
  for (AstNode* param : param_list_node->child) {
    Debug_("BuildParam", '\n');
    auto res = BuildParam(param);
    param_list.push_back(std::move(res.first));
    entries.push_back(std::move(res.second));
  }
  InsertSymTab(func_name,
               BuildEntry<FUNCTION>(id_node, type, std::move(param_list)),
               id_node);
  mp_.PushScope();  // push scope for the function parameters
  size_t i = 0;
  for (AstNode* param : param_list_node->child) {
    auto& entry = entries[i++];
    InsertParam(param, std::move(entry));
  }
  BuildBlock(*it);
  mp_.PopScope();  // pop scope
}

void Analyzer::BuildGlobalDecl(AstNode* decl) noexcept {
  Debug_("BuildGlobalDecl", '\n');
  if (decl->node_type == VARIABLE_DECL_LIST_NODE) {
    for (AstNode* child : decl->child) BuildGlobalDecl(child);
    return;
  }
  assert(decl->node_type == DECLARATION_NODE);
  DeclKind kind = std::get<DeclSemanticValue>(decl->semantic_value).kind;
  switch (kind) {
    case VARIABLE_DECL:
      Debug_("VARIABLE_DECL", '\n');
      BuildVariableDecl(decl);
      break;
    case TYPE_DECL:
      Debug_("TYPE_DECL", '\n');
      BuildTypeDecl(decl);
      break;
    case FUNCTION_DECL:
      Debug_("FUNCTION_DECL", '\n');
      // TODO: catch StopFunction
      BuildFunctionDecl(decl);
      break;
  }
}

void Analyzer::BuildProgram(AstNode* prog) {
  assert(prog);
  assert(prog->node_type == PROGRAM_NODE);
  for (AstNode* decl : prog->child) BuildGlobalDecl(decl);
}

bool Analyzer::BuildSymbolTable(AstNode* prog) {
  Debug_("BuildSymbolTable", '\n');
  BuildProgram(prog);
  mp_.ClearMap();
  return success_;
}

void Analyzer::AnalyzeVarRef(AstNode* var) {
  auto& value = std::get<IdentifierSemanticValue>(var->semantic_value);
  const TableEntry& entry = tab_[std::get<Identifier>(value.identifier).first];
  const VariableType& var_type = entry.GetValue<VariableType>();
  Debug_("AnalyzeVarRef", '\n');
  if (value.kind == ARRAY_ID) {
    Debug_("AnalyzeVarRef: value.kind == ARRAY_ID", '\n');
    for (AstNode* expr : var->child) {
      AnalyzeRelopExpr(expr);
      if (expr->data_type != INT_TYPE) {
        success_ = false;
        PrintMsg(file_, expr->loc, ERR_SUBSCRIPT_NOT_INT);
        throw StopExpression();
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
    const TableEntry& entry = tab[std::get<Identifier>(value.identifier).first];
    const VariableType& type = entry.GetValue<VariableType>();
    return type.Slice(expr->child.size());
  } else {
    return VariableType(expr->data_type);
  }
}

inline MsgType CheckConvertibility(const VariableType& proto,
                                   const VariableType& args) {
  // Check whether `b` can be implicitly converted to `a`. Returns an error or
  // a warning if incorrect conversion occurs.
  if (proto.IsArray() && !args.IsArray()) {
    return ERR_SCALAR_TO_ARR;
  }
  if (!proto.IsArray() && args.IsArray()) {
    return ERR_ARR_TO_SCALAR;
  }
  if (proto.GetDimension() != args.GetDimension()) {
    return WARN_INCOMPAT_DIMEN;
  }
  if (proto.IsArray() && proto.data_type != args.data_type) {
    return WARN_INCOMPAT_ARR_TYPE;
  }
  for (size_t i = 1; i < proto.dims.size(); ++i) { // ignore the first dimension
    if (proto.dims[i] != args.dims[i]) return WARN_INCOMPAT_DIMEN;
  }
  return ERR_NOTHING;
}

}  // namespace

void Analyzer::AnalyzeFunctionCall(AstNode* node) {
  Debug_("AnalyzeFunctionCall", '\n');
  AstNode* id_node = *node->child.begin();
  auto& value = std::get<IdentifierSemanticValue>(id_node->semantic_value);
  const TableEntry& entry = tab_[std::get<Identifier>(value.identifier).first];
  const FunctionType& func = entry.GetValue<FunctionType>();
  AstNode* relop_expr_list = *std::next(node->child.begin());
  AnalyzeRelopExprList(relop_expr_list);
  size_t i = 0;
  for (AstNode* param : relop_expr_list->child) {
    MsgType x =
        CheckConvertibility(func.params[i++], GetPrototype(param, tab_));
    if (x != ERR_NOTHING) {
      PrintMsg(file_, param->loc, x, entry.GetNode()->loc, i,
                GetIdentifier(param).second,
                GetIdentifier(entry.GetNode()).second);
      if (GetMsgClass(x) == ERROR) success_ = false;
    }
  }
}

namespace {

inline DataType MixDataType(DataType a, DataType b) noexcept {
  if (a == INT_TYPE && b == INT_TYPE) return INT_TYPE;
  return FLOAT_TYPE;
}

}  // namespace

void Analyzer::AnalyzeRelopExpr(AstNode* expr) {
  Debug_("AnalyzeRelopExpr", '\n');
  switch (expr->node_type) {
    case EXPR_NODE: {
      std::vector<DataType> types;
      for (AstNode* operand : expr->child) {
        AnalyzeRelopExpr(operand);
        types.push_back(operand->data_type);
        assert(operand->data_type != UNKNOWN_TYPE);
        if (operand->data_type == VOID_TYPE) {
          success_ = false;
          PrintMsg(file_, operand->loc, ERR_VOID_ASSIGN);
          // throw StopExpression();
          return;
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
  Debug_("AnalyzeAssignExpr", '\n');
  if (expr->node_type == STMT_NODE &&
      std::get<StmtSemanticValue>(expr->semantic_value).kind == ASSIGN_STMT) {
    AstNode* id_node = *expr->child.begin();
    AstNode* relop_expr = *std::next(expr->child.begin());
    AnalyzeVarRef(id_node);
    AnalyzeRelopExpr(relop_expr);
    if (relop_expr->data_type == VOID_TYPE) {
      success_ = false;
      PrintMsg(file_, relop_expr->loc, ERR_VOID_ASSIGN);
      // throw StopExpression();
      return;
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
    success_ = false;
    PrintMsg(file_, relop_expr->loc, ERR_VOID_ASSIGN);
    // throw StopExpression();
    return;
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
      success_ = false;
      PrintMsg(file_, condition->loc, ERR_VOID_ASSIGN);
      // throw StopExpression();
      return;
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
    success_ = false;
    PrintMsg(file_, relop_expr->loc, ERR_VOID_ASSIGN);
    // throw StopExpression();
    return;
  }
  AnalyzeStatement(*it++);
}

void Analyzer::AnalyzeIfElseStmt(AstNode* stmt) {
  auto it = stmt->child.begin();
  AstNode* relop_expr = *it++;
  AnalyzeRelopExpr(relop_expr);
  if (relop_expr->data_type == VOID_TYPE) {
    success_ = false;
    PrintMsg(file_, relop_expr->loc, ERR_VOID_ASSIGN);
    // throw StopExpression();
    return;
  }
  AnalyzeStatement(*it++);
  AnalyzeStatement(*it++);
}

void Analyzer::AnalyzeStatement(AstNode* stmt) {
  Debug_("AnalyzeStatement", '\n');
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
        try {
          if (!stmt->child.empty()) AnalyzeRelopExpr(*stmt->child.begin());
          DataType type =
              stmt->child.empty() ? VOID_TYPE : stmt->child.front()->data_type;
          if (return_type_ != NONE_TYPE && type != return_type_) {
            if (return_type_ == VOID_TYPE) {
              PrintMsg(file_, stmt->child.front()->loc, WARN_VOID_RETURN);
            } else if (type == VOID_TYPE) {
              PrintMsg(file_, stmt->loc, WARN_RETURN_NOVAL);
            } else {
              PrintMsg(file_, stmt->child.front()->loc, WARN_CONVERSION, type,
                       return_type_);
              // TODO: Need a conversion node?
            }
          }
        } catch (StopExpression&) {
        }
        break;
    }
  } else if (stmt->node_type == BLOCK_NODE) {
    AnalyzeBlock(stmt);
  }
}

void Analyzer::AnalyzeStmtList(AstNode* stmt_list) {
  for (AstNode* stmt : stmt_list->child) AnalyzeStatement(stmt);
}

void Analyzer::AnalyzeInitID(AstNode* init_id) {
  Debug_("AnalyzeInitID", '\n');
  auto& value = std::get<IdentifierSemanticValue>(init_id->semantic_value);
  if (value.kind == WITH_INIT_ID) {
    AstNode* init_val = *init_id->child.begin();
    AnalyzeRelopExpr(init_val);
    if (init_val->data_type == VOID_TYPE) {
      success_ = false;
      PrintMsg(file_, init_val->loc, ERR_VOID_ASSIGN);
      // throw StopExpression();
      return;
    }
  }
}

void Analyzer::AnalyzeVariableDecl(AstNode* var_decl) {
  Debug_("AnalyzeVariableDecl", '\n');
  for (auto it = std::next(var_decl->child.begin());
       it != var_decl->child.end(); it++) {
    AstNode* init_id = *it;
    AnalyzeInitID(init_id);
  }
}

void Analyzer::AnalyzeDeclList(AstNode* decl_list) {
  Debug_("AnalyzeDeclList", '\n');
  for (AstNode* child : decl_list->child) {
    assert(child->node_type == DECLARATION_NODE);
    DeclKind kind = std::get<DeclSemanticValue>(child->semantic_value).kind;
    if (kind == VARIABLE_DECL) {
      AnalyzeVariableDecl(child);
    }
  }
}

void Analyzer::AnalyzeBlock(AstNode* block) {
  Debug_("AnalyzeBlock", '\n');
  for (AstNode* node : block->child) {
    if (node->node_type == STMT_LIST_NODE) {
      AnalyzeStmtList(node);
    } else {
      AnalyzeDeclList(node);
    }
  }
}

void Analyzer::AnalyzeFunctionDecl(AstNode* func) {
  Debug_("AnalyzeFunctionDecl", '\n');
  AstNode* type_node = *func->child.begin();
  DataType type = std::get<DataType>(
      std::get<TypeSpecSemanticValue>(type_node->semantic_value).type);
  return_type_ = type;
  AnalyzeBlock(*std::prev(func->child.end()));
  return_type_ = NONE_TYPE;
}

void Analyzer::AnalyzeGlobalDecl(AstNode* decl) {
  Debug_("AnalyzeGlobalDecl", '\n');
  if (decl->node_type == VARIABLE_DECL_LIST_NODE) {
    for (AstNode* child : decl->child) AnalyzeGlobalDecl(child);
    return;
  }
  DeclKind kind = std::get<DeclSemanticValue>(decl->semantic_value).kind;
  if (kind == FUNCTION_DECL) AnalyzeFunctionDecl(decl);
}

void Analyzer::AnalyzeProgram(AstNode* prog) {
  Debug_("AnalyzeProgram", '\n');
  for (AstNode* decl : prog->child) AnalyzeGlobalDecl(decl);
}

bool Analyzer::SemanticAnalysis(AstNode* prog) {
  AnalyzeProgram(prog);
  return success_;
}
