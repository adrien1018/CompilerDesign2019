#include "analysis.h"

#include <cassert>
#include <functional>
#include <stdexcept>
#include <utility>
#include <variant>

#include "utils.h"

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

const BuiltinAttr kBuiltinFunction[] = {{1, VOID_TYPE},
                                        {1, VOID_TYPE},
                                        {1, VOID_TYPE},
                                        {0, INT_TYPE},
                                        {0, FLOAT_TYPE}};

const size_t kBuiltinFunctionNum =
    sizeof(kBuiltinFunction) / sizeof(BuiltinAttr);

const std::unordered_map<std::string, size_t> kBuiltinFunctionMap = {
    {"write", 0}, {"read", 3}, {"fread", 4}};

struct StopExpression {};

#define TRY_EXPRESSION(action) \
  try {                        \
    action;                    \
  } catch (StopExpression&) {  \
  }

namespace {

const std::string& GetName(const AstNode* nd) {
  return std::get<std::string>(
      std::get<IdentifierSemanticValue>(nd->semantic_value).identifier);
}
const Identifier& GetIdentifier(const AstNode* nd) {
  return std::get<Identifier>(
      std::get<IdentifierSemanticValue>(nd->semantic_value).identifier);
}

}  // namespace

/**
 * Returns the type attribute of the type node `nd`.
 * Throws a `StopExpression` if the corresponding typename is not declared or
 * the declaration is not a type declaration.
 */
TypeAttr Analyzer::BuildType(AstNode* nd) {
  auto& value = std::get<TypeSpecSemanticValue>(nd->semantic_value);
  try {
    std::string type_name = std::get<std::string>(value.type);
    Debug_("type_name = ", type_name, '\n');
    size_t id = mp_.Query(type_name);
    if (id == SymMap_::npos) {
      success_ = false;
      PrintMsg(file_, nd->loc, ERR_TYPE_UNDECL, type_name);
      throw StopExpression();
    } else {
      if (tab_[id].GetType() != TYPE_ALIAS) {
        success_ = false;
        PrintMsg(file_, nd->loc, ERR_NOT_TYPE, type_name);
        throw StopExpression();
      }
      value.type = id;
      return tab_[id].GetValue<TypeAttr>();
    }
  } catch (const std::bad_variant_access& e) {
    try {
      size_t id = std::get<size_t>(value.type);
      return tab_[id].GetValue<TypeAttr>();
    } catch (const std::bad_variant_access& e) {
      // built-in types.
      return TypeAttr(std::get<DataType>(value.type));
    }
  } catch (...) {
    // unexpected exceptions.
    throw;
  }
}

size_t Analyzer::InsertSymTab(std::variant<std::string, Identifier>& id,
                              TableEntry&& entry, AstNode* nd, bool is_param) {
  auto id_num = mp_.Insert(std::get<std::string>(id));
  if (id_num.second) {
    tab_.emplace_back(std::move(entry));
    id = id_num.first;
    return id_num.first.first;
  }
  auto& prev_entry = tab_[id_num.first.first];
  if (entry.GetType() == TYPE_ALIAS && prev_entry.GetType() == TYPE_ALIAS &&
      entry.GetValue<TypeAttr>() == prev_entry.GetValue<TypeAttr>()) {
    // typedef can be redeclared
    id = id_num.first;
    return id_num.first.first;
  }
  success_ = false;
  MsgType msg =
      is_param ? ERR_REDECL_PARAM
               : entry.GetType() != prev_entry.GetType()
                     ? ERR_REDECL_TYPE
                     : ((entry.GetType() == VARIABLE &&
                         entry.GetValue<VariableAttr>() ==
                             prev_entry.GetValue<VariableAttr>()) ||
                        (entry.GetType() == FUNCTION &&
                         entry.GetValue<FunctionAttr>().return_type ==
                             prev_entry.GetValue<FunctionAttr>().return_type))
                           ? ERR_REDECL
                           : ERR_REDECL_CONFLICT;
  PrintMsg(file_, nd->loc, msg, prev_entry.GetNode()->loc, id_num.first.second);
  throw StopExpression();
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
    int32_t size = std::get<int32_t>(cv);
    if (size < 0) {
      success_ = false;
      PrintMsg(file_, cexpr->loc, ERR_DIMEN_NEG, GetName(parent));
      throw StopExpression();
    }
    dims.push_back(size);
  }
  return dims;
}

void Analyzer::BuildInitID(AstNode* init_id, const TypeAttr& attr,
                           bool glob) noexcept {
  Debug_("BuildInitID", '\n');
  assert(init_id->node_type == IDENTIFIER_NODE);
  try {
    auto& value = std::get<IdentifierSemanticValue>(init_id->semantic_value);
    if (value.kind == NORMAL_ID || value.kind == WITH_INIT_ID) {
      if (value.kind == WITH_INIT_ID) {
        AstNode* init_val = *init_id->child.begin();
        BuildRelopExpr(init_val);
        if (attr.IsArray()) {
          success_ = false;
          PrintMsg(file_, init_val->loc, ERR_INVALID_INIT);
          throw StopExpression();
        }
        if (glob) {
          if (init_val->node_type != CONST_VALUE_NODE) {
            bool failed = false;
            if (init_val->node_type != EXPR_NODE) {
              failed = true;
            } else {
              auto& expr =
                  std::get<ExprSemanticValue>(init_val->semantic_value);
              if (!expr.is_const_eval) failed = true;
            }
            if (failed) {
              success_ = false;
              PrintMsg(file_, init_val->loc, ERR_INIT_NONCONST);
              throw StopExpression();
            }
          }
        }
      }
      InsertSymTab(value.identifier,
                   BuildEntry<VARIABLE>(init_id, attr.data_type, attr.dims),
                   init_id);
    } else {
      std::vector<size_t> dims = ParseDimDecl(init_id);
      dims.insert(dims.end(), attr.dims.begin(), attr.dims.end());
      InsertSymTab(
          value.identifier,
          BuildEntry<VARIABLE>(init_id, attr.data_type, std::move(dims)),
          init_id);
    }
  } catch (StopExpression&) {
    // Ignore this identifier.
  }
}

void Analyzer::BuildVariableDecl(AstNode* var_decl, bool glob) noexcept {
  Debug_("BuildVariableDecl", '\n');
  assert(!var_decl->child.empty());
  AstNode* type_node = *var_decl->child.begin();
  try {
    const TypeAttr& type = BuildType(type_node);
    for (auto it = std::next(var_decl->child.begin());
         it != var_decl->child.end(); it++) {
      AstNode* init_id = *it;
      BuildInitID(init_id, type, glob);
    }
  } catch (StopExpression&) {
    // Ignore these identifiers.
  }
}

void Analyzer::BuildTypedefID(AstNode* id_item, const TypeAttr& attr) {
  Debug_("BuildTypedefID\n");
  assert(id_item->node_type == IDENTIFIER_NODE);
  auto& value = std::get<IdentifierSemanticValue>(id_item->semantic_value);
  if (value.kind == ARRAY_ID) {
    auto dim = ParseDimDecl(id_item);
    if (attr.data_type == VOID_TYPE) {
      success_ = false;
      PrintMsg(file_, id_item->loc, ERR_VOID_ARRAY, GetName(id_item));
      throw StopExpression();
    }
    dim.insert(dim.end(), attr.dims.begin(), attr.dims.end());
    InsertSymTab(
        value.identifier,
        BuildEntry<TYPE_ALIAS>(id_item, attr.data_type, std::move(dim)),
        id_item);
  } else {
    InsertSymTab(value.identifier, BuildEntry<TYPE_ALIAS>(id_item, attr),
                 id_item);
  }
}

void Analyzer::BuildTypeDecl(AstNode* type_decl) noexcept {
  assert(!type_decl->child.empty());
  AstNode* type_node = *type_decl->child.begin();
  try {
    const TypeAttr& attr = BuildType(type_node);
    for (auto it = std::next(type_decl->child.begin());
         it != type_decl->child.end(); it++) {
      AstNode* id_item = *it;
      BuildTypedefID(id_item, attr);
    }
  } catch (StopExpression&) {
    // Ignore the type alias.
  }
}

TableEntry Analyzer::BuildParam(AstNode* param) {
  try {
    const TypeAttr& attr = BuildType(*param->child.begin());
    AstNode* identifier = *std::next(param->child.begin());
    auto& value = std::get<IdentifierSemanticValue>(identifier->semantic_value);
    if (value.kind == NORMAL_ID) {
      return BuildEntry<VARIABLE>(identifier, attr.data_type, attr.dims);
    }
    auto dims = ParseDimDecl(identifier);
    dims.insert(dims.end(), attr.dims.begin(), attr.dims.end());
    return BuildEntry<VARIABLE>(identifier, attr.data_type, std::move(dims));
  } catch (...) {
    // rethrow
    throw;
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
    throw StopExpression();
  }
  const TableEntry& entry = tab_[id];
  if (entry.GetType() != VARIABLE) {
    success_ = false;
    PrintMsg(file_, node->loc, ERR_NOT_VAR, name);
    throw StopExpression();
  }
  const auto& var = entry.GetValue<VariableAttr>();
  if (node->child.size() != var.GetDimension()) {
    if (var.IsArray()) {
      if (!is_function_arg || node->child.size() > var.GetDimension()) {
        success_ = false;
        PrintMsg(file_, node->loc, ERR_ARR_DIMEN);
        throw StopExpression();
      }
    } else {
      success_ = false;
      PrintMsg(file_, node->loc, ERR_SCALAR_SUBSCRIPT, name);
      throw StopExpression();
    }
  }
  value.identifier = GetIdentifier(entry.GetNode());
  for (AstNode* dim : node->child) BuildRelopExpr(dim);
}

void Analyzer::BuildFunctionCall(AstNode* node) {
  Debug_("BuildFunctionCall", '\n');
  assert(node->node_type == STMT_NODE);
  AstNode* id_node = *node->child.begin();
  auto& value = std::get<IdentifierSemanticValue>(id_node->semantic_value);
  const std::string& name = std::get<std::string>(value.identifier);
  {
    auto it = kBuiltinFunctionMap.find(name);
    if (it != kBuiltinFunctionMap.end()) {  // built-in functions
      AstNode* relop_expr_list = *std::next(node->child.begin());
      size_t id = it->second;
      auto& attr = kBuiltinFunction[id];
      if (size_t num_param = relop_expr_list->child.size();
          num_param != attr.num_param) {
        success_ = false;
        PrintMsg(
            file_, id_node->loc,
            num_param > attr.num_param ? ERR_ARGS_TOO_MANY : ERR_ARGS_TOO_FEW,
            name);
        throw StopExpression();
      }
      value.identifier = (Identifier){~id, {}};
      BuildRelopExprList(relop_expr_list, true);
      node->data_type = attr.return_type;
      return;
    }
  }
  size_t id = mp_.Query(name);
  if (id == SymMap_::npos) {
    success_ = false;
    PrintMsg(file_, id_node->loc, ERR_UNDECL, name);
    throw StopExpression();
  }
  const TableEntry& entry = tab_[id];
  if (entry.GetType() != FUNCTION) {
    success_ = false;
    PrintMsg(file_, id_node->loc, ERR_NOT_CALLABLE, name);
    throw StopExpression();
  }
  const auto& func = entry.GetValue<FunctionAttr>();
  AstNode* relop_expr_list = *std::next(node->child.begin());
  if (size_t num_param = relop_expr_list->child.size();
      num_param != func.NumParam()) {
    Debug_("num_param ", num_param, "\n");
    Debug_("func.NumParam ", func.NumParam(), "\n");
    success_ = false;
    PrintMsg(file_, id_node->loc,
             num_param > func.NumParam() ? ERR_ARGS_TOO_MANY : ERR_ARGS_TOO_FEW,
             entry.GetNode()->loc, name);
    throw StopExpression();
  }
  value.identifier = GetIdentifier(entry.GetNode());
  node->data_type = func.return_type;
  BuildRelopExprList(relop_expr_list, true);
}

void Analyzer::BuildRelopExpr(AstNode* expr, bool is_function_arg) noexcept {
  Debug_("BuildRelopExpr\n");
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
  Debug_("PushScope:block\n");
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
  Debug_("PopScope:block\n");
  mp_.PopScope();
}

size_t Analyzer::InsertParam(AstNode* param, TableEntry&& entry) {
  AstNode* identifier = *std::next(param->child.begin());
  auto& value = std::get<IdentifierSemanticValue>(identifier->semantic_value);
  return InsertSymTab(value.identifier, std::move(entry), param, true);
}

void Analyzer::BuildFunctionDecl(AstNode* func_decl) {
  bool flag = false;
  try {
    Debug_("BuildFunctionDecl", '\n');
    auto it = func_decl->child.begin();
    AstNode* type_node = *it++;
    const TypeAttr& attr = BuildType(type_node);
    AstNode* id_node = *it++;
    assert(id_node && id_node->node_type == IDENTIFIER_NODE);
    auto& func_name =
        std::get<IdentifierSemanticValue>(id_node->semantic_value).identifier;
    if (attr.IsArray()) {
      success_ = false;
      PrintMsg(file_, id_node->loc, ERR_RETURN_ARRAY,
               std::get<std::string>(func_name));
      throw StopExpression();
    }
    Debug_("func_name = ", std::get<std::string>(func_name), '\n');
    std::vector<TableEntry> entries;
    AstNode* param_list_node = *it++;
    for (AstNode* param : param_list_node->child) {
      Debug_("BuildParam", '\n');
      entries.push_back(BuildParam(param));
    }
    auto entry = BuildEntry<FUNCTION>(id_node, attr.data_type);
    size_t f = InsertSymTab(func_name, std::move(entry), id_node);
    // Debug_("f = ", f, "\n");
    // std::vector<size_t>& refs = tab_[f].GetValue<FunctionAttr>().params;
    mp_.PushScope();  // push scope for the function parameters
    flag = true;
    size_t i = 0;
    for (AstNode* param : param_list_node->child) {
      auto& et = entries[i++];
      size_t pos = InsertParam(param, std::move(et));
      tab_[f].GetValue<FunctionAttr>().params.push_back(pos);
    }
    // Debug_("refs.size() = ", refs.size(), "\n");
    // Debug_("tab = ", tab_[f].GetValue<FunctionAttr>().params.size(), "\n");
    BuildBlock(*it);
  } catch (StopExpression&) {
  }
  if (flag) mp_.PopScope();  // pop scope
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
      BuildVariableDecl(decl, true);
      break;
    case TYPE_DECL:
      Debug_("TYPE_DECL", '\n');
      BuildTypeDecl(decl);
      break;
    case FUNCTION_DECL:
      Debug_("FUNCTION_DECL", '\n');
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

namespace {

AstNode* MakeConvNode(DataType from, DataType to, AstNode* fa, AstNode* ch) {
  assert(from == INT_TYPE || from == FLOAT_TYPE || from == BOOLEAN_TYPE);
  assert(to == INT_TYPE || to == FLOAT_TYPE || to == BOOLEAN_TYPE);
  AstNode* conv = new AstNode(CONVERSION_NODE);
  conv->semantic_value = ConversionSemanticValue{from, to};
  conv->parent = fa;
  conv->child.push_back(ch);
  conv->data_type = to;
  ch->parent = conv;
  return conv;
}

}  // namespace

void Analyzer::AnalyzeVarRef(AstNode* var) {
  auto& value = std::get<IdentifierSemanticValue>(var->semantic_value);
  const TableEntry& entry = tab_[std::get<Identifier>(value.identifier).first];
  const VariableAttr& var_type = entry.GetValue<VariableAttr>();
  Debug_("AnalyzeVarRef", '\n');
  if (value.kind == ARRAY_ID) {
    Debug_("AnalyzeVarRef: value.kind == ARRAY_ID", '\n');
    for (AstNode* expr : var->child) {
      TRY_EXPRESSION(AnalyzeRelopExpr(expr));
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

inline VariableAttr GetPrototype(AstNode* expr,
                                 const std::vector<TableEntry>& tab) {
  if (expr->node_type != IDENTIFIER_NODE) {
    return VariableAttr(expr->data_type);
  }
  auto& value = std::get<IdentifierSemanticValue>(expr->semantic_value);
  const TableEntry& entry = tab[std::get<Identifier>(value.identifier).first];
  const VariableAttr& type = entry.GetValue<VariableAttr>();
  return type.Slice(expr->child.size());
}

inline MsgType CheckConvertibility(const VariableAttr& proto,
                                   const VariableAttr& args) {
  // Check whether `b` can be implicitly converted to `a`. Returns an error or
  // a warning if incorrect conversion occurs.
  if (args.data_type == CONST_STRING_TYPE) {
    return ERR_STRING_TO_SCALAR;
  }
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
  for (size_t i = 1; i < proto.dims.size();
       ++i) {  // ignore the first dimension
    if (proto.dims[i] != args.dims[i]) return WARN_INCOMPAT_DIMEN;
  }
  return ERR_NOTHING;
}

}  // namespace

void Analyzer::AnalyzeFunctionCall(AstNode* node) {
  Debug_("AnalyzeFunctionCall", '\n');
  assert(func_ptr_);
  func_ptr_->is_leaf = false;
  AstNode* id_node = *node->child.begin();
  auto& value = std::get<IdentifierSemanticValue>(id_node->semantic_value);
  size_t& id = std::get<Identifier>(value.identifier).first;
  if (id > ~kBuiltinFunctionNum) {  // built-in function
    AstNode* relop_expr_list = *std::next(node->child.begin());
    assert(relop_expr_list->child.size() == kBuiltinFunction[~id].num_param);
    AnalyzeRelopExprList(relop_expr_list);
    if (!relop_expr_list->child.empty()) {
      AstNode* param = relop_expr_list->child.front();
      auto proto = GetPrototype(param, tab_);
      if (proto.IsArray()) {
        success_ = false;
        PrintMsg(file_, param->loc, ERR_ARR_TO_SCALAR,
                 GetIdentifier(param).second);
      } else if (~id == 0) {  // write
        switch (proto.data_type) {
          case CONST_STRING_TYPE:
            id = ~(size_t)0;
            break;
          case INT_TYPE:
          case BOOLEAN_TYPE:
            id = ~(size_t)1;
            break;
          case FLOAT_TYPE:
            id = ~(size_t)2;
            break;
          default:
            assert(false);
        }
      }
    }
    return;
  }
  const TableEntry& entry = tab_[id];
  const FunctionAttr& func = entry.GetValue<FunctionAttr>();
  AstNode* relop_expr_list = *std::next(node->child.begin());
  AnalyzeRelopExprList(relop_expr_list);
  size_t i = 0;
  for (auto it = relop_expr_list->child.begin();
       it != relop_expr_list->child.end();) {
    AstNode* nd = *it;
    auto& args = tab_[func.params[i++]].GetValue<VariableAttr>();
    auto param = GetPrototype(nd, tab_);
    MsgType x = CheckConvertibility(args, param);
    if (x != ERR_NOTHING) {
      if (nd->node_type == IDENTIFIER_NODE) {
        PrintMsg(file_, nd->loc, x, entry.GetNode()->loc, i,
                 GetIdentifier(nd).second,
                 GetIdentifier(entry.GetNode()).second);
      } else {
        if (x == ERR_STRING_TO_SCALAR) {
          PrintMsg(file_, nd->loc, x, entry.GetNode()->loc, i, args.data_type,
                   GetIdentifier(entry.GetNode()).second);
        } else {
          PrintMsg(file_, nd->loc, x, entry.GetNode()->loc, i,
                   GetIdentifier(entry.GetNode()).second);
        }
      }
      if (GetMsgClass(x) == ERROR) success_ = false;
    }
    if (!args.IsArray() && !param.IsArray()) {
      if (args.data_type != param.data_type) {
        AstNode* conv =
            MakeConvNode(param.data_type, args.data_type, relop_expr_list, nd);
        it = relop_expr_list->child.erase(it);
        relop_expr_list->child.insert(it, conv);
      } else {
        it = std::next(it);
      }
    } else {
      it = std::next(it);
    }
  }
}

namespace {

inline DataType MixDataType(DataType a, DataType b) noexcept {
  if (a == INT_TYPE && b == INT_TYPE) return INT_TYPE;
  return FLOAT_TYPE;
}

inline bool NeedConversion(DataType from, DataType to) {
  // boolean can be directly converted to int
  return from != to && (from != BOOLEAN_TYPE || to != INT_TYPE);
}

void MergeExpr(AstNode* expr, DataType type) {
  AstNode* lhs = *expr->child.begin();
  AstNode* rhs = *std::next(expr->child.begin());
  if (NeedConversion(lhs->data_type, type)) {
    AstNode* conv = MakeConvNode(lhs->data_type, type, expr, lhs);
    expr->child.pop_front();
    expr->child.push_front(conv);
  }
  if (NeedConversion(rhs->data_type, type)) {
    AstNode* conv = MakeConvNode(rhs->data_type, type, expr, rhs);
    expr->child.pop_back();
    expr->child.push_back(conv);
  }
  expr->data_type = type;
}

}  // namespace

void Analyzer::AnalyzeRelopExpr(AstNode* expr) {
  Debug_("AnalyzeRelopExpr", '\n');
  switch (expr->node_type) {
    case EXPR_NODE: {
      std::vector<DataType> types;
      for (AstNode* operand : expr->child) {
        TRY_EXPRESSION(AnalyzeRelopExpr(operand));
        types.push_back(operand->data_type);
        assert(operand->data_type != UNKNOWN_TYPE);
        if (operand->data_type == VOID_TYPE) {
          success_ = false;
          PrintMsg(file_, operand->loc, ERR_VOID_ASSIGN);
          throw StopExpression();
        }
      }
      auto& value = std::get<ExprSemanticValue>(expr->semantic_value);
      if (value.kind == BINARY_OPERATION) {
        assert(types.size() == 2);
        BinaryOperator op = std::get<BinaryOperator>(value.op);
        switch (op) {
          case BINARY_OP_OR:
          case BINARY_OP_AND:
            MergeExpr(expr, BOOLEAN_TYPE);
            assert(expr->child.size() == 2);
            break;
          case BINARY_OP_LT:
          case BINARY_OP_LE:
          case BINARY_OP_GT:
          case BINARY_OP_GE:
          case BINARY_OP_EQ:
          case BINARY_OP_NE:
            MergeExpr(expr, MixDataType(types[0], types[1]));
            expr->data_type = BOOLEAN_TYPE;
            assert(expr->child.size() == 2);
            break;
          case BINARY_OP_ADD:
          case BINARY_OP_SUB:
          case BINARY_OP_MUL:
          case BINARY_OP_DIV:
            MergeExpr(expr, MixDataType(types[0], types[1]));
            break;
        }
      } else {
        assert(types.size() == 1);
        UnaryOperator op = std::get<UnaryOperator>(value.op);
        switch (op) {
          case UNARY_OP_POSITIVE:
          case UNARY_OP_NEGATIVE:
            if (types[0] == CONST_STRING_TYPE) {
              success_ = false;
              PrintMsg(file_, (*expr->child.begin())->loc,
                       ERR_UNARY_MINUS_STRING);
              throw StopExpression();
            }
            expr->data_type = types[0];
            break;
          case UNARY_OP_LOGICAL_NEGATION: {
            if (types[0] != BOOLEAN_TYPE) {
              AstNode* conv = MakeConvNode(types[0], BOOLEAN_TYPE, expr,
                                           *expr->child.begin());
              expr->child.pop_back();
              expr->child.push_back(conv);
            }
            expr->data_type = BOOLEAN_TYPE;
            break;
          }
        }
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
    TRY_EXPRESSION(AnalyzeVarRef(id_node));
    TRY_EXPRESSION(AnalyzeRelopExpr(relop_expr));
    if (relop_expr->data_type == VOID_TYPE) {
      success_ = false;
      PrintMsg(file_, relop_expr->loc, ERR_VOID_ASSIGN);
      throw StopExpression();
    }
    assert(id_node->data_type == INT_TYPE || id_node->data_type == FLOAT_TYPE);
    assert(relop_expr->data_type == INT_TYPE ||
           relop_expr->data_type == FLOAT_TYPE ||
           relop_expr->data_type == BOOLEAN_TYPE);
    if (id_node->data_type != relop_expr->data_type) {
      AstNode* conv = MakeConvNode(relop_expr->data_type, id_node->data_type,
                                   expr, relop_expr);
      expr->child.pop_back();
      expr->child.push_back(conv);
    }
  } else {
    try {
      AnalyzeRelopExpr(expr);
    } catch (...) {
      throw;
    }
  }
}

void Analyzer::AnalyzeAssignExprList(AstNode* assign_expr_list) noexcept {
  for (AstNode* expr : assign_expr_list->child) {
    TRY_EXPRESSION(AnalyzeAssignExpr(expr));
  }
}

void Analyzer::AnalyzeRelopExprList(AstNode* relop_expr_list) noexcept {
  for (AstNode* expr : relop_expr_list->child) {
    TRY_EXPRESSION(AnalyzeRelopExpr(expr));
  }
}

void Analyzer::AnalyzeWhileStmt(AstNode* stmt) noexcept {
  AstNode* relop_expr = *stmt->child.begin();
  TRY_EXPRESSION(AnalyzeRelopExpr(relop_expr));
  if (relop_expr->data_type == VOID_TYPE) {
    success_ = false;
    PrintMsg(file_, relop_expr->loc, ERR_VOID_ASSIGN);
  }
  if (relop_expr->data_type != BOOLEAN_TYPE) {
    AstNode* conv =
        MakeConvNode(relop_expr->data_type, BOOLEAN_TYPE, stmt, relop_expr);
    stmt->child.pop_front();
    stmt->child.push_front(conv);
  }
  AnalyzeStatement(*std::next(stmt->child.begin()));
}

void Analyzer::AnalyzeForStmt(AstNode* stmt) noexcept {
  auto it = stmt->child.begin();
  AnalyzeAssignExprList(*it++);
  AstNode* relop_expr_list = *it++;
  AnalyzeRelopExprList(relop_expr_list);
  if (!relop_expr_list->child.empty()) {
    AstNode* condition = *std::prev(relop_expr_list->child.end());
    if (condition->data_type == VOID_TYPE) {
      success_ = false;
      PrintMsg(file_, condition->loc, ERR_VOID_ASSIGN);
    }
    if (condition->data_type != BOOLEAN_TYPE) {
      AstNode* conv = MakeConvNode(condition->data_type, BOOLEAN_TYPE,
                                   relop_expr_list, condition);
      relop_expr_list->child.pop_back();
      relop_expr_list->child.push_back(conv);
    }
  }
  AnalyzeAssignExprList(*it++);
  AnalyzeStatement(*it++);
}

void Analyzer::AnalyzeIfStmt(AstNode* stmt) noexcept {
  auto it = stmt->child.begin();
  AstNode* relop_expr = *it++;
  TRY_EXPRESSION(AnalyzeRelopExpr(relop_expr));
  if (relop_expr->data_type == VOID_TYPE) {
    success_ = false;
    PrintMsg(file_, relop_expr->loc, ERR_VOID_ASSIGN);
  }
  if (relop_expr->data_type != BOOLEAN_TYPE) {
    AstNode* conv =
        MakeConvNode(relop_expr->data_type, BOOLEAN_TYPE, stmt, relop_expr);
    stmt->child.pop_front();
    stmt->child.push_front(conv);
  }
  AnalyzeStatement(*it++);
}

void Analyzer::AnalyzeIfElseStmt(AstNode* stmt) noexcept {
  auto it = stmt->child.begin();
  AstNode* relop_expr = *it++;
  TRY_EXPRESSION(AnalyzeRelopExpr(relop_expr));
  if (relop_expr->data_type == VOID_TYPE) {
    success_ = false;
    PrintMsg(file_, relop_expr->loc, ERR_VOID_ASSIGN);
  }
  if (relop_expr->data_type != BOOLEAN_TYPE) {
    AstNode* conv =
        MakeConvNode(relop_expr->data_type, BOOLEAN_TYPE, stmt, relop_expr);
    stmt->child.pop_front();
    stmt->child.push_front(conv);
  }
  AnalyzeStatement(*it++);
  AnalyzeStatement(*it++);
}

void Analyzer::AnalyzeStatement(AstNode* stmt) noexcept {
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
        TRY_EXPRESSION(AnalyzeAssignExpr(stmt));
        break;
      case FUNCTION_CALL_STMT:
        AnalyzeFunctionCall(stmt);
        break;
      case RETURN_STMT:
        try {
          if (!stmt->child.empty()) AnalyzeRelopExpr(*stmt->child.begin());
          DataType type =
              stmt->child.empty() ? VOID_TYPE : stmt->child.front()->data_type;
          if (func_ptr_ && type != func_ptr_->return_type) {
            if (func_ptr_->return_type == VOID_TYPE) {
              PrintMsg(file_, stmt->child.front()->loc, WARN_VOID_RETURN);
            } else if (type == VOID_TYPE) {
              PrintMsg(file_, stmt->loc, WARN_RETURN_NOVAL);
            } else {
              PrintMsg(file_, stmt->child.front()->loc, WARN_CONVERSION, type,
                       func_ptr_->return_type);
              AstNode* ret = *stmt->child.begin();
              AstNode* conv =
                  MakeConvNode(type, func_ptr_->return_type, stmt, ret);
              stmt->child.pop_front();
              stmt->child.push_front(conv);
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

void Analyzer::AnalyzeStmtList(AstNode* stmt_list) noexcept {
  for (AstNode* stmt : stmt_list->child) AnalyzeStatement(stmt);
}

void Analyzer::AnalyzeInitID(AstNode* init_id) {
  Debug_("AnalyzeInitID", '\n');
  auto& value = std::get<IdentifierSemanticValue>(init_id->semantic_value);
  if (value.kind == WITH_INIT_ID) {
    AstNode* init_val = *init_id->child.begin();
    try {
      AnalyzeRelopExpr(init_val);
    } catch (...) {
      throw;
    }
    if (init_val->data_type == VOID_TYPE) {
      success_ = false;
      PrintMsg(file_, init_val->loc, ERR_VOID_ASSIGN);
      throw StopExpression();
    }
    VariableAttr& attr = tab_[std::get<Identifier>(value.identifier).first]
                             .GetValue<VariableAttr>();
    if (init_val->data_type != attr.data_type) {
      AstNode* conv =
          MakeConvNode(init_val->data_type, attr.data_type, init_id, init_val);
      init_id->child.pop_back();
      init_id->child.push_back(conv);
    }
  }
}

void Analyzer::AnalyzeVariableDecl(AstNode* var_decl) noexcept {
  Debug_("AnalyzeVariableDecl", '\n');
  for (auto it = std::next(var_decl->child.begin());
       it != var_decl->child.end(); it++) {
    AstNode* init_id = *it;
    TRY_EXPRESSION(AnalyzeInitID(init_id));
  }
}

void Analyzer::AnalyzeDeclList(AstNode* decl_list) noexcept {
  Debug_("AnalyzeDeclList", '\n');
  for (AstNode* child : decl_list->child) {
    assert(child->node_type == DECLARATION_NODE);
    DeclKind kind = std::get<DeclSemanticValue>(child->semantic_value).kind;
    if (kind == VARIABLE_DECL) {
      AnalyzeVariableDecl(child);
    }
  }
}

void Analyzer::AnalyzeBlock(AstNode* block) noexcept {
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
  AstNode* id_node = *std::next(func->child.begin());
  auto& value = std::get<IdentifierSemanticValue>(id_node->semantic_value);
  size_t pos = std::get<Identifier>(value.identifier).first;
  func_ptr_ = &tab_[pos].GetValue<FunctionAttr>();
  AnalyzeBlock(*std::prev(func->child.end()));
  func_ptr_ = nullptr;
  // AstNode* type_node = *func->child.begin();
  // DataType return_type;
  // try {
  //   auto& attr = tab_[std::get<size_t>(std::get<TypeSpecSemanticValue>(
  //                                          type_node->semantic_value)
  //                                          .type)]
  //                    .GetValue<TypeAttr>();
  //   return_type = attr.IsArray() ? NONE_TYPE : attr.data_type;
  // } catch (const std::bad_variant_access& e) {
  //   return_type = std::get<DataType>(
  //       std::get<TypeSpecSemanticValue>(type_node->semantic_value).type);
  // }
  // return_type_ = return_type;
  // AnalyzeBlock(*std::prev(func->child.end()));
  // return_type_ = NONE_TYPE;
}

void Analyzer::AnalyzeGlobalDecl(AstNode* decl) noexcept {
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
