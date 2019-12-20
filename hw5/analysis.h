#ifndef ANALYSIS_H_
#define ANALYSIS_H_

#include <string>
#include <utility>
#include <vector>

#include "entry.h"
#include "error.h"
#include "symtab.h"

constexpr size_t kNumBuiltinFunction = 3;
constexpr std::array<DataType, kNumBuiltinFunction> kBuiltinReturnType = {
    VOID_TYPE, INT_TYPE, FLOAT_TYPE};
const std::array<std::pair<std::string, size_t>, kNumBuiltinFunction>
    kBuiltinFunction = {std::make_pair("write", 1), std::make_pair("read", 0),
                        std::make_pair("fread", 0)};

class Analyzer {
 public:
  Analyzer(const FileInfor file) : file_(file), success_(true) {}
  bool SemanticAnalysis(AstNode* prog);
  bool BuildSymbolTable(AstNode* prog);
  std::vector<TableEntry>& GetSymbolTable() { return tab_; }
  std::vector<TableEntry>&& MoveSymbolTable() { return std::move(tab_); }

 private:
  using SymMap_ = SymbolMap<std::string::value_type>;
  SymMap_ mp_;
  std::vector<TableEntry> tab_;
  DataType return_type_ = NONE_TYPE;

  FileInfor file_;
  bool success_;

  void BuildProgram(AstNode* prog);
  void BuildGlobalDecl(AstNode* decl) noexcept;
  void BuildFunctionDecl(AstNode* func_decl);
  void BuildBlock(AstNode* block) noexcept;
  void BuildDeclList(AstNode* decl_list) noexcept;
  void BuildStmtList(AstNode* stmt_list) noexcept;
  void BuildStatement(AstNode* stmt) noexcept;
  void BuildAssignExprList(AstNode* assign_expr_list) noexcept;
  void BuildRelopExprList(AstNode* relop_expr_list,
                          bool is_function_arg = false) noexcept;
  void BuildAssignExpr(AstNode* expr) noexcept;
  void BuildRelopExpr(AstNode* expr, bool is_function_arg = false) noexcept;
  void BuildFunctionCall(AstNode* node);
  void BuildVarRef(AstNode* node, bool is_function_arg = false);
  void BuildTypeDecl(AstNode* type_decl) noexcept;
  void BuildTypedefID(AstNode* id_item, const TypeAttr& attr);
  void BuildVariableDecl(AstNode* var_decl) noexcept;
  void BuildInitID(AstNode* init_id, const TypeAttr& attr) noexcept;

  std::pair<VariableAttr, TableEntry> BuildParam(AstNode* param);
  std::vector<size_t> ParseDimDecl(AstNode* parent);
  void InsertSymTab(std::variant<std::string, Identifier>& id,
                    TableEntry&& entry, AstNode*, bool is_param = false);
  void InsertParam(AstNode* param, TableEntry&& entry);
  TypeAttr BuildType(AstNode* nd);

  void AnalyzeProgram(AstNode* prog);
  void AnalyzeGlobalDecl(AstNode* decl) noexcept;
  void AnalyzeFunctionDecl(AstNode* func);
  void AnalyzeBlock(AstNode* block) noexcept;
  void AnalyzeStmtList(AstNode* stmt_list) noexcept;
  void AnalyzeDeclList(AstNode* decl_list) noexcept;
  void AnalyzeVariableDecl(AstNode* var_decl) noexcept;
  void AnalyzeInitID(AstNode* init_id);
  void AnalyzeStatement(AstNode* stmt) noexcept;
  void AnalyzeIfStmt(AstNode* stmt) noexcept;
  void AnalyzeIfElseStmt(AstNode* stmt) noexcept;
  void AnalyzeForStmt(AstNode* stmt) noexcept;
  void AnalyzeWhileStmt(AstNode* stmt) noexcept;
  void AnalyzeRelopExprList(AstNode* relop_expr_list) noexcept;
  void AnalyzeAssignExprList(AstNode* assign_expr_list) noexcept;
  void AnalyzeAssignExpr(AstNode* expr);
  void AnalyzeRelopExpr(AstNode* expr);
  void AnalyzeFunctionCall(AstNode* node);
  void AnalyzeVarRef(AstNode* var);
};

#endif  // ANALYSIS_H_
