#ifndef ANALYSIS_H_
#define ANALYSIS_H_

#include <string>
#include <utility>
#include <vector>

#include "entry.h"
#include "error.h"

struct BuiltinAttr {
  size_t num_param;
  DataType return_type;
};

extern const size_t kBuiltinFunctionNum;
extern const BuiltinAttr kBuiltinFunction[];

using AnalyzerInfo =
    std::tuple<std::vector<TableEntry>&, SymbolMap<std::string::value_type>&>;

class Analyzer {
 public:
  Analyzer(const FileInfor file) : file_(file), success_(true) {}
  bool SemanticAnalysis(AstNode* prog);
  bool BuildSymbolTable(AstNode* prog);
  std::vector<TableEntry>& GetSymbolTable() { return tab_; }
  AnalyzerInfo MoveSymbolTable() { return std::tie(tab_, mp_); }

 private:
  using SymMap_ = SymbolMap<std::string::value_type>;
  SymMap_ mp_;
  std::vector<TableEntry> tab_;
  // DataType return_type_ = NONE_TYPE;
  FunctionAttr* func_ptr_ = nullptr;

  FileInfor file_;
  bool success_;

  void BuildProgram(AstNode* prog);
  void BuildGlobalDecl(AstNode* decl) noexcept;
  void BuildFunctionDecl(AstNode* func_decl);
  void BuildBlock(AstNode* block, bool push_scope = true) noexcept;
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
  void BuildVariableDecl(AstNode* var_decl, bool glob = false) noexcept;
  void BuildInitID(AstNode* init_id, const TypeAttr& attr, bool glob) noexcept;

  TableEntry BuildParam(AstNode* param);
  std::vector<size_t> ParseDimDecl(AstNode* parent);
  size_t InsertSymTab(std::variant<std::string, Identifier>& id,
                      TableEntry&& entry, AstNode*, bool is_param = false);
  size_t InsertParam(AstNode* param, TableEntry&& entry);
  TypeAttr BuildType(AstNode* nd);

  void AnalyzeProgram(AstNode* prog);
  void AnalyzeGlobalDecl(AstNode* decl) noexcept;
  void AnalyzeFunctionDecl(AstNode* func);
  void AnalyzeBlock(AstNode* block) noexcept;
  void AnalyzeStmtList(AstNode* stmt_list);
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
