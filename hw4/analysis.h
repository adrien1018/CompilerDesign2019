#ifndef ANALYSIS_H_
#define ANALYSIS_H_

#include <string>
#include <utility>
#include <vector>

#include "entry.h"
#include "error.h"
#include "symtab.h"


class Analyzer {
 public:
  Analyzer(const FileInfor file) : file_(file), success_(true) {}
  bool SemanticAnalysis(AstNode* prog);
  bool BuildSymbolTable(AstNode* prog);
  std::vector<TableEntry>& GetSymbolTable() {
    return tab_;
  }

 private:
  SymbolMap<std::string> mp_;
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
  void BuildTypedefID(AstNode* id_item, DataType type);
  void BuildVariableDecl(AstNode* var_decl) noexcept;
  void BuildInitID(AstNode* init_id, DataType type) noexcept;

  std::pair<VariableType, TableEntry> BuildParam(AstNode* param);
  std::vector<size_t> ParseDimDecl(const std::list<AstNode*>& dim_decl);
  void InsertSymTab(std::variant<std::string, size_t>& id, TableEntry&& entry,
                    AstNode*, bool is_param = false);
  void InsertParam(AstNode* param, TableEntry&& entry);
  DataType BuildType(AstNode* nd);

  void AnalyzeProgram(AstNode* prog);
  void AnalyzeGlobalDecl(AstNode* decl);
  void AnalyzeFunctionDecl(AstNode* func);
  void AnalyzeBlock(AstNode* block);
  void AnalyzeStmtList(AstNode* stmt_list);
  void AnalyzeDeclList(AstNode* decl_list);
  void AnalyzeVariableDecl(AstNode* var_decl);
  void AnalyzeInitID(AstNode* init_id);
  void AnalyzeStatement(AstNode* stmt);
  void AnalyzeIfStmt(AstNode* stmt);
  void AnalyzeIfElseStmt(AstNode* stmt);
  void AnalyzeForStmt(AstNode* stmt);
  void AnalyzeWhileStmt(AstNode* stmt);
  void AnalyzeRelopExprList(AstNode* relop_expr_list);
  void AnalyzeAssignExprList(AstNode* assign_expr_list);
  void AnalyzeAssignExpr(AstNode* expr);
  void AnalyzeRelopExpr(AstNode* expr);
  void AnalyzeFunctionCall(AstNode* node);
  void AnalyzeVarRef(AstNode* var);
};

#endif  // ANALYSIS_H_
