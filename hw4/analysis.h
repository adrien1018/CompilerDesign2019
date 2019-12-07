#ifndef ANALYSIS_H_
#define ANALYSIS_H_

#include <string>
#include <utility>
#include <vector>

#include "entry.h"
#include "symtab.h"


class Analyzer {
 public:
  Analyzer(const std::string& filename, std::istream* stream, bool color_output)
      : filename_(filename), stream_(stream), color_output_(color_output) {}
  void SemanticAnalysis(AstNode* prog);
  void BuildSymbolTable(AstNode* prog);
  std::vector<TableEntry>& GetSymbolTable() {
    return tab_;
  }

 private:
  SymbolMap<std::string> mp_;
  std::vector<TableEntry> tab_;
  DataType return_type_ = NONE_TYPE;

  std::string filename_;
  std::istream* stream_;
  bool color_output_;

  void BuildProgram(AstNode* prog);
  void BuildGlobalDecl(AstNode* decl);
  void BuildFunctionDecl(AstNode* func_decl);
  void BuildBlock(AstNode* block);
  void BuildDeclList(AstNode* decl_list);
  void BuildStmtList(AstNode* stmt_list);
  void BuildStatement(AstNode* stmt);
  void BuildAssignExprList(AstNode* assign_expr_list);
  void BuildRelopExprList(AstNode* relop_expr_list,
                          bool is_function_arg = false);
  void BuildAssignExpr(AstNode* expr);
  void BuildRelopExpr(AstNode* expr, bool is_function_arg = false);
  void BuildWriteCall(AstNode* node);
  void BuildFunctionCall(AstNode* node);
  void BuildVarRef(AstNode* node, bool is_function_arg = false);
  void BuildTypeDecl(AstNode* type_decl);
  void BuildTypedefID(AstNode* id_item, DataType type);
  void BuildVariableDecl(AstNode* var_decl);
  void BuildInitID(AstNode* init_id, DataType type);

  std::pair<VariableType, TableEntry> BuildParam(AstNode* param);
  std::vector<size_t> ParseDimDecl(const std::list<AstNode*>& dim_decl);
  void InsertSymTab(std::variant<std::string, size_t>& id, TableEntry&& entry);
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
