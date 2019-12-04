#ifndef ANALYSIS_H_
#define ANALYSIS_H_

#include <utility>
#include <vector>

#include "entry.h"
#include "symtab.h"

class Analyzer {
 public:
  void SemanticAnalysis(AstNode* prog);
  void BuildSymbolTable(AstNode* prog);

 private:
  std::vector<SemanticError> err_;
  SymbolMap<std::string> mp_;
  std::vector<TableEntry> tab_;

  void BuildProgram(AstNode* prog);
  void BuildGlobalDecl(AstNode* decl);
  void BuildFunctionDecl(AstNode* func_decl);
  void BuildBlock(AstNode* block);
  void BuildDeclList(AstNode* decl_list);
  void BuildStmtList(AstNode* stmt_list);
  void BuildStatement(AstNode* stmt);
  void BuildAssignExprList(AstNode* assign_expr_list);
  void BuildRelopExprList(AstNode* relop_expr_list);
  void BuildAssignExpr(AstNode* expr);
  void BuildRelopExpr(AstNode* expr);
  void BuildFunctionCall(AstNode* node);
  void BuildVarRef(AstNode* node);
  void BuildTypeDecl(AstNode* type_decl);
  void BuildTypedefID(AstNode* id_item, DataType type);
  void BuildVariableDecl(AstNode* var_decl);
  void BuildInitID(AstNode* init_id, DataType type);

  VariableType BuildParam(AstNode* param);
  std::vector<size_t> ParseDimDecl(const std::list<AstNode*>& dim_decl);
  void InsertSymTab(std::variant<std::string, size_t>& id, TableEntry&& entry);
  DataType BuildType(AstNode* nd);

  void AnalyzeProgram(AstNode* prog);
  void AnalyzeGlobalDecl(AstNode* decl);
  void AnalyzeFunctionDecl(AstNode* func);
  void AnalyzeBlock(AstNode* block);
  void AnalyzeDeclList(AstNode* decl_list);
  void AnalyzeStmtList(AstNode* stmt_list);
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
