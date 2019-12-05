#ifndef ANALYSIS_H_
#define ANALYSIS_H_

#include <string>
#include <utility>
#include <vector>

#include "entry.h"
#include "symtab.h"

struct SemanticError {
  enum ErrorType {
    VAR_REDECL,          // redeclaration of variable
    VAR_UNDECL,          // variable undeclared
    INCOMPAT_ARRAY_DIM,  // incompatible array dimensions
    TOO_FEW_ARGS,        // too few arguments
    TOO_MANY_ARGS,       // too many arguments
  } error;
  Location loc1, loc2;
  std::string msg;

  SemanticError() = default;
  SemanticError(ErrorType err) : error(err) {}
  SemanticError(ErrorType err, std::string&& s) : error(err), msg(s) {}
};

class Analyzer {
 public:
  void SemanticAnalysis(AstNode* prog);
  void BuildSymbolTable(AstNode* prog);

 private:
  std::vector<SemanticError> err_;
  SymbolMap<std::string> mp_;
  std::vector<TableEntry> tab_;
  DataType return_type_ = NONE_TYPE;

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
  void BuildFunctionCall(AstNode* node);
  void BuildVarRef(AstNode* node, bool is_function_arg = false);
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
  void AnalyzeStmtList(AstNode* stmt_list);
  void AnalyzeStatement(AstNode* stmt);
  void AnalyzeIfStmt(AstNode* stmt);
  void AnalyzeIfElseStmt(AstNode* stmt);
  void AnalyzeForStmt(AstNode* stmt);
  void AnalyzeWhileStmt(AstNode* stmt);
  void AnalyzeRelopExprList(AstNode* relop_expr_list, bool is_function_arg = false);
  void AnalyzeAssignExprList(AstNode* assign_expr_list);
  void AnalyzeAssignExpr(AstNode* expr);
  void AnalyzeRelopExpr(AstNode* expr, bool is_function_arg = false);
  void AnalyzeFunctionCall(AstNode* node);
  void AnalyzeVarRef(AstNode* var, bool is_function_arg = false);
};

#endif  // ANALYSIS_H_
