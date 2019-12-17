#ifndef GENERATOR_H_
#define GENERATOR_H_

#include <fstream>
#include <vector>

#include "entry.h"

class Generator {
 public:
 private:
  std::vector<TableEntry> tab_;  // symbol table
  std::ofstream ofs_;            // output stream
  enum {
    UNKNOWN_SECTION,
    DATA_SECTION,
    TEXT_SECTION
  } section_ = UNKNOWN_SECTION;

  /*** first pass: calculate the offset (to fp) of each local variables ***/
  void VisitProgram(AstNode *prog);
  void VisitGlobalDecl(AstNode *prog);
  void VisitFunctionDecl(AstNode *decl);
  void VisitStmtList(AstNode *stmt_list, size_t &offset);
  void VisitStatement(AstNode *stmt, size_t &offset);
  void VisitDeclList(AstNode *decl_list, size_t &offset);
  void VisitVariableDecl(AstNode *decl, size_t &offset);
  void VisitBlock(AstNode *block, size_t &offset);

  /*** second pass: generate RISC-V assembly ***/
  void GenerateVariableDecl(AstNode *var_decl, bool global);
  void GenerateFunctionDecl(AstNode *func_decl);
  void GenerateGlobalDecl(AstNode *decl);
  void GenerateProgram(AstNode *prog);
};

#endif  // GENERATOR_H_
