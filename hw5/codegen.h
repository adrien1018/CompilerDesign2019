#ifndef GENERATOR_H_
#define GENERATOR_H_

#include <cstdint>
#include <fstream>
#include <vector>

#include "entry.h"
#include "instruction.h"

class CodeGen {
 public:
  CodeGen() = default;
  CodeGen(std::vector<TableEntry> &&tab) : tab_(tab) {}

  void CodeGeneration(AstNode *prog) { VisitProgram(prog); }
  CodeGenInfo MoveInfo() {
    return std::make_tuple(std::move(ir_), std::move(labels_), std::move(data_),
                           std::move(tab_));
  }

 private:
  std::vector<TableEntry> tab_;  // symbol table
  std::ofstream ofs_;            // output stream
  enum {
    UNKNOWN_SECTION,
    DATA_SECTION,
    TEXT_SECTION
  } section_ = UNKNOWN_SECTION;
  std::vector<IRInsr> ir_;
  std::vector<Label> labels_;
  std::vector<CodeData> data_;
  std::vector<Identifier> func_;
  size_t cur_stack_, cur_register_;

  size_t InsertLabel(bool func = false);
  void InitState(FunctionAttr&);
  size_t AllocStack(FunctionAttr&, size_t);
  size_t AllocRegister(FunctionAttr&);
  void LoadConst(uint64_t x, size_t dest);
  void VisitProgram(AstNode* prog);
  void VisitGlobalDecl(AstNode* prog);
  void VisitFunctionDecl(AstNode* decl);
  void VisitStmtList(AstNode* stmt_list, FunctionAttr&);
  void VisitStatement(AstNode* stmt, FunctionAttr&);
  void VisitDeclList(AstNode* decl_list, FunctionAttr&, bool global);
  void VisitVariableDecl(AstNode* decl, FunctionAttr&, bool global);
  void VisitBlock(AstNode* block, FunctionAttr&);
  void VisitRelopExpr(AstNode* expr, FunctionAttr&, size_t dest);
  void VisitConversion(AstNode* expr, FunctionAttr&, size_t dest);
  void VisitOpr(AstNode* expr, FunctionAttr&, size_t dest);
  void VisitConst(AstNode* expr, FunctionAttr&, size_t dest);
  void VisitIdentifier(AstNode* expr, FunctionAttr&, size_t dest);
  void VisitAssignment(AstNode* expr, FunctionAttr&);
  bool VisitArray(AstNode* expr, FunctionAttr&, size_t dest);
  void VisitFunctionCall(AstNode* expr, FunctionAttr&, size_t dest);
#ifdef CODEGEN_DEBUG
  void PrintIR();
#endif
};

#endif  // GENERATOR_H_
