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
  CodeGen(std::vector<TableEntry>&& tab) : tab_(tab) {}

  void CodeGeneration(AstNode* prog) { VisitProgram(prog); }
  CodeGenInfo MoveInfo() { return std::tie(ir_, labels_, data_, tab_, func_); }

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
  RegCount cur_register_;
  size_t cur_stack_;

  size_t InsertLabel(bool func = false);
  void InitState(FunctionAttr&);
  size_t AllocStack(FunctionAttr&, size_t);
  size_t AllocRegister(FunctionAttr&, DataType);
  void LoadConst(uint64_t x, size_t dest);
  void VisitProgram(AstNode*);
  void VisitGlobalDecl(AstNode*);
  void VisitFunctionDecl(AstNode*);
  void VisitStmtList(AstNode*, FunctionAttr&);
  void VisitStatement(AstNode*, FunctionAttr&);
  void VisitDeclList(AstNode*, FunctionAttr&, bool global);
  void VisitVariableDecl(AstNode*, FunctionAttr&, bool global);
  void VisitBlock(AstNode* block, FunctionAttr&);
  void VisitRelopExpr(AstNode*, FunctionAttr&, size_t dest);
  void VisitRelopExprList(AstNode*, FunctionAttr&, size_t dest);
  void VisitConversion(AstNode*, FunctionAttr&, size_t dest);
  void VisitOpr(AstNode*, FunctionAttr&, size_t dest);
  void VisitConst(AstNode*, FunctionAttr&, size_t dest);
  void VisitIdentifier(AstNode*, FunctionAttr&, size_t dest);
  void VisitAssignment(AstNode*, FunctionAttr&);
  void VisitAssignmentList(AstNode*, FunctionAttr&);
  bool VisitArray(AstNode*, FunctionAttr&, size_t dest);
  void VisitFunctionCall(AstNode*, FunctionAttr&, size_t dest);
#ifdef CODEGEN_DEBUG
  void PrintIR();
#endif
};

#endif  // GENERATOR_H_
