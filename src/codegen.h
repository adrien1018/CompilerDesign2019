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

  void CodeGeneration(AstNode* prog, const CodeGenOptions& opt) {
    opt_ = opt;
    VisitProgram(prog);
  }
  CodeGenInfo MoveInfo() {
    return std::tie(ir_, labels_, data_, tab_, func_, opt_, func_reg_info_);
  }

 private:
  CodeGenOptions opt_;
  std::vector<FuncRegInfo> func_reg_info_;
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
  struct FuncSpace {
    RegCount reg;
    size_t stk;
  } cur_, max_;

  size_t InsertLabel(bool func = false);
  void InitState(FunctionAttr&);
  size_t AllocStack(size_t);
  size_t AllocRegister(DataType);
  void LoadConst(uint64_t x, size_t dest);
  void VisitProgram(AstNode*);
  void VisitGlobalDecl(AstNode*);
  void VisitFunctionDecl(AstNode*);
  void VisitStmtList(AstNode*, FunctionAttr&);
  void VisitStatement(AstNode*, FunctionAttr&);
  void VisitDeclList(AstNode*, bool global);
  void VisitVariableDecl(AstNode*, bool global);
  void VisitBlock(AstNode* block, FunctionAttr&);
  void VisitRelopExpr(AstNode*, size_t dest);
  void VisitRelopExprList(AstNode*, size_t dest);
  void VisitConversion(AstNode*, size_t dest);
  void VisitOpr(AstNode*, size_t dest);
  void VisitConst(AstNode*, size_t dest);
  void VisitIdentifier(AstNode*, size_t dest);
  void VisitAssignment(AstNode*);
  void VisitAssignmentList(AstNode*);
  bool VisitArray(AstNode*, size_t dest);
  void VisitFunctionCall(AstNode*, size_t dest);

  void OptFunctionRegAlloc(FunctionAttr&);
#if CODEGEN_DEBUG
  void PrintIR();
#endif
};

#endif  // GENERATOR_H_
