#ifndef GENERATOR_H_
#define GENERATOR_H_

#include <cstdint>
#include <fstream>
#include <vector>

#include "entry.h"

enum Opcode {
  INSR_LUI, // load upper
  INSR_AUIPC, // store pc
  INSR_JAL, // jump
  INSR_JALR, // indirect jump
  INSR_BEQ, // branch
  INSR_BNE,
  INSR_BLT,
  INSR_BGE,
  INSR_BLTU,
  INSR_BGEU,
  INSR_LB, // load
  INSR_LH,
  INSR_LW,
  INSR_LD,
  INSR_LBU,
  INSR_LHU,
  INSR_LWU,
  INSR_SB, // store
  INSR_SH,
  INSR_SW,
  INSR_SD,
  INSR_ADDI, // immediate arith
  INSR_SLTI,
  INSR_SLTIU,
  INSR_XORI,
  INSR_ORI,
  INSR_ANDI,
  INSR_SLLI,
  INSR_SRLI,
  INSR_SRAI,
  INSR_ADDIW,
  INSR_SLLIW,
  INSR_SRLIW,
  INSR_SRAIW,
  INSR_ADD, // arith
  INSR_SUB,
  INSR_SLL,
  INSR_SLT,
  INSR_SLTU,
  INSR_XOR,
  INSR_SRL,
  INSR_SRA,
  INSR_OR,
  INSR_AND,
  INSR_ADDW,
  INSR_SUBW,
  INSR_SLLW,
  INSR_SRLW,
  INSR_SRAW,
  INSR_MUL,
  INSR_MULH,
  INSR_MULHSU,
  INSR_MULHU,
  INSR_DIV,
  INSR_DIVU,
  INSR_REM,
  INSR_REMU,
  INSR_MULW,
  INSR_DIVW,
  INSR_DIVUW,
  INSR_REMW,
  INSR_REMUW,
// Floating point
  INSR_FLW, // load
  INSR_FLD,
  INSR_FSW, // store
  INSR_FSD,
  INSR_FMADD_S, // fused mul-add
  INSR_FMSUB_S,
  INSR_FNMADD_S,
  INSR_FNMSUB_S,
  INSR_FMADD_D,
  INSR_FMSUB_D,
  INSR_FNMADD_D,
  INSR_FNMSUB_D,
  INSR_FADD_S, // arith
  INSR_FSUB_S,
  INSR_FMUL_S,
  INSR_FDIV_S,
  INSR_FSQRT_S,
  INSR_FSGNJ_S,
  INSR_FSGNJN_S,
  INSR_FSGNJX_S,
  INSR_FMIN_S,
  INSR_FMAX_S,
  INSR_FADD_D,
  INSR_FSUB_D,
  INSR_FMUL_D,
  INSR_FDIV_D,
  INSR_FSQRT_D,
  INSR_FSGNJ_D,
  INSR_FSGNJN_D,
  INSR_FSGNJX_D,
  INSR_FMIN_D,
  INSR_FMAX_D,
  INSR_FCVT_W_S, // float to int
  INSR_FCVT_WU_S,
  INSR_FCVT_L_S,
  INSR_FCVT_LU_S,
  INSR_FCVT_W_D,
  INSR_FCVT_WU_D,
  INSR_FCVT_L_D,
  INSR_FCVT_LU_D,
  INSR_FCVT_S_W, // int to float
  INSR_FCVT_S_WU,
  INSR_FCVT_S_L,
  INSR_FCVT_S_LU,
  INSR_FCVT_D_W,
  INSR_FCVT_D_WU,
  INSR_FCVT_D_L,
  INSR_FCVT_D_LU,
  INSR_FCVT_S_D, // float convert
  INSR_FCVT_D_S,
  INSR_FMV_X_W, // bitwise float to int
  INSR_FMV_X_D,
  INSR_FMV_W_X, // bitwise int to float
  INSR_FMV_D_X,
  INSR_FEQ_S, // compare
  INSR_FLT_S,
  INSR_FLE_S,
  INSR_FEQ_D,
  INSR_FLT_D,
  INSR_FLE_D,
  INSR_FCLASS_S, // class
  INSR_FCLASS_D,
// Pseudo-instructions
  PINSR_J, // jump (+dest ID)
  PINSR_CALL, // call function (+dest ID)
  PINSR_TAIL, // tail call function (+dest ID)
  PINSR_RET, // return (no arg)
};

// num or initialized array, string constant or uninitialized array
using CodeData = std::variant<std::vector<uint8_t>, std::string, size_t>;

struct IRInsr {
  Opcode op;
  size_t rs1, rs2, rs3, rd;
  int64_t imm;
};

struct RV64Insr {
  Opcode op;
  uint8_t rs1, rs2, rs3, rd;
  int64_t imm; // or destination ID
};

class CodeGen {
 public:
  CodeGen() = default;
  CodeGen(std::vector<TableEntry> &&tab) : tab_(tab) {}

  void CodeGeneration(AstNode *prog) { VisitProgram(prog); }

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
