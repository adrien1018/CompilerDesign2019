#ifndef INSTRUCTION_H_
#define INSTRUCTION_H_

#include <fstream>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

enum Opcode {
  INSR_LUI,     // Load Upper Immediate
  INSR_AUIPC,   // Add Upper Immediate to PC
  INSR_JAL,     // Jump and Link
  INSR_JALR,    // Jump and Link Register
  INSR_BEQ,     // Branch Equal
  INSR_BNE,     // Branch Not Equal
  INSR_BLT,     // Branch Less Than
  INSR_BGE,     // Branch Greater than Equal
  INSR_BLTU,    // Branch Less Than Unsigned
  INSR_BGEU,    // Branch Greater than Equal Unsigned
  INSR_LB,      // Load Byte
  INSR_LH,      // Load Half
  INSR_LW,      // Load Word
  INSR_LD,      // Load Double
  INSR_LBU,     // Load Byte Unsigned
  INSR_LHU,     // Load Half Unsigned
  INSR_LWU,     // Load Word Unsigned
  INSR_SB,      // Store Byte
  INSR_SH,      // Store Half
  INSR_SW,      // Store Word
  INSR_SD,      // Store Double
  INSR_ADDI,    // Add Immediate
  INSR_SLTI,    // Set Less Than Immediate
  INSR_SLTIU,   // Set Less Than Immediate Unsigned
  INSR_XORI,    // Xor Immediate
  INSR_ORI,     // Or Immediate
  INSR_ANDI,    // And Immediate
  INSR_SLLI,    // Shift Left Logical Immediate
  INSR_SRLI,    // Shift Right Logical Immediate
  INSR_SRAI,    // Shift Right Arithmetic Immediate
  INSR_ADDIW,   // Add Immediate Word
  INSR_SLLIW,   // Shift Left Logical Immediate Word
  INSR_SRLIW,   // Shift Right Logical Immediate Word
  INSR_SRAIW,   // Shift Right Arithmetic Immediate Word
  INSR_ADD,     // Add
  INSR_SUB,     // Subtract
  INSR_SLL,     // Shift Left Logical
  INSR_SLT,     // Set Less Than
  INSR_SLTU,    // Set Less Than Unsigned
  INSR_XOR,     // Xor
  INSR_SRL,     // Shift Right Logical
  INSR_SRA,     // Shift Right Arithmetic
  INSR_OR,      // Or
  INSR_AND,     // And
  INSR_ADDW,    // Add Word
  INSR_SUBW,    // Subtract Word
  INSR_SLLW,    // Shift Left Logical Word
  INSR_SRLW,    // Shift Right Logical Word
  INSR_SRAW,    // Shift Right Arithmetic Word
  INSR_MUL,     // Multiply
  INSR_MULH,    // Multiply High Signed Signed
  INSR_MULHSU,  // Multiply High Signed Unsigned
  INSR_MULHU,   // Multiply High Unsigned Unsigned
  INSR_DIV,     // Divide Signed
  INSR_DIVU,    // Divide Unsigned
  INSR_REM,     // Remainder Signed
  INSR_REMU,    // Remainder Unsigned
  INSR_MULW,    // Multiple Word
  INSR_DIVW,    // Divide Signed Word
  INSR_DIVUW,   // Divide Unsigned Word
  INSR_REMW,    // Remainder Signed Word
  INSR_REMUW,   // Remainder Unsigned Word
  kIntegerInsr,

  // Floating point
  INSR_FLW,  // load
  INSR_FLD,
  INSR_FSW,  // store
  INSR_FSD,
  INSR_FMADD_S,  // fused mul-add
  INSR_FMSUB_S,
  INSR_FNMADD_S,
  INSR_FNMSUB_S,
  INSR_FMADD_D,
  INSR_FMSUB_D,
  INSR_FNMADD_D,
  INSR_FNMSUB_D,
  INSR_FADD_S,  // arith
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
  INSR_FCVT_W_S,  // float to int
  INSR_FCVT_WU_S,
  INSR_FCVT_L_S,
  INSR_FCVT_LU_S,
  INSR_FCVT_W_D,
  INSR_FCVT_WU_D,
  INSR_FCVT_L_D,
  INSR_FCVT_LU_D,
  INSR_FCVT_S_W,  // int to float
  INSR_FCVT_S_WU,
  INSR_FCVT_S_L,
  INSR_FCVT_S_LU,
  INSR_FCVT_D_W,
  INSR_FCVT_D_WU,
  INSR_FCVT_D_L,
  INSR_FCVT_D_LU,
  INSR_FCVT_S_D,  // float convert
  INSR_FCVT_D_S,
  INSR_FMV_X_W,  // bitwise float to int
  INSR_FMV_X_D,
  INSR_FMV_W_X,  // bitwise int to float
  INSR_FMV_D_X,
  INSR_FEQ_S,  // compare
  INSR_FLT_S,
  INSR_FLE_S,
  INSR_FEQ_D,
  INSR_FLT_D,
  INSR_FLE_D,
  INSR_FCLASS_S,  // class
  INSR_FCLASS_D,
  kFloatingPointInsr,

  // Pseudo-instructions
  PINSR_J,     // jump (+dest ID)
  PINSR_CALL,  // call function (+dest ID)
  PINSR_TAIL,  // tail call function (+dest ID)
  PINSR_RET,   // return (no arg)
};

enum InsrFormat {
  R_TYPE,   // Register
  I_TYPE,   // Immediate
  S_TYPE,   // Store
  B_TYPE,   // Branch
  U_TYPE,   // Upper immediate
  J_TYPE,   // Jump
  R0_TYPE,  // R-type with rs2 intentionally left blank
  R4_TYPE,  // Fusion
  PSEUDO    // Pseudo instructions
};

const std::unordered_map<Opcode, std::string> kRV64InsrCode = {
    {INSR_LUI, "LUI"},
    {INSR_AUIPC, "AUIPC"},
    {INSR_JAL, "JAL"},
    {INSR_JALR, "JALR"},
    {INSR_BEQ, "BEQ"},
    {INSR_BNE, "BNE"},
    {INSR_BLT, "BLT"},
    {INSR_BGE, "BGE"},
    {INSR_BLTU, "BLTU"},
    {INSR_BGEU, "BGEU"},
    {INSR_LB, "LB"},
    {INSR_LH, "LH"},
    {INSR_LW, "LW"},
    {INSR_LD, "LD"},
    {INSR_LBU, "LBU"},
    {INSR_LHU, "LHU"},
    {INSR_LWU, "LWU"},
    {INSR_SB, "SB"},
    {INSR_SH, "SH"},
    {INSR_SW, "SW"},
    {INSR_SD, "SD"},
    {INSR_ADDI, "ADDI"},
    {INSR_SLTI, "SLTI"},
    {INSR_SLTIU, "SLTIU"},
    {INSR_XORI, "XORI"},
    {INSR_ORI, "ORI"},
    {INSR_ANDI, "ANDI"},
    {INSR_SLLI, "SLLI"},
    {INSR_SRLI, "SRLI"},
    {INSR_SRAI, "SRAI"},
    {INSR_ADDIW, "ADDIW"},
    {INSR_SLLIW, "SLLIW"},
    {INSR_SRLIW, "SRLIW"},
    {INSR_SRAIW, "SRAIW"},
    {INSR_ADD, "ADD"},
    {INSR_SUB, "SUB"},
    {INSR_SLL, "SLL"},
    {INSR_SLT, "SLT"},
    {INSR_SLTU, "SLTU"},
    {INSR_XOR, "XOR"},
    {INSR_SRL, "SRL"},
    {INSR_SRA, "SRA"},
    {INSR_OR, "OR"},
    {INSR_AND, "AND"},
    {INSR_ADDW, "ADDW"},
    {INSR_SUBW, "SUBW"},
    {INSR_SLLW, "SLLW"},
    {INSR_SRLW, "SRLW"},
    {INSR_SRAW, "SRAW"},
    {INSR_MUL, "MUL"},
    {INSR_MULH, "MULH"},
    {INSR_MULHSU, "MULHSU"},
    {INSR_MULHU, "MULHU"},
    {INSR_DIV, "DIV"},
    {INSR_DIVU, "DIVU"},
    {INSR_REM, "REM"},
    {INSR_REMU, "REMU"},
    {INSR_MULW, "MULW"},
    {INSR_DIVW, "DIVW"},
    {INSR_DIVUW, "DIVUW"},
    {INSR_REMW, "REMW"},
    {INSR_REMUW, "REMUW"},
    {INSR_FLW, "FLW"},
    {INSR_FLD, "FLD"},
    {INSR_FSW, "FSW"},
    {INSR_FSD, "FSD"},
    {INSR_FMADD_S, "FMADD.S"},
    {INSR_FMSUB_S, "FMSUB.S"},
    {INSR_FNMADD_S, "FNMADD.S"},
    {INSR_FNMSUB_S, "FNMSUB.S"},
    {INSR_FMADD_D, "FMADD.D"},
    {INSR_FMSUB_D, "FMSUB.D"},
    {INSR_FNMADD_D, "FNMADD.D"},
    {INSR_FNMSUB_D, "FNMSUB.D"},
    {INSR_FADD_S, "FADD.S"},
    {INSR_FSUB_S, "FSUB.S"},
    {INSR_FMUL_S, "FMUL.S"},
    {INSR_FDIV_S, "FDIV.S"},
    {INSR_FSQRT_S, "FSQRT.S"},
    {INSR_FSGNJ_S, "FSGNJ.S"},
    {INSR_FSGNJN_S, "FSGNJN.S"},
    {INSR_FSGNJX_S, "FSGNJX.S"},
    {INSR_FMIN_S, "FMIN.S"},
    {INSR_FMAX_S, "FMAX.S"},
    {INSR_FADD_D, "FADD.D"},
    {INSR_FSUB_D, "FSUB.D"},
    {INSR_FMUL_D, "FMUL.D"},
    {INSR_FDIV_D, "FDIV.D"},
    {INSR_FSQRT_D, "FSQRT_D"},
    {INSR_FSGNJ_D, "FSGNJ_D"},
    {INSR_FSGNJN_D, "FSGNJN_D"},
    {INSR_FSGNJX_D, "FSGNJX_D"},
    {INSR_FMIN_D, "FMIN_D"},
    {INSR_FMAX_D, "FMAX_D"},
    {INSR_FCVT_W_S, "FCVT.W.S"},
    {INSR_FCVT_WU_S, "FCVT.WU.S"},
    {INSR_FCVT_L_S, "FCVT.L.S"},
    {INSR_FCVT_LU_S, "FCVT.LU.S"},
    {INSR_FCVT_W_D, "FCVT.W.D"},
    {INSR_FCVT_WU_D, "FCVT.WU.D"},
    {INSR_FCVT_L_D, "FCVT.L.D"},
    {INSR_FCVT_LU_D, "FCVT.LU.D"},
    {INSR_FCVT_S_W, "FCVT.S.W"},
    {INSR_FCVT_S_WU, "FCVT.S.WU"},
    {INSR_FCVT_S_L, "FCVT.S.L"},
    {INSR_FCVT_S_LU, "FCVT.S.LU"},
    {INSR_FCVT_D_W, "FCVT.D.W"},
    {INSR_FCVT_D_WU, "FCVT.D.WU"},
    {INSR_FCVT_D_L, "FCVT.D.L"},
    {INSR_FCVT_D_LU, "FCVT.D.LU"},
    {INSR_FCVT_S_D, "FCVT.S.D"},
    {INSR_FCVT_D_S, "FCVT.D.S"},
    {INSR_FMV_X_W, "FMV.X.W"},
    {INSR_FMV_X_D, "FMV.X.D"},
    {INSR_FMV_W_X, "FMV.W.X"},
    {INSR_FMV_D_X, "FMV.D.X"},
    {INSR_FEQ_S, "FEQ.S"},
    {INSR_FLT_S, "FLT.S"},
    {INSR_FLE_S, "FLE.S"},
    {INSR_FEQ_D, "FEQ.D"},
    {INSR_FLT_D, "FLT.D"},
    {INSR_FLE_D, "FLE.D"},
    {INSR_FCLASS_S, "FCLASS.S"},
    {INSR_FCLASS_D, "FCLASS.D"},
    {PINSR_J, "J"},
    {PINSR_CALL, "CALL"},
    {PINSR_TAIL, "TAIL"},
    {PINSR_RET, "RET"}};

const std::unordered_map<Opcode, InsrFormat> kRV64InsrFormat = {
    {INSR_LUI, U_TYPE},       {INSR_AUIPC, J_TYPE},
    {INSR_JAL, J_TYPE},       {INSR_JALR, I_TYPE},
    {INSR_BEQ, B_TYPE},       {INSR_BNE, B_TYPE},
    {INSR_BLT, B_TYPE},       {INSR_BGE, B_TYPE},
    {INSR_BLTU, B_TYPE},      {INSR_BGEU, B_TYPE},
    {INSR_LB, I_TYPE},        {INSR_LH, I_TYPE},
    {INSR_LW, I_TYPE},        {INSR_LD, I_TYPE},
    {INSR_LBU, I_TYPE},       {INSR_LHU, I_TYPE},
    {INSR_LWU, I_TYPE},       {INSR_SB, S_TYPE},
    {INSR_SH, S_TYPE},        {INSR_SW, S_TYPE},
    {INSR_SD, S_TYPE},        {INSR_ADDI, I_TYPE},
    {INSR_SLTI, I_TYPE},      {INSR_SLTIU, I_TYPE},
    {INSR_XORI, I_TYPE},      {INSR_ORI, I_TYPE},
    {INSR_ANDI, I_TYPE},      {INSR_SLLI, I_TYPE},
    {INSR_SRLI, I_TYPE},      {INSR_SRAI, I_TYPE},
    {INSR_ADDIW, I_TYPE},     {INSR_SLLIW, I_TYPE},
    {INSR_SRLIW, I_TYPE},     {INSR_SRAIW, I_TYPE},
    {INSR_ADD, R_TYPE},       {INSR_SUB, R_TYPE},
    {INSR_SLL, R_TYPE},       {INSR_SLT, R_TYPE},
    {INSR_SLTU, R_TYPE},      {INSR_XOR, R_TYPE},
    {INSR_SRL, R_TYPE},       {INSR_SRA, R_TYPE},
    {INSR_OR, R_TYPE},        {INSR_AND, R_TYPE},
    {INSR_ADDW, R_TYPE},      {INSR_SUBW, R_TYPE},
    {INSR_SLLW, R_TYPE},      {INSR_SRLW, R_TYPE},
    {INSR_SRAW, R_TYPE},      {INSR_MUL, R_TYPE},
    {INSR_MULH, R_TYPE},      {INSR_MULHSU, R_TYPE},
    {INSR_MULHU, R_TYPE},     {INSR_DIV, R_TYPE},
    {INSR_DIVU, R_TYPE},      {INSR_REM, R_TYPE},
    {INSR_REMU, R_TYPE},      {INSR_MULW, R_TYPE},
    {INSR_DIVW, R_TYPE},      {INSR_DIVUW, R_TYPE},
    {INSR_REMW, R_TYPE},      {INSR_REMUW, R_TYPE},
    {INSR_FLW, I_TYPE},       {INSR_FLD, I_TYPE},
    {INSR_FSW, S_TYPE},       {INSR_FSD, S_TYPE},
    {INSR_FMADD_S, R4_TYPE},  {INSR_FMSUB_S, R4_TYPE},
    {INSR_FNMADD_S, R4_TYPE}, {INSR_FNMSUB_S, R4_TYPE},
    {INSR_FMADD_D, R4_TYPE},  {INSR_FMSUB_D, R4_TYPE},
    {INSR_FNMADD_D, R4_TYPE}, {INSR_FNMSUB_D, R4_TYPE},
    {INSR_FADD_S, R_TYPE},    {INSR_FSUB_S, R_TYPE},
    {INSR_FMUL_S, R_TYPE},    {INSR_FDIV_S, R_TYPE},
    {INSR_FSQRT_S, R0_TYPE},  {INSR_FSGNJ_S, R_TYPE},
    {INSR_FSGNJN_S, R_TYPE},  {INSR_FSGNJX_S, R_TYPE},
    {INSR_FMIN_S, R_TYPE},    {INSR_FMAX_S, R_TYPE},
    {INSR_FADD_D, R_TYPE},    {INSR_FSUB_D, R_TYPE},
    {INSR_FMUL_D, R_TYPE},    {INSR_FDIV_D, R_TYPE},
    {INSR_FSQRT_D, R0_TYPE},  {INSR_FSGNJ_D, R_TYPE},
    {INSR_FSGNJN_D, R_TYPE},  {INSR_FSGNJX_D, R_TYPE},
    {INSR_FMIN_D, R_TYPE},    {INSR_FMAX_D, R_TYPE},
    {INSR_FCVT_W_S, R0_TYPE}, {INSR_FCVT_WU_S, R0_TYPE},
    {INSR_FCVT_L_S, R0_TYPE}, {INSR_FCVT_LU_S, R0_TYPE},
    {INSR_FCVT_W_D, R0_TYPE}, {INSR_FCVT_WU_D, R0_TYPE},
    {INSR_FCVT_L_D, R0_TYPE}, {INSR_FCVT_LU_D, R0_TYPE},
    {INSR_FCVT_S_W, R0_TYPE}, {INSR_FCVT_S_WU, R0_TYPE},
    {INSR_FCVT_S_L, R0_TYPE}, {INSR_FCVT_S_LU, R0_TYPE},
    {INSR_FCVT_D_W, R0_TYPE}, {INSR_FCVT_D_WU, R0_TYPE},
    {INSR_FCVT_D_L, R0_TYPE}, {INSR_FCVT_D_LU, R0_TYPE},
    {INSR_FCVT_S_D, R0_TYPE}, {INSR_FCVT_D_S, R0_TYPE},
    {INSR_FMV_X_W, R0_TYPE},  {INSR_FMV_X_D, R0_TYPE},
    {INSR_FMV_W_X, R0_TYPE},  {INSR_FMV_D_X, R0_TYPE},
    {INSR_FEQ_S, R_TYPE},     {INSR_FLT_S, R_TYPE},
    {INSR_FLE_S, R_TYPE},     {INSR_FEQ_D, R_TYPE},
    {INSR_FLT_D, R_TYPE},     {INSR_FLE_D, R_TYPE},
    {INSR_FCLASS_S, R0_TYPE}, {INSR_FCLASS_D, R0_TYPE},
    {PINSR_J, PSEUDO},        {PINSR_CALL, PSEUDO},
    {PINSR_TAIL, PSEUDO},     {PINSR_RET, PSEUDO}};

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
  int64_t imm;  // or destination ID
};

class InsrGen {
 public:
  // void GenerateAR();

 private:
  std::ofstream ofs_;
  std::vector<RV64Insr> insr_;
};

#endif  // INSTRUCTION_H_
