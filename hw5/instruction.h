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
  // Pseudo-instructions
  PINSR_J,     // jump (+dest ID)
  PINSR_CALL,  // call function (+dest ID)
  PINSR_TAIL,  // tail call function (+dest ID)
  PINSR_RET,   // return (no arg)
};

enum InsrFormat { R_TYPE, I_TYPE, S_TYPE, B_TYPE, U_TYPE, J_TYPE };

const std::unordered_map<Opcode, std::string> kRV64InsrCode = {
    {INSR_LUI, "lui"},
    {INSR_AUIPC, "auipc"},
    {INSR_JAL, "jal"},
    {INSR_JALR, "jalr"},
    {INSR_BEQ, "beq"},
    {INSR_BNE, "bne"},
    {INSR_BLT, "blt"},
    {INSR_BGE, "bge"},
    {INSR_BLTU, "bltu"},
    {INSR_BGEU, "bgeu"},
    {INSR_LB, "lb"},
    {INSR_LH, "lh"},
    {INSR_LW, "lw"},
    {INSR_LD, "ld"},
    {INSR_LBU, "lbu"},
    {INSR_LHU, "lhu"},
    {INSR_LWU, "lwu"},
    {INSR_SB, "sb"},
    {INSR_SH, "sh"},
    {INSR_SW, "sw"},
    {INSR_SD, "sd"},
    {INSR_ADDI, "addi"},
    {INSR_SLTI, "slti"},
    {INSR_SLTIU, "sltiu"},
    {INSR_XORI, "xori"},
    {INSR_ORI, "ori"},
    {INSR_ANDI, "andi"},
    {INSR_SLLI, "slli"},
    {INSR_SRLI, "srli"},
    {INSR_SRAI, "srai"},
    {INSR_ADDIW, "addiw"},
    {INSR_SLLIW, "slliw"},
    {INSR_SRLIW, "srliw"},
    {INSR_SRAIW, "sraiw"},
    {INSR_ADD, "add"},
    {INSR_SUB, "sub"},
    {INSR_SLL, "sll"},
    {INSR_SLT, "slt"},
    {INSR_SLTU, "sltu"},
    {INSR_XOR, "xor"},
    {INSR_SRL, "srl"},
    {INSR_SRA, "sra"},
    {INSR_OR, "or"},
    {INSR_AND, "and"},
    {INSR_ADDW, "addw"},
    {INSR_SUBW, "subw"},
    {INSR_SLLW, "sllw"},
    {INSR_SRLW, "srlw"},
    {INSR_SRAW, "sraw"},
    {INSR_MUL, "mul"},
    {INSR_MULH, "mulh"},
    {INSR_MULHSU, "mulhsu"},
    {INSR_MULHU, "mulhu"},
    {INSR_DIV, "div"},
    {INSR_DIVU, "divu"},
    {INSR_REM, "rem"},
    {INSR_REMU, "remu"},
    {INSR_MULW, "mulw"},
    {INSR_DIVW, "divw"},
    {INSR_DIVUW, "divuw"},
    {INSR_REMW, "remw"},
    {INSR_REMUW, "remuw"},
    {INSR_FLW, "flw"},
    {INSR_FLD, "fld"},
    {INSR_FSW, "fsw"},
    {INSR_FSD, "fsd"},
    {INSR_FMADD_S, "fmadd_s"},
    {INSR_FMSUB_S, "fmsub_s"},
    {INSR_FNMADD_S, "fnmadd_s"},
    {INSR_FNMSUB_S, "fnmsub_s"},
    {INSR_FMADD_D, "fmadd_d"},
    {INSR_FMSUB_D, "fmsub_d"},
    {INSR_FNMADD_D, "fnmadd_d"},
    {INSR_FNMSUB_D, "fnmsub_d"},
    {INSR_FADD_S, "fadd_s"},
    {INSR_FSUB_S, "fsub_s"},
    {INSR_FMUL_S, "fmul_s"},
    {INSR_FDIV_S, "fdiv_s"},
    {INSR_FSQRT_S, "fsqrt_s"},
    {INSR_FSGNJ_S, "fsgnj_s"},
    {INSR_FSGNJN_S, "fsgnjn_s"},
    {INSR_FSGNJX_S, "fsgnjx_s"},
    {INSR_FMIN_S, "fmin_s"},
    {INSR_FMAX_S, "fmax_s"},
    {INSR_FADD_D, "fadd_d"},
    {INSR_FSUB_D, "fsub_d"},
    {INSR_FMUL_D, "fmul_d"},
    {INSR_FDIV_D, "fdiv_d"},
    {INSR_FSQRT_D, "fsqrt_d"},
    {INSR_FSGNJ_D, "fsgnj_d"},
    {INSR_FSGNJN_D, "fsgnjn_d"},
    {INSR_FSGNJX_D, "fsgnjx_d"},
    {INSR_FMIN_D, "fmin_d"},
    {INSR_FMAX_D, "fmax_d"},
    {INSR_FCVT_W_S, "fcvt_w_s"},
    {INSR_FCVT_WU_S, "fcvt_wu_s"},
    {INSR_FCVT_L_S, "fcvt_l_s"},
    {INSR_FCVT_LU_S, "fcvt_lu_s"},
    {INSR_FCVT_W_D, "fcvt_w_d"},
    {INSR_FCVT_WU_D, "fcvt_wu_d"},
    {INSR_FCVT_L_D, "fcvt_l_d"},
    {INSR_FCVT_LU_D, "fcvt_lu_d"},
    {INSR_FCVT_S_W, "fcvt_s_w"},
    {INSR_FCVT_S_WU, "fcvt_s_wu"},
    {INSR_FCVT_S_L, "fcvt_s_l"},
    {INSR_FCVT_S_LU, "fcvt_s_lu"},
    {INSR_FCVT_D_W, "fcvt_d_w"},
    {INSR_FCVT_D_WU, "fcvt_d_wu"},
    {INSR_FCVT_D_L, "fcvt_d_l"},
    {INSR_FCVT_D_LU, "fcvt_d_lu"},
    {INSR_FCVT_S_D, "fcvt_s_d"},
    {INSR_FCVT_D_S, "fcvt_d_s"},
    {INSR_FMV_X_W, "fmv_x_w"},
    {INSR_FMV_X_D, "fmv_x_d"},
    {INSR_FMV_W_X, "fmv_w_x"},
    {INSR_FMV_D_X, "fmv_d_x"},
    {INSR_FEQ_S, "feq_s"},
    {INSR_FLT_S, "flt_s"},
    {INSR_FLE_S, "fle_s"},
    {INSR_FEQ_D, "feq_d"},
    {INSR_FLT_D, "flt_d"},
    {INSR_FLE_D, "fle_d"},
    {INSR_FCLASS_S, "fclass_s"},
    {INSR_FCLASS_D, "fclass_d"},
    {PINSR_J, "j"},
    {PINSR_CALL, "call"},
    {PINSR_TAIL, "tail"},
    {PINSR_RET, "ret"}};

const std::unordered_map<Opcode, InsrFormat> kRV64InsrFormat = {
    {INSR_LUI, U_TYPE},    {INSR_AUIPC, J_TYPE}, {INSR_JAL, J_TYPE},
    {INSR_JALR, I_TYPE},   {INSR_BEQ, B_TYPE},   {INSR_BNE, B_TYPE},
    {INSR_BLT, B_TYPE},    {INSR_BGE, B_TYPE},   {INSR_BLTU, B_TYPE},
    {INSR_BGEU, B_TYPE},   {INSR_LB, I_TYPE},    {INSR_LH, I_TYPE},
    {INSR_LW, I_TYPE},     {INSR_LD, I_TYPE},    {INSR_LBU, I_TYPE},
    {INSR_LHU, I_TYPE},    {INSR_LWU, I_TYPE},   {INSR_SB, S_TYPE},
    {INSR_SH, S_TYPE},     {INSR_SW, S_TYPE},    {INSR_SD, S_TYPE},
    {INSR_ADDI, I_TYPE},   {INSR_SLTI, I_TYPE},  {INSR_SLTIU, I_TYPE},
    {INSR_XORI, I_TYPE},   {INSR_ORI, I_TYPE},   {INSR_ANDI, I_TYPE},
    {INSR_SLLI, I_TYPE},   {INSR_SRLI, I_TYPE},  {INSR_SRAI, I_TYPE},
    {INSR_ADDIW, I_TYPE},  {INSR_SLLIW, I_TYPE}, {INSR_SRLIW, I_TYPE},
    {INSR_SRAIW, I_TYPE},  {INSR_ADD, R_TYPE},   {INSR_SUB, R_TYPE},
    {INSR_SLL, R_TYPE},    {INSR_SLT, R_TYPE},   {INSR_SLTU, R_TYPE},
    {INSR_XOR, R_TYPE},    {INSR_SRL, R_TYPE},   {INSR_SRA, R_TYPE},
    {INSR_OR, R_TYPE},     {INSR_AND, R_TYPE},   {INSR_ADDW, R_TYPE},
    {INSR_SUBW, R_TYPE},   {INSR_SLLW, R_TYPE},  {INSR_SRLW, R_TYPE},
    {INSR_SRAW, R_TYPE},   {INSR_MUL, R_TYPE},   {INSR_MULH, R_TYPE},
    {INSR_MULHSU, R_TYPE}, {INSR_MULHU, R_TYPE}, {INSR_DIV, R_TYPE},
    {INSR_DIVU, R_TYPE},   {INSR_REM, R_TYPE},   {INSR_REMU, R_TYPE},
    {INSR_MULW, R_TYPE},   {INSR_DIVW, R_TYPE},  {INSR_DIVUW, R_TYPE},
    {INSR_REMW, R_TYPE},   {INSR_REMUW, R_TYPE},
    // {INSR_FLW,
    // {INSR_FLD,
    // {INSR_FSW,
    // {INSR_FSD,
    // {INSR_FMADD_S,
    // {INSR_FMSUB_S,
    // {INSR_FNMADD_S,
    // {INSR_FNMSUB_S,
    // {INSR_FMADD_D,
    // {INSR_FMSUB_D,
    // {INSR_FNMADD_D,
    // {INSR_FNMSUB_D,
    // {INSR_FADD_S,
    // {INSR_FSUB_S,
    // {INSR_FMUL_S,
    // {INSR_FDIV_S,
    // {INSR_FSQRT_S,
    // {INSR_FSGNJ_S,
    // {INSR_FSGNJN_S,
    // {INSR_FSGNJX_S,
    // {INSR_FMIN_S,
    // {INSR_FMAX_S,
    // {INSR_FADD_D,
    // {INSR_FSUB_D,
    // {INSR_FMUL_D,
    // {INSR_FDIV_D,
    // {INSR_FSQRT_D,
    // {INSR_FSGNJ_D,
    // {INSR_FSGNJN_D,
    // {INSR_FSGNJX_D,
    // {INSR_FMIN_D,
    // {INSR_FMAX_D,
    // {INSR_FCVT_W_S,
    // {INSR_FCVT_WU_S,
    // {INSR_FCVT_L_S,
    // {INSR_FCVT_LU_S,
    // {INSR_FCVT_W_D,
    // {INSR_FCVT_WU_D,
    // {INSR_FCVT_L_D,
    // {INSR_FCVT_LU_D,
    // {INSR_FCVT_S_W,
    // {INSR_FCVT_S_WU,
    // {INSR_FCVT_S_L,
    // {INSR_FCVT_S_LU,
    // {INSR_FCVT_D_W,
    // {INSR_FCVT_D_WU,
    // {INSR_FCVT_D_L,
    // {INSR_FCVT_D_LU,
    // {INSR_FCVT_S_D,
    // {INSR_FCVT_D_S,
    // {INSR_FMV_X_W,
    // {INSR_FMV_X_D,
    // {INSR_FMV_W_X,
    // {INSR_FMV_D_X,
    // {INSR_FEQ_S,
    // {INSR_FLT_S,
    // {INSR_FLE_S,
    // {INSR_FEQ_D,
    // {INSR_FLT_D,
    // {INSR_FLE_D,
    // {INSR_FCLASS_S,
    // {INSR_FCLASS_D,
    // {PINSR_J,
    // {PINSR_CALL,
    // {PINSR_TAIL,
    // {PINSR_RET,
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
