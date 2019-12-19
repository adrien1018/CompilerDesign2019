#ifndef INSTRUCTION_H_
#define INSTRUCTION_H_

#include <array>
#include <fstream>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <variant>
#include <vector>

#include "utils.h"

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
  PINSR_LA     // Load absolute address
};

enum InsrFormat {
  R_TYPE,   // Register
  I_TYPE,   // Immediate
  S_TYPE,   // Store
  B_TYPE,   // Branch
  U_TYPE,   // Upper immediate
  J_TYPE,   // Jump
  R0_TYPE,  // R-type with rs2 intentionally left empty
  R4_TYPE,  // Fusion
  PSEUDO    // Pseudo instructions
};

template <size_t N>
constexpr CString<N> ToInsr(const CString<N>& s) {
  CString<N> ret;
  for (size_t i = 0; i < N; i++) {
    if (s.str[i] == '_')
      ret.str[i] = '.';
    else
      ret.str[i] = s.str[i];
  }
  return ret;
}

const std::unordered_map<Opcode, std::string> kRV64InsrCode = {
#define INSR_PAIR(x) \
  { INSR_##x, ToInsr(CString(#x)) }
    INSR_PAIR(LUI),       INSR_PAIR(AUIPC),     INSR_PAIR(JAL),
    INSR_PAIR(JALR),      INSR_PAIR(BEQ),       INSR_PAIR(BNE),
    INSR_PAIR(BLT),       INSR_PAIR(BGE),       INSR_PAIR(BLTU),
    INSR_PAIR(BGEU),      INSR_PAIR(LB),        INSR_PAIR(LH),
    INSR_PAIR(LW),        INSR_PAIR(LD),        INSR_PAIR(LBU),
    INSR_PAIR(LHU),       INSR_PAIR(LWU),       INSR_PAIR(SB),
    INSR_PAIR(SH),        INSR_PAIR(SW),        INSR_PAIR(SD),
    INSR_PAIR(ADDI),      INSR_PAIR(SLTI),      INSR_PAIR(SLTIU),
    INSR_PAIR(XORI),      INSR_PAIR(ORI),       INSR_PAIR(ANDI),
    INSR_PAIR(SLLI),      INSR_PAIR(SRLI),      INSR_PAIR(SRAI),
    INSR_PAIR(ADDIW),     INSR_PAIR(SLLIW),     INSR_PAIR(SRLIW),
    INSR_PAIR(SRAIW),     INSR_PAIR(ADD),       INSR_PAIR(SUB),
    INSR_PAIR(SLL),       INSR_PAIR(SLT),       INSR_PAIR(SLTU),
    INSR_PAIR(XOR),       INSR_PAIR(SRL),       INSR_PAIR(SRA),
    INSR_PAIR(OR),        INSR_PAIR(AND),       INSR_PAIR(ADDW),
    INSR_PAIR(SUBW),      INSR_PAIR(SLLW),      INSR_PAIR(SRLW),
    INSR_PAIR(SRAW),      INSR_PAIR(MUL),       INSR_PAIR(MULH),
    INSR_PAIR(MULHSU),    INSR_PAIR(MULHU),     INSR_PAIR(DIV),
    INSR_PAIR(DIVU),      INSR_PAIR(REM),       INSR_PAIR(REMU),
    INSR_PAIR(MULW),      INSR_PAIR(DIVW),      INSR_PAIR(DIVUW),
    INSR_PAIR(REMW),      INSR_PAIR(REMUW),     INSR_PAIR(FLW),
    INSR_PAIR(FLD),       INSR_PAIR(FSW),       INSR_PAIR(FSD),
    INSR_PAIR(FMADD_S),   INSR_PAIR(FMSUB_S),   INSR_PAIR(FNMADD_S),
    INSR_PAIR(FNMSUB_S),  INSR_PAIR(FMADD_D),   INSR_PAIR(FMSUB_D),
    INSR_PAIR(FNMADD_D),  INSR_PAIR(FNMSUB_D),  INSR_PAIR(FADD_S),
    INSR_PAIR(FSUB_S),    INSR_PAIR(FMUL_S),    INSR_PAIR(FDIV_S),
    INSR_PAIR(FSQRT_S),   INSR_PAIR(FSGNJ_S),   INSR_PAIR(FSGNJN_S),
    INSR_PAIR(FSGNJX_S),  INSR_PAIR(FMIN_S),    INSR_PAIR(FMAX_S),
    INSR_PAIR(FADD_D),    INSR_PAIR(FSUB_D),    INSR_PAIR(FMUL_D),
    INSR_PAIR(FDIV_D),    INSR_PAIR(FSQRT_D),   INSR_PAIR(FSGNJ_D),
    INSR_PAIR(FSGNJN_D),  INSR_PAIR(FSGNJX_D),  INSR_PAIR(FMIN_D),
    INSR_PAIR(FMAX_D),    INSR_PAIR(FCVT_W_S),  INSR_PAIR(FCVT_WU_S),
    INSR_PAIR(FCVT_L_S),  INSR_PAIR(FCVT_LU_S), INSR_PAIR(FCVT_W_D),
    INSR_PAIR(FCVT_WU_D), INSR_PAIR(FCVT_L_D),  INSR_PAIR(FCVT_LU_D),
    INSR_PAIR(FCVT_S_W),  INSR_PAIR(FCVT_S_WU), INSR_PAIR(FCVT_S_L),
    INSR_PAIR(FCVT_S_LU), INSR_PAIR(FCVT_D_W),  INSR_PAIR(FCVT_D_WU),
    INSR_PAIR(FCVT_D_L),  INSR_PAIR(FCVT_D_LU), INSR_PAIR(FCVT_S_D),
    INSR_PAIR(FCVT_D_S),  INSR_PAIR(FMV_X_W),   INSR_PAIR(FMV_X_D),
    INSR_PAIR(FMV_W_X),   INSR_PAIR(FMV_D_X),   INSR_PAIR(FEQ_S),
    INSR_PAIR(FLT_S),     INSR_PAIR(FLE_S),     INSR_PAIR(FEQ_D),
    INSR_PAIR(FLT_D),     INSR_PAIR(FLE_D),     INSR_PAIR(FCLASS_S),
    INSR_PAIR(FCLASS_D),
#undef INSR_PAIR
    {PINSR_J, "J"},       {PINSR_CALL, "CALL"}, {PINSR_TAIL, "TAIL"},
    {PINSR_RET, "RET"},   {PINSR_LA, "LA"}};

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
    {INSR_FCLASS_S, R0_TYPE}, {INSR_FCLASS_D, R0_TYPE}};

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
  int64_t imm;
};

namespace rv64 {

constexpr size_t kRegisters = 32;
constexpr size_t kNumCalleeSaved = 13;
constexpr size_t kNumCallerSaved = 16;
constexpr size_t kNumSavedRegisters = 11;
constexpr size_t kNumTempRegisters = 6;

// RISC-V register ABI names
constexpr uint8_t kZero = 0;
constexpr uint8_t kRa = 1;
constexpr uint8_t kSp = 2;
constexpr uint8_t kGp = 3;
constexpr uint8_t kTp = 4;
constexpr uint8_t kT0 = 5;
constexpr uint8_t kT1 = 6;
constexpr uint8_t kT2 = 7;
constexpr uint8_t kFp = 8;
constexpr uint8_t kS0 = 8;
constexpr uint8_t kS1 = 9;
constexpr uint8_t kA0 = 10;
constexpr uint8_t kA1 = 11;
constexpr uint8_t kA2 = 12;
constexpr uint8_t kA3 = 13;
constexpr uint8_t kA4 = 14;
constexpr uint8_t kA5 = 15;
constexpr uint8_t kA6 = 16;
constexpr uint8_t kA7 = 17;
constexpr uint8_t kS2 = 18;
constexpr uint8_t kS3 = 19;
constexpr uint8_t kS4 = 20;
constexpr uint8_t kS5 = 21;
constexpr uint8_t kS6 = 22;
constexpr uint8_t kS7 = 23;
constexpr uint8_t kS8 = 24;
constexpr uint8_t kS9 = 25;
constexpr uint8_t kS10 = 26;
constexpr uint8_t kS11 = 27;
constexpr uint8_t kT3 = 28;
constexpr uint8_t kT4 = 29;
constexpr uint8_t kT5 = 30;
constexpr uint8_t kT6 = 31;

constexpr std::array<uint8_t, kNumCalleeSaved> kCalleeSaved = {
    2, 8, 9, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27};

constexpr std::array<uint8_t, kNumCallerSaved> kCallerSaved = {
    1, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 28, 29, 30, 31};

constexpr std::array<uint8_t, kNumSavedRegisters> kSavedRegisters = {
    kS1, kS2, kS3, kS4, kS5, kS6, kS7, kS8, kS9, kS10, kS11};

constexpr std::array<uint8_t, kNumTempRegisters> kTempRegisters = {
    kT1, kT2, kT3, kT4, kT5, kT6};

}  // namespace rv64

struct MemoryLocation {
  bool in_register;
  std::variant<uint8_t, int64_t> mem;
};

class InsrGen {
 public:
  void Flush();
  void GenerateAR(const std::vector<IRInsr>& ir);

 private:
  std::ofstream ofs_;
  std::vector<RV64Insr> insr_;
  std::array<size_t, rv64::kRegisters> regs_{};

  class Register {
   public:
    template <size_t N>
    uint8_t GetRegister(const std::array<uint8_t, N>& pool, size_t& replaced);
    uint8_t GetSavedRegister(size_t& replaced);
    uint8_t GetTempRegister(size_t& replaced);

    void SetRegister(uint8_t pos, size_t id);

   private:
    static constexpr size_t kEmpty = (size_t)-1;
    static constexpr size_t kReserved = (size_t)-2;
    std::array<size_t, rv64::kRegisters> regs_{};
    uint32_t empty_;
  } reg_;

  template <class... Args>
  void GenerateInsr(Opcode op, Args&&... args);

  void GenerateRTypeInsr(const IRInsr& insr, std::vector<MemoryLocation>& mem);

  size_t GeneratePrologue(const std::vector<IRInsr>& ir);
  void GenerateEpilogue(size_t offset);

  void PushCalleeRegisters();
  void PopCalleeRegisters();
  uint8_t GetSavedRegister(size_t id, std::vector<MemoryLocation>& mem);
  uint8_t GetTempRegister(size_t id, std::vector<MemoryLocation>& mem);
};

#endif  // INSTRUCTION_H_
