#ifndef INSTRUCTION_H_
#define INSTRUCTION_H_

#include <array>
#include <cassert>
#include <climits>
#include <fstream>
#include <iostream>
#include <queue>
#include <set>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <variant>
#include <vector>

#include "entry.h"
#include "utils.h"

namespace rv64 {

constexpr size_t kRegisters = 32;
constexpr size_t kNumCalleeSaved = 13;
constexpr size_t kNumCallerSaved = 15;
constexpr size_t kNumIntSavedRegs = 11;
constexpr size_t kNumIntTempRegs = 6;
constexpr size_t kNumFloatSavedRegs = 12;
constexpr size_t kNumFloatTempRegs = 11;
constexpr size_t kNumIntArgs = 8;
constexpr size_t kNumFloatArgs = 8;

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
// floating-point registers
constexpr uint8_t kFt0 = 128 | 0;
constexpr uint8_t kFt1 = 128 | 1;
constexpr uint8_t kFt2 = 128 | 2;
constexpr uint8_t kFt3 = 128 | 3;
constexpr uint8_t kFt4 = 128 | 4;
constexpr uint8_t kFt5 = 128 | 5;
constexpr uint8_t kFt6 = 128 | 6;
constexpr uint8_t kFt7 = 128 | 7;
constexpr uint8_t kFs0 = 128 | 8;
constexpr uint8_t kFs1 = 128 | 9;
constexpr uint8_t kFa0 = 128 | 10;
constexpr uint8_t kFa1 = 128 | 11;
constexpr uint8_t kFa2 = 128 | 12;
constexpr uint8_t kFa3 = 128 | 13;
constexpr uint8_t kFa4 = 128 | 14;
constexpr uint8_t kFa5 = 128 | 15;
constexpr uint8_t kFa6 = 128 | 16;
constexpr uint8_t kFa7 = 128 | 17;
constexpr uint8_t kFs2 = 128 | 18;
constexpr uint8_t kFs3 = 128 | 19;
constexpr uint8_t kFs4 = 128 | 20;
constexpr uint8_t kFs5 = 128 | 21;
constexpr uint8_t kFs6 = 128 | 22;
constexpr uint8_t kFs7 = 128 | 23;
constexpr uint8_t kFs8 = 128 | 24;
constexpr uint8_t kFs9 = 128 | 25;
constexpr uint8_t kFs10 = 128 | 26;
constexpr uint8_t kFs11 = 128 | 27;
constexpr uint8_t kFt8 = 128 | 28;
constexpr uint8_t kFt9 = 128 | 29;
constexpr uint8_t kFt10 = 128 | 30;
constexpr uint8_t kFt11 = 128 | 31;

constexpr uint8_t kRNE = 0;
constexpr uint8_t kRTZ = 1;
constexpr uint8_t kRDN = 2;
constexpr uint8_t kRUP = 3;
constexpr uint8_t kRMM = 4;

constexpr std::array<uint8_t, kNumCalleeSaved> kCalleeSaved = {
    1, 8, 9, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27};

constexpr std::array<uint8_t, kNumCallerSaved> kCallerSaved = {
    5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 28, 29, 30, 31};

constexpr std::array<uint8_t, kNumIntSavedRegs> kIntSavedRegs = {
    kS1, kS2, kS3, kS4, kS5, kS6, kS7, kS8, kS9, kS10, kS11};

constexpr std::array<uint8_t, kNumIntTempRegs> kIntTempRegs = {kT1, kT2, kT3,
                                                               kT4, kT5, kT6};

constexpr std::array<uint8_t, kNumFloatSavedRegs> kFloatSavedRegs = {
    kFs0, kFs1, kFs2, kFs3, kFs4, kFs5, kFs6, kFs7, kFs8, kFs9, kFs10, kFs11};

constexpr std::array<uint8_t, kNumFloatTempRegs> kFloatTempRegs = {
    kFt0, kFt1, kFt2, kFt3, kFt4, kFt5, kFt6, kFt7, kFt8, kFt9, kFt10};

constexpr std::array<uint8_t, kNumIntArgs> kIntArgs = {kA0, kA1, kA2, kA3,
                                                       kA4, kA5, kA6, kA7};
constexpr std::array<uint8_t, kNumFloatArgs> kFloatArgs = {
    kFa0, kFa1, kFa2, kFa3, kFa4, kFa5, kFa6, kFa7};

const std::string kIntRegisterName[] = {
    "zero", "ra", "sp", "gp", "tp",  "t0",  "t1", "t2", "fp", "s1", "a0",
    "a1",   "a2", "a3", "a4", "a5",  "a6",  "a7", "s2", "s3", "s4", "s5",
    "s6",   "s7", "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6"};

const std::string kFloatRegisterName[] = {
    "ft0", "ft1", "ft2",  "ft3",  "ft4", "ft5", "ft6",  "ft7",
    "fs0", "fs1", "fa0",  "fa1",  "fa2", "fa3", "fa4",  "fa5",
    "fa6", "fa7", "fs2",  "fs3",  "fs4", "fs5", "fs6",  "fs7",
    "fs8", "fs9", "fs10", "fs11", "ft8", "ft9", "ft10", "ft11"};

const std::string kRoundingModeName[] = {"rne", "rtz", "rdn", "rup", "rmm"};

template <class T>
constexpr bool IsSavedReg(uint8_t v) {
  if constexpr (std::is_same_v<T, int>) {
    return v == kS1 || v == kS2 || v == kS3 || v == kS4 || v == kS5 ||
           v == kS6 || v == kS7 || v == kS8 || v == kS9 || v == kS10 ||
           v == kS11;
  } else {
    return v == kFs0 || v == kFs1 || v == kFs2 || v == kFs3 || v == kFs4 ||
           v == kFs5 || v == kFs6 || v == kFs7 || v == kFs8 || v == kFs9 ||
           v == kFs10 || v == kFs11;
  }
}

}  // namespace rv64

enum Opcode {
  /*** Integer instructions ***/
  INSR_LUI,    // Load Upper Immediate
  INSR_AUIPC,  // Add Upper Immediate to PC
  INSR_JAL,    // Jump and Link
  INSR_JALR,   // Jump and Link Register
  // branch rs1, rs2, LABEL ->
  //     [negative branch] rs1, rs2, NEXT
  //     jal x0, LABEL
  //   NEXT: [next instruction]
  //   if LABEL is too far away to fit into immediate, or
  //     [negative branch] rs1, rs2, NEXT
  //     lui [tmp], %hi(LABEL)
  //     jalr x0, %lo(label)([tmp])
  //   NEXT: [next instruction]
  //   if LABEL is even far away to fit into jal's immediate
  //   (clang and gcc both appear to fail in this case...)
  //   ("negative branch" means taken and not taken reversed, e.g. beq <-> bne)
  INSR_BEQ,   // Branch Equal
  INSR_BNE,   // Branch Not Equal
  INSR_BLT,   // Branch Less Than
  INSR_BGE,   // Branch Greater than Equal
  INSR_BLTU,  // Branch Less Than Unsigned (not used)
  INSR_BGEU,  // Branch Greater than Equal Unsigned (not used)A
  // load/store r1, imm(r2) ->
  //     lui [tmp], [msb(imm)]
  //     add [tmp], [tmp], r2
  //     load/store r1, [lsb(imm)]([tmp])
  //   if imm is too large to fit into immediate
  // the base address can be omitted if imm_type == kData
  //   in this case, the instruction will be expanded to
  //     lui [tmp], %hi(data label)
  //     load/store r1, %lo(data label)([tmp])
  INSR_LB,   // Load Byte (not used)
  INSR_LH,   // Load Half (not used)
  INSR_LW,   // Load Word
  INSR_LD,   // Load Double
  INSR_LBU,  // Load Byte Unsigned (not used)
  INSR_LHU,  // Load Half Unsigned (not used)
  INSR_LWU,  // Load Word Unsigned (not used)
  INSR_SB,   // Store Byte
  INSR_SH,   // Store Half
  INSR_SW,   // Store Word
  INSR_SD,   // Store Double
  // arithI r1, r2, imm ->
  //     lui [tmp], [msb(imm)]
  //     addi [tmp], [tmp], [lsb(imm)]
  //     arith r1, r2, [tmp]
  //   if imm is too large to fit into immediate
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
  INSR_MULH,    // Multiply High Signed Signed (not used)
  INSR_MULHSU,  // Multiply High Signed Unsigned (not used)
  INSR_MULHU,   // Multiply High Unsigned Unsigned (not used)
  INSR_DIV,     // Divide Signed
  INSR_DIVU,    // Divide Unsigned (not used)
  INSR_REM,     // Remainder Signed (opt only)
  INSR_REMU,    // Remainder Unsigned (not used)
  INSR_MULW,    // Multiple Word
  INSR_DIVW,    // Divide Signed Word
  INSR_DIVUW,   // Divide Unsigned Word
  INSR_REMW,    // Remainder Signed Word
  INSR_REMUW,   // Remainder Unsigned Word
  kIntegerInsr,

  /*** Floating point instructions ***/
  // refer to descriptions of INSR_LW
  INSR_FLW,       // load
  INSR_FLD,       // (not used)
  INSR_FSW,       // store
  INSR_FSD,       // (not used)
  INSR_FMADD_S,   // fused mul-add (opt only)
  INSR_FMSUB_S,   // (opt only)
  INSR_FNMADD_S,  // (opt only)
  INSR_FNMSUB_S,  // (opt only)
  INSR_FMADD_D,   // (not used)
  INSR_FMSUB_D,   // (not used)
  INSR_FNMADD_D,  // (not used)
  INSR_FNMSUB_D,  // (not used)
  INSR_FADD_S,    // arith
  INSR_FSUB_S,
  INSR_FMUL_S,
  INSR_FDIV_S,
  INSR_FSQRT_S,  // (not used)
  INSR_FSGNJ_S,  // (opt only)
  INSR_FSGNJN_S,
  INSR_FSGNJX_S,   // (opt only)
  INSR_FMIN_S,     // (opt only)
  INSR_FMAX_S,     // (opt only)
  INSR_FADD_D,     // (not used)
  INSR_FSUB_D,     // (not used)
  INSR_FMUL_D,     // (not used)
  INSR_FDIV_D,     // (not used)
  INSR_FSQRT_D,    // (not used)
  INSR_FSGNJ_D,    // (not used)
  INSR_FSGNJN_D,   // (not used)
  INSR_FSGNJX_D,   // (not used)
  INSR_FMIN_D,     // (not used)
  INSR_FMAX_D,     // (not used)
  INSR_FCVT_W_S,   // float to int
  INSR_FCVT_WU_S,  // (not used)
  INSR_FCVT_L_S,
  INSR_FCVT_LU_S,  // (not used)
  INSR_FCVT_W_D,   // (not used)
  INSR_FCVT_WU_D,  // (not used)
  INSR_FCVT_L_D,   // (not used)
  INSR_FCVT_LU_D,  // (not used)
  INSR_FCVT_S_W,   // int to float
  INSR_FCVT_S_WU,  // (not used)
  INSR_FCVT_S_L,
  INSR_FCVT_S_LU,  // (not used)
  INSR_FCVT_D_W,   // (not used)
  INSR_FCVT_D_WU,  // (not used)
  INSR_FCVT_D_L,   // (not used)
  INSR_FCVT_D_LU,  // (not used)
  INSR_FCVT_S_D,   // (not used)
  INSR_FCVT_D_S,   // (not used)
  INSR_FMV_X_W,    // bitwise float to int (not used)
  INSR_FMV_X_D,    // (not used)
  INSR_FMV_W_X,    // bitwise int to float
  INSR_FMV_D_X,    // (not used)
  INSR_FEQ_S,      // compare
  INSR_FLT_S,
  INSR_FLE_S,
  INSR_FEQ_D,     // (not used)
  INSR_FLT_D,     // (not used)
  INSR_FLE_D,     // (not used)
  INSR_FCLASS_S,  // (not used)
  INSR_FCLASS_D,  // (not used)
  kFloatingPointInsr,

  /*** Pseudo-instructions ***/
  // j LABEL ->
  //     lui [tmp], %hi(LABEL)
  //     jalr x0, %lo(LABEL)([tmp])
  // if LABEL is too far away to fit into immediate
  PINSR_J,
  // call LABEL, the assembler will handle immediate
  PINSR_CALL,
  PINSR_TAIL,     // Tail call function (+dest ID) (opt only)
  PINSR_RET,      // Return (no arg)
  PINSR_LA,       // Load absolute address
  PINSR_MV,       // Copy (can be optimized!)
  PINSR_FMV_S,    // Floating-point copy (can be optimized!)
  PINSR_PUSH_SP,  // push stack pointer here
  PINSR_LI,       // Load immediate
  kPseudoInsr
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
      ret.str[i] = s.str[i] ^ 32;
  }
  return ret;
}

struct RV64InsrCodeTable {
  std::string arr[kPseudoInsr];
  RV64InsrCodeTable() {
    for (std::pair<Opcode, std::string> i : {
#define INSR_PAIR(x) \
  (std::pair<Opcode, std::string>){INSR_##x, ToInsr(CString(#x))}
             INSR_PAIR(LUI),         INSR_PAIR(AUIPC),
             INSR_PAIR(JAL),         INSR_PAIR(JALR),
             INSR_PAIR(BEQ),         INSR_PAIR(BNE),
             INSR_PAIR(BLT),         INSR_PAIR(BGE),
             INSR_PAIR(BLTU),        INSR_PAIR(BGEU),
             INSR_PAIR(LB),          INSR_PAIR(LH),
             INSR_PAIR(LW),          INSR_PAIR(LD),
             INSR_PAIR(LBU),         INSR_PAIR(LHU),
             INSR_PAIR(LWU),         INSR_PAIR(SB),
             INSR_PAIR(SH),          INSR_PAIR(SW),
             INSR_PAIR(SD),          INSR_PAIR(ADDI),
             INSR_PAIR(SLTI),        INSR_PAIR(SLTIU),
             INSR_PAIR(XORI),        INSR_PAIR(ORI),
             INSR_PAIR(ANDI),        INSR_PAIR(SLLI),
             INSR_PAIR(SRLI),        INSR_PAIR(SRAI),
             INSR_PAIR(ADDIW),       INSR_PAIR(SLLIW),
             INSR_PAIR(SRLIW),       INSR_PAIR(SRAIW),
             INSR_PAIR(ADD),         INSR_PAIR(SUB),
             INSR_PAIR(SLL),         INSR_PAIR(SLT),
             INSR_PAIR(SLTU),        INSR_PAIR(XOR),
             INSR_PAIR(SRL),         INSR_PAIR(SRA),
             INSR_PAIR(OR),          INSR_PAIR(AND),
             INSR_PAIR(ADDW),        INSR_PAIR(SUBW),
             INSR_PAIR(SLLW),        INSR_PAIR(SRLW),
             INSR_PAIR(SRAW),        INSR_PAIR(MUL),
             INSR_PAIR(MULH),        INSR_PAIR(MULHSU),
             INSR_PAIR(MULHU),       INSR_PAIR(DIV),
             INSR_PAIR(DIVU),        INSR_PAIR(REM),
             INSR_PAIR(REMU),        INSR_PAIR(MULW),
             INSR_PAIR(DIVW),        INSR_PAIR(DIVUW),
             INSR_PAIR(REMW),        INSR_PAIR(REMUW),
             INSR_PAIR(FLW),         INSR_PAIR(FLD),
             INSR_PAIR(FSW),         INSR_PAIR(FSD),
             INSR_PAIR(FMADD_S),     INSR_PAIR(FMSUB_S),
             INSR_PAIR(FNMADD_S),    INSR_PAIR(FNMSUB_S),
             INSR_PAIR(FMADD_D),     INSR_PAIR(FMSUB_D),
             INSR_PAIR(FNMADD_D),    INSR_PAIR(FNMSUB_D),
             INSR_PAIR(FADD_S),      INSR_PAIR(FSUB_S),
             INSR_PAIR(FMUL_S),      INSR_PAIR(FDIV_S),
             INSR_PAIR(FSQRT_S),     INSR_PAIR(FSGNJ_S),
             INSR_PAIR(FSGNJN_S),    INSR_PAIR(FSGNJX_S),
             INSR_PAIR(FMIN_S),      INSR_PAIR(FMAX_S),
             INSR_PAIR(FADD_D),      INSR_PAIR(FSUB_D),
             INSR_PAIR(FMUL_D),      INSR_PAIR(FDIV_D),
             INSR_PAIR(FSQRT_D),     INSR_PAIR(FSGNJ_D),
             INSR_PAIR(FSGNJN_D),    INSR_PAIR(FSGNJX_D),
             INSR_PAIR(FMIN_D),      INSR_PAIR(FMAX_D),
             INSR_PAIR(FCVT_W_S),    INSR_PAIR(FCVT_WU_S),
             INSR_PAIR(FCVT_L_S),    INSR_PAIR(FCVT_LU_S),
             INSR_PAIR(FCVT_W_D),    INSR_PAIR(FCVT_WU_D),
             INSR_PAIR(FCVT_L_D),    INSR_PAIR(FCVT_LU_D),
             INSR_PAIR(FCVT_S_W),    INSR_PAIR(FCVT_S_WU),
             INSR_PAIR(FCVT_S_L),    INSR_PAIR(FCVT_S_LU),
             INSR_PAIR(FCVT_D_W),    INSR_PAIR(FCVT_D_WU),
             INSR_PAIR(FCVT_D_L),    INSR_PAIR(FCVT_D_LU),
             INSR_PAIR(FCVT_S_D),    INSR_PAIR(FCVT_D_S),
             INSR_PAIR(FMV_X_W),     INSR_PAIR(FMV_X_D),
             INSR_PAIR(FMV_W_X),     INSR_PAIR(FMV_D_X),
             INSR_PAIR(FEQ_S),       INSR_PAIR(FLT_S),
             INSR_PAIR(FLE_S),       INSR_PAIR(FEQ_D),
             INSR_PAIR(FLT_D),       INSR_PAIR(FLE_D),
             INSR_PAIR(FCLASS_S),    INSR_PAIR(FCLASS_D),
#undef INSR_PAIR
             {PINSR_J, "j"},         {PINSR_CALL, "call"},
             {PINSR_TAIL, "tail"},   {PINSR_RET, "ret"},
             {PINSR_LA, "la"},       {PINSR_MV, "mv"},
             {PINSR_FMV_S, "fmv.s"}, {PINSR_PUSH_SP, "_push_sp_"},
             {PINSR_LI, "li"}}) {
      arr[i.first] = i.second;
    }
  }
  const std::string& operator[](Opcode i) const { return arr[i]; }
};
const RV64InsrCodeTable kRV64InsrCode;

struct RV64InsrFormatTable {
  InsrFormat arr[kPseudoInsr];
  constexpr RV64InsrFormatTable() : arr{} {
    constexpr std::pair<Opcode, InsrFormat> table[] = {
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
    for (auto i : table) {
      arr[i.first] = i.second;
    }
  }
  InsrFormat operator[](Opcode i) const { return arr[i]; }
};
const RV64InsrFormatTable kRV64InsrFormat;

const std::string kBuiltinLabel[] = {"_write_str", "_write_int", "_write_float",
                                     "_read_int", "_read_float"};

struct IRInsr {
  struct NoRD {};
  static const NoRD kNoRD;
  struct Register {
    size_t id;
    bool is_real;
    Register() : id(), is_real() {}
    Register(NoRD) : id(), is_real() {}
    Register(size_t x, bool is_real = false) : id(x), is_real(is_real) {}
  };
  enum ImmType {
    kConst,  // a constant
    kLabel,  // a label ID referring to a IR label array position
    // negative label for builtin functions
    kData,         // a data ID referring to a CodeData array position
    kRoundingMode  // rounding mode (refer to rv64::kRoundingMode)
  };
  IRInsr() {}
  IRInsr(Opcode op) : op(op), rd(), rs1(), rs2(), imm_type(), imm() {}
  template <class RD, class RS1>
  IRInsr(Opcode op, RD rd, RS1 rs1)
      : op(op), rd(rd), rs1(rs1), rs2(), imm_type(), imm() {}
  template <class RD, class RS1, class RS2>
  IRInsr(Opcode op, RD rd, RS1 rs1, RS2 rs2)
      : op(op), rd(rd), rs1(rs1), rs2(rs2), imm_type(), imm() {}
  template <class Int>
  IRInsr(Opcode op, ImmType imm_type, Int imm)
      : op(op), rd(), rs1(), rs2(), imm_type(imm_type), imm(imm) {}
  template <class RD, class Int>
  IRInsr(Opcode op, RD rd, ImmType imm_type, Int imm)
      : op(op), rd(rd), rs1(), rs2(), imm_type(imm_type), imm(imm) {}
  template <class RD, class RS1, class Int>
  IRInsr(Opcode op, RD rd, RS1 rs1, ImmType imm_type, Int imm)
      : op(op), rd(rd), rs1(rs1), imm_type(imm_type), imm(imm) {}
  template <class RD, class RS1, class RS2, class Int>
  IRInsr(Opcode op, RD rd, RS1 rs1, RS2 rs2, ImmType imm_type, Int imm)
      : op(op), rd(rd), rs1(rs1), rs2(rs2), imm_type(imm_type), imm(imm) {}
  Opcode op;
  Register rd, rs1, rs2 /*, rs3*/;
  ImmType imm_type;
  int64_t imm;
};

struct RV64Insr {
  Opcode op;
  uint8_t rs1, rs2, rs3, rd;
  IRInsr::ImmType imm_type;
  int64_t imm;

  RV64Insr() = default;
  explicit RV64Insr(Opcode op, uint8_t rs1, uint8_t rs2, uint8_t rs3,
                    uint8_t rd, IRInsr::ImmType imm_type, int64_t imm)
      : op(op),
        rs1(rs1),
        rs2(rs2),
        rs3(rs3),
        rd(rd),
        imm_type(imm_type),
        imm(imm) {}
};

struct RegLocation {
  bool callee_saved;
  uint8_t loc;

  RegLocation() = default;
  RegLocation(bool c, uint8_t l) : callee_saved(c), loc(l) {}
};

struct MemoryLocation {
  static constexpr int64_t kUnAllocated = LLONG_MAX;
  bool in_register;
  std::variant<int64_t, RegLocation> addr;
  MemoryLocation() : in_register(false), addr(kUnAllocated) {}
};

struct Label {
  size_t ir_pos;
  bool is_func;
  Label() {}
  Label(size_t ir_pos, bool is_func = false)
      : ir_pos(ir_pos), is_func(is_func) {}
};

// num (or initialized array, not used in this project),
//   string constant or uninitialized array

struct CodeGenOptions {
  bool register_alloc;
};

struct FuncRegInfo {
  std::vector<uint8_t> int_caller, float_caller;
};

struct FreqInfo {
  std::vector<size_t> int_freq, float_freq;

  FreqInfo() = default;
  FreqInfo(size_t ireg, size_t freg) : int_freq(ireg), float_freq(freg) {}
};

using CodeData = std::variant<std::vector<uint8_t>, std::string, size_t>;

using CodeGenInfo = std::tuple<std::vector<IRInsr>&, std::vector<Label>&,
                               std::vector<CodeData>&, std::vector<TableEntry>&,
                               std::vector<Identifier>&, CodeGenOptions&,
                               std::vector<FuncRegInfo>&>;

/**
 * RV64 Instruction Generator
 *
 * The `InsrGen` object helps translating the IR instructions (instructions in
 * which "pseudo" registers are used) into real RV64 instructions. Things like
 * allocating stack frames, saving registers and allocating registers for
 * temporaries and intermediates (and probabily some optimizations) are handled
 * in this class.
 */
class InsrGen {
 public:
  explicit InsrGen() = default;
  explicit InsrGen(const std::string& file);
  explicit InsrGen(const std::string& file, std::vector<CodeData>&& data,
                   std::vector<Label>&& label, std::vector<TableEntry>&& tab,
                   std::vector<Identifier>&& func);
  explicit InsrGen(const std::string& file, CodeGenInfo&& code_gen);

  // The instructions will be flushed upon destruction.
  ~InsrGen() {}

  // Flush the instructions in the buffer to the output file.
  void Flush();

  /**
   * Generate activation record for a function (`.text` section in the RV64
   * assembly).
   *
   * @param local The number of bytes allocated for the local variables.
   * @param num_register The number of (pseudo) registers used in this function.
   *
   * The local variables should be referenced in IR instructions using `sp +
   * offset` while the temporaries and intermediates will be referenced in the
   * RV64 instructions using `fp - offset`. Both `sp` and `fp` should not be
   * changed in the IR instructions.
   *
   * The load/store of the pseudo registers will be handled in this
   * function. The caller should only cares about the load/store of the
   * local variables, parameter passsing, etc.
   *
   * The callee saved registers are stored on the stack starting from position
   * `sp + local`.
   *
   * Use `RET` instruction to return from the function (the prologue will be
   * generated upon seeing the `RET` instruction).
   *
   * Use `CALL` instruction to do procedure call (the caller saved
   * registers will be saved upon seeing the `CALL` instruction. This is
   * optional since the caller saved registers are only used for optimization).
   */
  void GenerateAR(size_t local, size_t ireg, size_t freg, size_t next_func,
                  bool is_main, const FuncRegInfo& info);

  /**
   * Generate a list of global variables (`.data` section in the RV64 assembly).
   */
  void GenerateData(const std::vector<CodeData>& data);
  void GenerateData(std::vector<CodeData>&& data);

  void GenerateRV64();

 private:
  std::ofstream ofs_;
  std::vector<RV64Insr> buf_;
  std::vector<std::variant<RV64Insr, size_t>> insr_;  // instruction buffer
  std::vector<CodeData> data_;
  std::vector<Label> label_;
  std::vector<Identifier> func_;
  std::vector<IRInsr> ir_insr_;
  std::vector<TableEntry> tab_;
  CodeGenOptions opt_;
  std::vector<FuncRegInfo> func_reg_info_;
  std::vector<std::string> label_func_, func_name_;
  size_t ir_pos_ = 0, label_pos_ = 0, tot_label_, num_branch_expand_ = 0;
  uint8_t int_tmp_ = 0, float_tmp_ = 0;
  std::array<bool, rv64::kRegisters> int_used_{};
  std::array<bool, rv64::kRegisters> float_used_{};
  std::unordered_map<std::string, size_t> str_cache_;
  std::unordered_map<size_t, std::pair<uint8_t, bool>> int_query_, float_query_;
  std::vector<MemoryLocation> int_loc_, float_loc_;
  int64_t sp_offset_;

  void InitRegs(size_t ireg, size_t freg, const FuncRegInfo& info,
                const FreqInfo& freq);
  void RegAlloc(size_t ireg, size_t freg, const FuncRegInfo& info,
                const FreqInfo& freq);
  FreqInfo CountFreq(size_t ireg, size_t freg, size_t ed) const;

  void PushInsr(const RV64Insr& v);
  template <class... Args>
  void PushInsr(Opcode op, Args&&... args);
  template <class... Args>
  void PushPInsr(Opcode op, Args&&... args);

  template <class T>
  std::vector<RV64Insr> ExpandImm(const T& v, uint8_t rd, uint8_t rs1) const;

  void GenerateInsrImpl(const IRInsr& v);
  void GenerateRTypeInsr(const IRInsr& ir);
  void GenerateITypeInsr(const IRInsr& ir);
  void GenerateSTypeInsr(const IRInsr& ir);
  void GenerateUTypeInsr(const IRInsr& ir);
  void GenerateBTypeInsr(const IRInsr& ir);
  void GenerateJTypeInsr(const IRInsr& ir);
  void GenerateR0TypeInsr(const IRInsr& ir);
  void GenerateR4TypeInsr(const IRInsr& ir);
  void GeneratePseudoInsr(const IRInsr& ir, int64_t offset);

  void GeneratePrologue(size_t local, const std::vector<uint8_t>& saved);
  void GenerateEpilogue(size_t local, const std::vector<uint8_t>& saved);

  void PushCalleeRegs(int64_t offset, const std::vector<uint8_t>& saved);
  void PushCallerRegs(int64_t offset, const std::vector<uint8_t>& caller_saved);
  void PopCalleeRegs(int64_t offset, const std::vector<uint8_t>& saved);
  void PopCallerRegs(int64_t offset, const std::vector<uint8_t>& caller_saved);

  void InitLabel();
  inline std::string GetLabel(const RV64Insr& insr) const;
  inline std::string GetLabel(int64_t imm) const;
  inline std::string GetData(const RV64Insr& insr) const;
  inline std::string GetData(int64_t imm) const;
  void PrintInsr(std::ofstream& ofs, const RV64Insr& insr, size_t p,
                 const std::vector<size_t>& pos);
  void PrintPseudoInsr(std::ofstream& ofs, const RV64Insr& insr, size_t p,
                       const std::vector<size_t>& pos) const;
  void PrintBranchInsr(std::ofstream& ofs, const RV64Insr& insr, size_t p,
                       const std::vector<size_t>& pos);
  void PrintNegBranch(std::ofstream& ofs, const RV64Insr& insr) const;
  void PrintJal(std::ofstream& ofs, size_t p, const std::vector<size_t>& pos,
                int64_t imm) const;

  template <class T>
  uint8_t GetRealReg(const IRInsr::Register& reg, bool load);
  template <class T>
  void PushStack(IRInsr::Register reg, uint8_t rg);
  void ReleaseRegs();

  static constexpr int64_t kPosSpOffset = LLONG_MAX;
  static constexpr int64_t kNegSpOffset = LLONG_MIN;
};

#endif  // INSTRUCTION_H_
