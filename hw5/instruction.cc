#include "instruction.h"

#include <algorithm>
#include <cassert>
#include <iostream>

const IRInsr::NoRD IRInsr::kNoRD;

namespace {

constexpr bool IsLoadOp(Opcode op) {
  return op == INSR_LB || op == INSR_LH || op == INSR_LW || op == INSR_LD ||
         op == INSR_LBU || op == INSR_LHU || op == INSR_LWU || op == INSR_LHU ||
         op == INSR_LWU || op == INSR_FLW || op == INSR_FLD;
}

constexpr bool IsIntOp(Opcode op) { return op < kIntegerInsr; }

constexpr bool IsFloatOp(Opcode op) {
  return op >= kIntegerInsr && op < kFloatingPointInsr;
}

constexpr bool IsPseudoOp(Opcode op) { return op >= kFloatingPointInsr; }

constexpr bool IsCvtOp(Opcode op) {
  return op == INSR_FCVT_W_S || op == INSR_FCVT_WU_S || op == INSR_FCVT_L_S ||
         op == INSR_FCVT_LU_S || op == INSR_FCVT_W_D || op == INSR_FCVT_WU_D ||
         op == INSR_FCVT_L_D || op == INSR_FCVT_LU_D || op == INSR_FCVT_S_W ||
         op == INSR_FCVT_S_WU || op == INSR_FCVT_S_L || op == INSR_FCVT_S_LU ||
         op == INSR_FCVT_D_W || op == INSR_FCVT_D_WU || op == INSR_FCVT_D_L ||
         op == INSR_FCVT_D_LU || op == INSR_FCVT_S_D || op == INSR_FCVT_D_S;
}

constexpr bool IsCvtIFOp(Opcode op) {
  return op == INSR_FCVT_W_S || op == INSR_FCVT_WU_S || op == INSR_FCVT_L_S ||
         op == INSR_FCVT_LU_S || op == INSR_FCVT_W_D || op == INSR_FCVT_WU_D ||
         op == INSR_FCVT_L_D || op == INSR_FCVT_LU_D;
}

constexpr bool IsCvtFIOp(Opcode op) {
  return op == INSR_FCVT_S_W || op == INSR_FCVT_S_WU || op == INSR_FCVT_S_L ||
         op == INSR_FCVT_S_LU || op == INSR_FCVT_D_W || op == INSR_FCVT_D_WU ||
         op == INSR_FCVT_D_L || op == INSR_FCVT_D_LU;
}

constexpr bool IsCvtFFOp(Opcode op) {
  return op == INSR_FCVT_S_D || op == INSR_FCVT_D_S;
}

#define RD(v)                               \
  (v.rd < 32 ? rv64::kIntRegisterName[v.rd] \
             : rv64::kFloatRegisterName[v.rd - 128])
#define RS1(v)                                \
  (v.rs1 < 32 ? rv64::kIntRegisterName[v.rs1] \
              : rv64::kFloatRegisterName[v.rs1 - 128])
#define RS2(v)                                \
  (v.rs2 < 32 ? rv64::kIntRegisterName[v.rs2] \
              : rv64::kFloatRegisterName[v.rs2 - 128])
#define RS3(v)                                \
  (v.rs3 < 32 ? rv64::kIntRegisterName[v.rs3] \
              : rv64::kFloatRegisterName[v.rs3 - 128])

#define LABEL(v) \
  (v.imm < 0 ? kBuiltinLabel[~v.imm] : ".L" + std::to_string(v.imm))
#define IMM(v) (v.imm)
#define DATA(v) (".D" + std::to_string(v.imm))
#define ROUND(v) (rv64::kRoundingModeName[v.imm])

size_t PrintPseudoInsr(std::ofstream &ofs, const RV64Insr &insr, size_t p,
                       const std::vector<size_t> &pos,
                       const std::vector<size_t> &pref) {
  static constexpr size_t kJumpThreshold = 1 << 17;
  if (insr.op != PINSR_J) ofs << kRV64InsrCode.at(insr.op) << ' ';
  switch (insr.op) {
    case PINSR_J: {
      if (insr.imm_type == IRInsr::kConst) {
        ofs << kRV64InsrCode.at(insr.op) << ' ' << IMM(insr);
        return 0;
      }
      assert(insr.imm_type == IRInsr::kLabel);
      int64_t dist = int64_t(p) - insr.imm;
      if ((size_t)insr.imm < p) dist -= pref[p] - pref[insr.imm];
      if (((size_t)std::abs(dist) << 2) > kJumpThreshold) {
        // TODO: Check whether t0 is dirty.
        ofs << "lui t0, \%hi(" << LABEL(insr) << ")\n";
        ofs << "jalr x0, \%lo(" << LABEL(insr) << ")(t0)\n";
        return 1;
      } else {
        ofs << kRV64InsrCode.at(insr.op) << ' ' << LABEL(insr);
        return 0;
      }
    }
    case PINSR_CALL:
    case PINSR_TAIL:
      if (insr.imm_type == IRInsr::kConst) {
        ofs << IMM(insr);
      } else {
        assert(insr.imm_type == IRInsr::kLabel);
        ofs << LABEL(insr);
      }
      break;
    case PINSR_LA:
      assert(insr.imm_type == IRInsr::kData);
      ofs << RD(insr) << ", " << DATA(insr);
      break;
    case PINSR_MV:
      ofs << RD(insr) << ", " << RS1(insr);
      break;
  }
  return 0;
}

// Serialize RV64 instructions
size_t PrintInsr(std::ofstream &ofs, const RV64Insr &insr, size_t p,
                 const std::vector<size_t> &pos, std::vector<size_t> &pref) {
  ofs << "\t";
  if (insr.op >= kFloatingPointInsr) {
    return PrintPseudoInsr(ofs, insr, p, pos, pref);
  }
  ofs << kRV64InsrCode.at(insr.op) << ' ';
  switch (kRV64InsrFormat.at(insr.op)) {
    case R_TYPE:
      ofs << RD(insr) << ", " << RS1(insr) << ", " << RS2(insr);
      break;
    case I_TYPE:
      if (insr.op == INSR_JALR || IsLoadOp(insr.op)) {
        if (insr.imm_type == IRInsr::kConst) {
          ofs << RD(insr) << ", " << IMM(insr) << "(" << RS1(insr) << ")";
        } else {
          assert(insr.imm_type == IRInsr::kData);
          ofs << RD(insr) << ", " << DATA(insr) << "(" << RS1(insr) << ")";
        }
      } else {
        assert(insr.imm_type == IRInsr::kConst);
        ofs << RD(insr) << ", " << RS1(insr) << ", " << IMM(insr);
      }
      break;
    case U_TYPE:
      ofs << RD(insr) << ", " << IMM(insr);
      break;
    case S_TYPE:
      if (insr.imm_type == IRInsr::kConst) {
        ofs << RS2(insr) << ", " << IMM(insr) << "(" << RS1(insr) << ")";
      } else {
        assert(insr.imm_type == IRInsr::kData);
        ofs << RS2(insr) << ", " << DATA(insr) << "(" << RS1(insr) << ")";
      }
      break;
    case B_TYPE:
      if (insr.imm_type == IRInsr::kConst) {
        ofs << RS1(insr) << ", " << RS2(insr) << ", " << IMM(insr);
      } else {
        assert(insr.imm_type == IRInsr::kLabel);
        ofs << RS1(insr) << ", " << RS2(insr) << ", " << LABEL(insr);
      }
      break;
    case J_TYPE:
      if (insr.imm_type == IRInsr::kConst) {
        ofs << RD(insr) << ", " << IMM(insr);
      } else {
        assert(insr.imm_type == IRInsr::kLabel);
        ofs << RD(insr) << ", " << LABEL(insr);
      }
      break;
    case R0_TYPE: {
      if (insr.imm_type == IRInsr::kRoundingMode) {
        ofs << RD(insr) << ", " << RS1(insr) << ", " << ROUND(insr);
      } else {
        ofs << RD(insr) << ", " << RS1(insr);
      }
      break;
    }
    case R4_TYPE:
      ofs << RD(insr) << ", " << RS1(insr) << ", " << RS2(insr) << ", "
          << RS3(insr);
      break;
  }
  return 0;
}

std::string EscapeString(const std::string &str) {
  std::string ret;
  for (char i : str) {
    if (i == '\"' || i == '\\' || i == '\'') {
      ret.push_back('\\');
      ret.push_back(i);
    } else if (i >= 0x20 && i <= 0x7e) {
      ret.push_back(i);
    } else if (i == '\n') {
      ret.push_back('\\');
      ret.push_back('n');
    } else if (i == '\r') {
      ret.push_back('\\');
      ret.push_back('r');
    } else if (i == '\t') {
      ret.push_back('\\');
      ret.push_back('t');
    } else {
      ret.push_back('\\');
      ret.push_back((i >> 6 & 3) + '0');
      ret.push_back((i >> 3 & 7) + '0');
      ret.push_back((i & 7) + '0');
    }
  }
  return ret;
}

}  // namespace

InsrGen::InsrGen(const std::string &file) : ofs_(file) {}

InsrGen::InsrGen(const std::string &file, std::vector<CodeData> &&data,
                 std::vector<Label> &&label, std::vector<TableEntry> &&tab,
                 std::vector<Identifier> &&func)
    : ofs_(file),
      data_(std::move(data)),
      label_(std::move(label)),
      func_(std::move(func)),
      tab_(std::move(tab)),
      tot_label_(label_.size()) {}

InsrGen::InsrGen(const std::string &file, CodeGenInfo &&code_gen)
    : ofs_(file),
      data_(std::move(std::get<2>(code_gen))),
      label_(std::move(std::get<1>(code_gen))),
      func_(std::move(std::get<4>(code_gen))),
      ir_insr_(std::move(std::get<0>(code_gen))),
      tab_(std::move(std::get<3>(code_gen))),
      tot_label_(label_.size()) {}

template <class T>
uint8_t InsrGen::GetSavedReg(const IRInsr::Register &reg, bool load,
                             std::vector<MemoryLocation> &loc,
                             std::vector<uint8_t> &dirty, RegCtrl<T> &ctrl) {
  if (reg.is_real) {
    // TODO: Check the current usage of the specified register, and store it
    // back to memory if neccesary. For now the reserved register will not be
    // used, so leave this to optimzation.
    return reg.id;
  }
  size_t id = reg.id;
  if (loc[id].in_register) return std::get<uint8_t>(loc[id].mem);
  size_t to_replace = (size_t)-1;
  uint8_t rg = ctrl.GetSavedReg(to_replace, dirty);
  if (to_replace != (size_t)-1) {
    // store the register back to memory if the register is dirty
    loc[to_replace].in_register = false;
    if (dirty[to_replace]) {
      loc[to_replace].mem = -int64_t(to_replace) * 8;  // this is useless now
      GenerateInsr(INSR_SD, rg, rv64::kFp, -int64_t(to_replace) * 8);
    }
    dirty[to_replace] = false;
  }
  loc[id].in_register = true;
  loc[id].mem = rg;
  ctrl.SetPseudoReg(rg, id);
  if (load) {
    // load the pseudo register from memory
    GenerateInsr(INSR_LD, rg, rv64::kFp, IRInsr::kConst, -int64_t(id) * 8);
  }
  return rg;
}

template <class... Args>
void InsrGen::GeneratePInsr(Opcode op, Args &&... args) {
  std::vector<int64_t> param{args...};
  switch (op) {
    case PINSR_J:
    case PINSR_CALL:
    case PINSR_TAIL: {
      IRInsr::ImmType imm_type = IRInsr::ImmType(param[0]);
      buf_.emplace_back(op, 0, 0, 0, 0, imm_type, param[1]);
      break;
    }
    case PINSR_LA: {
      uint8_t rd = static_cast<uint8_t>(param[0]);
      IRInsr::ImmType imm_type = IRInsr::ImmType(param[1]);
      buf_.emplace_back(op, 0, 0, 0, rd, imm_type, param[2]);
      break;
    }
    case PINSR_MV: {
      uint8_t rd = static_cast<uint8_t>(param[0]);
      uint8_t rs = static_cast<uint8_t>(param[1]);
      buf_.emplace_back(op, rs, 0, 0, rd, IRInsr::kConst, 0);
      break;
    }
  }
}

template <class... Args>
void InsrGen::GenerateInsr(Opcode op, Args &&... args) {
  if (IsPseudoOp(op)) {
    GeneratePInsr(op, std::forward<Args>(args)...);
    return;
  }
  std::vector<int64_t> param{args...};
  switch (kRV64InsrFormat.at(op)) {
    case R_TYPE: {
      uint8_t rd = static_cast<uint8_t>(param[0]);
      uint8_t rs1 = static_cast<uint8_t>(param[1]);
      uint8_t rs2 = static_cast<uint8_t>(param[2]);
      buf_.emplace_back(op, rs1, rs2, 0, rd, IRInsr::kConst, 0);
      break;
    }
    case I_TYPE: {
      uint8_t rd = static_cast<uint8_t>(param[0]);
      uint8_t rs1 = static_cast<uint8_t>(param[1]);
      IRInsr::ImmType imm_type = IRInsr::ImmType(param[2]);
      int64_t imm = param[3];
      buf_.emplace_back(op, rs1, 0, 0, rd, imm_type, imm);
      break;
    }
    case U_TYPE: {
      uint8_t rd = static_cast<uint8_t>(param[0]);
      IRInsr::ImmType imm_type = IRInsr::ImmType(param[1]);
      int64_t imm = param[2];
      buf_.emplace_back(op, 0, 0, 0, rd, imm_type, imm);
      break;
    }
    case S_TYPE: {
      uint8_t rs2 = static_cast<uint8_t>(param[0]);
      uint8_t rs1 = static_cast<uint8_t>(param[1]);
      IRInsr::ImmType imm_type = IRInsr::ImmType(param[2]);
      int64_t imm = param[3];
      buf_.emplace_back(op, rs1, rs2, 0, 0, imm_type, imm);
      break;
    }
    case B_TYPE: {
      uint8_t rs1 = static_cast<uint8_t>(param[0]);
      uint8_t rs2 = static_cast<uint8_t>(param[1]);
      IRInsr::ImmType imm_type = IRInsr::ImmType(param[2]);
      int64_t imm = param[3];
      buf_.emplace_back(op, rs1, rs2, 0, 0, imm_type, imm);
      break;
    }
    case J_TYPE: {
      uint8_t rd = static_cast<uint8_t>(param[0]);
      IRInsr::ImmType imm_type = IRInsr::ImmType(param[1]);
      int64_t imm = param[2];
      buf_.emplace_back(op, 0, 0, 0, rd, imm_type, imm);
      break;
    }
    case R0_TYPE: {
      uint8_t rd = static_cast<uint8_t>(param[0]);
      uint8_t rs1 = static_cast<uint8_t>(param[1]);
      if (param.size() == 3) {
        int64_t imm = param[2];
        buf_.emplace_back(op, rs1, 0, 0, rd, IRInsr::kRoundingMode, imm);
      } else {
        buf_.emplace_back(op, rs1, 0, 0, rd, IRInsr::kConst, 0);
      }
      break;
    }
    case R4_TYPE: {
      uint8_t rd = static_cast<uint8_t>(param[0]);
      uint8_t rs1 = static_cast<uint8_t>(param[1]);
      uint8_t rs2 = static_cast<uint8_t>(param[2]);
      uint8_t rs3 = static_cast<uint8_t>(param[3]);
      buf_.emplace_back(op, rs1, rs2, rs3, rd, IRInsr::kConst, 0);
      break;
    }
  }
}

void InsrGen::PushCalleeRegs(int64_t offset) {
  for (size_t i = 0; i < rv64::kNumCalleeSaved; ++i) {
    uint8_t reg = rv64::kCalleeSaved[i];
    if (reg == rv64::kSp) continue;
    GenerateInsr(INSR_SD, reg, rv64::kSp, IRInsr::kConst,
                 offset + 8 * int64_t(reg));
  }
  for (size_t i = 0; i < rv64::kNumFloatSavedRegs; ++i) {
    uint8_t reg = rv64::kFloatSavedRegs[i];
    GenerateInsr(INSR_SD, reg, rv64::kSp, IRInsr::kConst,
                 offset + 8 * int64_t(reg - 96));
  }
}

void InsrGen::PushCallerRegs(int64_t offset) {
  // For now the caller saved registers will not be used.
  return;
  for (size_t i = 0; i < rv64::kNumCalleeSaved; ++i) {
    GenerateInsr(INSR_SD, rv64::kCallerSaved[i], rv64::kSp, IRInsr::kConst,
                 offset + 8 * int64_t(rv64::kCallerSaved[i]));
  }
}

void InsrGen::PopCalleeRegs(int64_t offset) {
  for (size_t i = 0; i < rv64::kNumCalleeSaved; ++i) {
    uint8_t reg = rv64::kCalleeSaved[i];
    if (reg == rv64::kSp) continue;
    GenerateInsr(INSR_LD, reg, rv64::kSp, IRInsr::kConst,
                 offset + 8 * int64_t(reg));
  }
  for (size_t i = 0; i < rv64::kNumFloatSavedRegs; ++i) {
    uint8_t reg = rv64::kFloatSavedRegs[i];
    GenerateInsr(INSR_LD, reg, rv64::kSp, IRInsr::kConst,
                 offset + 8 * int64_t(reg - 96));
  }
}

void InsrGen::PopCallerRegs(int64_t offset) {
  // For now the caller saved registers will not be used.
  return;
  for (size_t i = 0; i < rv64::kNumCalleeSaved; ++i) {
    GenerateInsr(INSR_LD, rv64::kCallerSaved[i], rv64::kSp, IRInsr::kConst,
                 offset + 8 * int64_t(rv64::kCallerSaved[i]));
  }
}

size_t InsrGen::GeneratePrologue(size_t local) {
  GenerateInsr(INSR_ADDI, rv64::kSp, rv64::kSp, IRInsr::kConst, kNegSpOffset);
  PushCalleeRegs(local);
  GenerateInsr(INSR_ADDI, rv64::kFp, rv64::kSp, IRInsr::kConst, kPosSpOffset);
  return 8 * 32;
}

void InsrGen::GenerateEpilogue(size_t local) {
  PopCalleeRegs(local);
  GenerateInsr(INSR_ADDI, rv64::kSp, rv64::kSp, IRInsr::kConst, kPosSpOffset);
}

void InsrGen::GenerateRTypeInsr(const IRInsr &ir,
                                std::vector<MemoryLocation> &loc,
                                std::vector<uint8_t> &dirty) {
  uint8_t rd, rs1, rs2;
  if (IsIntOp(ir.op)) {
    rd = GetSavedReg(ir.rd, false, loc, dirty, int_reg_);
    rs1 = GetSavedReg(ir.rs1, true, loc, dirty, int_reg_);
    rs2 = GetSavedReg(ir.rs2, true, loc, dirty, int_reg_);
  } else {
    rd = GetSavedReg(ir.rd, false, loc, dirty, float_reg_);
    rs1 = GetSavedReg(ir.rs1, true, loc, dirty, float_reg_);
    rs2 = GetSavedReg(ir.rs2, true, loc, dirty, float_reg_);
  }
  dirty[ir.rd.id] = 1;
  GenerateInsr(ir.op, rd, rs1, rs2);
}

void InsrGen::GenerateITypeInsr(const IRInsr &ir,
                                std::vector<MemoryLocation> &loc,
                                std::vector<uint8_t> &dirty) {
  uint8_t rd, rs1;
  if (IsIntOp(ir.op)) {
    rd = GetSavedReg(ir.rd, false, loc, dirty, int_reg_);
    rs1 = GetSavedReg(ir.rs1, true, loc, dirty, int_reg_);
  } else {
    rd = GetSavedReg(ir.rd, false, loc, dirty, float_reg_);
    rs1 = GetSavedReg(ir.rs1, true, loc, dirty, float_reg_);
  }
  dirty[ir.rd.id] = 1;
  GenerateInsr(ir.op, rd, rs1, ir.imm_type, ir.imm);
}

void InsrGen::GenerateSTypeInsr(const IRInsr &ir,
                                std::vector<MemoryLocation> &loc,
                                std::vector<uint8_t> &dirty) {
  uint8_t rs1, rs2;
  if (IsIntOp(ir.op)) {
    rs1 = GetSavedReg(ir.rs1, true, loc, dirty, int_reg_);
    rs2 = GetSavedReg(ir.rs2, true, loc, dirty, int_reg_);
  } else {
    rs1 = GetSavedReg(ir.rs1, true, loc, dirty, float_reg_);
    rs2 = GetSavedReg(ir.rs2, true, loc, dirty, float_reg_);
  }
  // assert(ir.imm_type == IRInsr::kConst);
  GenerateInsr(ir.op, rs2, rs1, ir.imm_type, ir.imm);
}

void InsrGen::GenerateUTypeInsr(const IRInsr &ir,
                                std::vector<MemoryLocation> &loc,
                                std::vector<uint8_t> &dirty) {
  uint8_t rd = GetSavedReg(ir.rd, false, loc, dirty, int_reg_);
  dirty[ir.rd.id] = 1;
  assert(ir.imm_type == IRInsr::kConst);
  GenerateInsr(ir.op, rd, ir.imm_type, ir.imm);
}

void InsrGen::GenerateBTypeInsr(const IRInsr &ir,
                                std::vector<MemoryLocation> &loc,
                                std::vector<uint8_t> &dirty) {
  uint8_t rs1 = GetSavedReg(ir.rs1, true, loc, dirty, int_reg_);
  uint8_t rs2 = GetSavedReg(ir.rs2, true, loc, dirty, int_reg_);
  GenerateInsr(ir.op, rs1, rs2, ir.imm_type, ir.imm);
}

void InsrGen::GenerateJTypeInsr(const IRInsr &ir,
                                std::vector<MemoryLocation> &loc,
                                std::vector<uint8_t> &dirty) {
  uint8_t rd = GetSavedReg(ir.rd, false, loc, dirty, int_reg_);
  if (ir.op == INSR_JAL) dirty[ir.rd.id] = 1;
  GenerateInsr(ir.op, rd, ir.imm_type, ir.imm);
}

void InsrGen::GenerateR0TypeInsr(const IRInsr &ir,
                                 std::vector<MemoryLocation> &loc,
                                 std::vector<uint8_t> &dirty) {
  if (IsCvtOp(ir.op)) {
    assert(!IsCvtFFOp(ir.op));  // double is not supported
    uint8_t rd, rs1;
    if (IsCvtIFOp(ir.op)) {
      rd = GetSavedReg(ir.rd, false, loc, dirty, int_reg_);
      rs1 = GetSavedReg(ir.rs1, true, loc, dirty, float_reg_);
    } else {
      rd = GetSavedReg(ir.rd, false, loc, dirty, float_reg_);
      rs1 = GetSavedReg(ir.rs1, true, loc, dirty, int_reg_);
    }
    dirty[rd] = 1;
    assert(ir.imm_type == IRInsr::kRoundingMode);
    GenerateInsr(ir.op, rd, rs1, ir.imm);
  } else {
    uint8_t rd = GetSavedReg(ir.rd, false, loc, dirty, float_reg_);
    uint8_t rs1 = GetSavedReg(ir.rs1, true, loc, dirty, float_reg_);
    dirty[rd] = 1;
    GenerateInsr(ir.op, rd, rs1);
  }
}

// optimization only
void InsrGen::GenerateR4TypeInsr(const IRInsr &ir,
                                 std::vector<MemoryLocation> &loc,
                                 std::vector<uint8_t> &dirty) {}

void InsrGen::GeneratePseudoInsr(const IRInsr &ir,
                                 std::vector<MemoryLocation> &loc,
                                 std::vector<uint8_t> &dirty, int64_t offset) {
  switch (ir.op) {
    case PINSR_J:
    case PINSR_LA:
      GenerateInsr(ir.op, ir.imm_type, ir.imm);
      break;
    case PINSR_CALL:
      PushCallerRegs(offset);
      GenerateInsr(ir.op, ir.imm_type, ir.imm);
      PopCallerRegs(offset);
      break;
    case PINSR_RET:
      GenerateInsr(PINSR_J, IRInsr::kLabel, int64_t(tot_label_));
      // GenerateEpilogue(offset);
      // GenerateInsr(ir.op, ir.imm_type, ir.imm);
      break;
    case PINSR_MV: {
      uint8_t rd = GetSavedReg(ir.rd, false, loc, dirty, int_reg_);
      uint8_t rs1 = GetSavedReg(ir.rs1, true, loc, dirty, int_reg_);
      dirty[ir.rd.id] = 1;
      GenerateInsr(ir.op, rd, rs1);
      break;
    }
    case PINSR_FMV_S: {
      uint8_t rd = GetSavedReg(ir.rd, false, loc, dirty, float_reg_);
      uint8_t rs1 = GetSavedReg(ir.rs1, true, loc, dirty, float_reg_);
      dirty[ir.rd.id] = 1;
      GenerateInsr(ir.op, rd, rs1);
      break;
    }
  }
}

void InsrGen::GenerateInsrImpl(const IRInsr &v,
                               std::vector<MemoryLocation> &loc,
                               std::vector<uint8_t> &dirty) {
  switch (kRV64InsrFormat.at(v.op)) {
    case R_TYPE:
      return GenerateRTypeInsr(v, loc, dirty);
    case I_TYPE:
      return GenerateITypeInsr(v, loc, dirty);
    case S_TYPE:
      return GenerateSTypeInsr(v, loc, dirty);
    case U_TYPE:
      return GenerateRTypeInsr(v, loc, dirty);
    case B_TYPE:
      return GenerateITypeInsr(v, loc, dirty);
    case J_TYPE:
      return GenerateSTypeInsr(v, loc, dirty);
    case R0_TYPE:
      return GenerateR0TypeInsr(v, loc, dirty);
    case R4_TYPE:
      return GenerateR4TypeInsr(v, loc, dirty);
  }
}

void InsrGen::GenerateAR(size_t local, size_t num_register, size_t next_func,
                         bool is_main) {
  int_reg_.Clear();
  float_reg_.Clear();
  buf_.clear();
  std::vector<MemoryLocation> loc(num_register);
  std::vector<uint8_t> dirty(num_register);
  std::vector<std::pair<size_t, size_t>> lab;
  int64_t sp_offset = 8 * 64 + 8 * num_register;  // TODO: optimize this number
  size_t ed =
      next_func < label_.size() ? label_[next_func].ir_pos : ir_insr_.size();
  while (ir_pos_ < ed) {
    const IRInsr &v = ir_insr_[ir_pos_];
    while (label_pos_ < next_func && label_[label_pos_].ir_pos == ir_pos_) {
      lab.emplace_back(buf_.size(), label_pos_);
      label_pos_++;
    }
    if (v.op == PINSR_PUSH_SP) {
      GeneratePrologue(local);
    } else if (IsPseudoOp(v.op)) {
      GeneratePseudoInsr(v, loc, dirty, local);
    } else {
      GenerateInsrImpl(v, loc, dirty);
    }
    ir_pos_++;
  }
  if (is_main) GenerateInsr(PINSR_MV, rv64::kA0, rv64::kZero);
  lab.emplace_back(buf_.size(), tot_label_++);
  GenerateEpilogue(local);
  for (auto &v : buf_) {
    if (v.imm == kPosSpOffset) v.imm = sp_offset;
    if (v.imm == kNegSpOffset) v.imm = -sp_offset;
  }
  // move all the instructions in the buffer to insr_
  for (size_t i = 0, j = 0; i < buf_.size(); ++i) {
    while (j < lab.size() && lab[j].first == i) {
      insr_.push_back(lab[j++].second);
    }
    insr_.push_back(std::move(buf_[i]));
  }
}

namespace {

void PrintData(std::ofstream &ofs, const CodeData &data) {
  ofs << "\t";
  size_t idx = data.index();
  switch (idx) {
    case 0: {  // std::vector<uint8_t>
      auto &v = std::get<std::vector<uint8_t>>(data);
      ofs << ".word";
      if (!v.empty()) {
        ofs << " ";
        int32_t init = 0;
        assert(v.size() == 4);
        for (size_t i = 0; i < 4; ++i) init |= v[i] << (i << 3);
        ofs << init;
      }
      break;
    }
    case 1: {  // std::string
      std::string s = std::get<std::string>(data);
      ofs << ".string \"" << EscapeString(s) << "\"";
      break;
    }
    case 2: {  // size_t
      ofs << ".space " << std::get<size_t>(data);
      break;
    }
  }
}

}  // namespace

void InsrGen::GenerateData(const std::vector<CodeData> &data) {
  data_.insert(data_.end(), data.begin(), data.end());
}

void InsrGen::GenerateData(std::vector<CodeData> &&data) {
  std::move(data.begin(), data.end(), std::back_inserter(data_));
}

void InsrGen::Flush() {
  ofs_ << ".data\n";
  for (size_t i = 0; i < data_.size(); ++i) {
    ofs_ << ".D" << i << ": ";
    PrintData(ofs_, data_[i]);
    ofs_ << "\n";
  }
  data_.clear();
  ofs_ << ".text\n";
  std::vector<size_t> pos(tot_label_);
  std::vector<size_t> pref(insr_.size());
  for (size_t p = 0; p < insr_.size(); ++p) {
    try {
      size_t l = std::get<size_t>(insr_[p]);
      pos[l] = p;
    } catch (std::bad_variant_access &) {
    }
  }
  for (size_t p = 0, f = 0; p < insr_.size(); ++p) {
    try {
      pref[p] = PrintInsr(ofs_, std::get<RV64Insr>(insr_[p]), p, pos, pref);
      ofs_ << "\n";
    } catch (std::bad_variant_access &) {
      size_t lb = std::get<size_t>(insr_[p]);
      // std::cerr << "lb = " << lb << "\n";
      if (lb < label_.size() && label_[lb].is_func) {
        assert(f < func_.size());
        std::string s(func_[f++].second);
        // std::cerr << "s = " << s << "\n";
        if (s == "main") s = "_start_MAIN";
        ofs_ << s << ":\n";
      } else if (lb >= label_.size()) {
        // epilogue
      } else {
        ofs_ << ".L" << lb << ":\n";
      }
    }
    if (p > 0) pref[p] += pref[p - 1];
  }
  insr_.clear();
}

void InsrGen::GenerateRV64() {
  assert(label_.empty() || label_[0].is_func);
  for (size_t i = 0; i < func_.size(); ++i) {
    const auto &attr = tab_[func_[i].first].GetValue<FunctionAttr>();
    size_t next_pos = label_pos_ + 1;
    while (next_pos < label_.size() && !label_[next_pos].is_func) ++next_pos;
    GenerateAR(attr.sp_offset, attr.tot_pseudo_reg, next_pos,
               std::string(func_[i].second) == "main");
  }
  Flush();
}
