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

constexpr bool IsLogicIFFOp(Opcode op) {
  return op == INSR_FEQ_S || op == INSR_FLT_S || op == INSR_FLE_S ||
         op == INSR_FEQ_D || op == INSR_FLT_D || op == INSR_FLE_D;
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

#define IMM(v) (v.imm)
#define ROUND(v) (rv64::kRoundingModeName[v.imm])

}  // namespace

inline std::string InsrGen::GetLabel(const RV64Insr &insr) const {
  return GetLabel(insr.imm);
}

inline std::string InsrGen::GetLabel(int64_t imm) const {
  if (imm < 0) return kBuiltinLabel[~imm];
  if ((size_t)imm >= label_.size()) {
    return "_end_" + func_name_[imm - label_.size()];
  }
  if (label_[imm].is_func) return label_func_[imm];
  return ".L" + std::to_string(imm);
}

inline std::string InsrGen::GetData(const RV64Insr &insr) const {
  return GetData(insr.imm);
}

inline std::string InsrGen::GetData(int64_t imm) const {
  if (data_[imm].index() == 1) {
    auto &str = std::get<std::string>(data_[imm]);
    return ".D" + std::to_string(str_cache_.at(str));
  }
  return ".D" + std::to_string(imm);
}

size_t InsrGen::PrintPseudoInsr(std::ofstream &ofs, const RV64Insr &insr,
                                size_t p, const std::vector<size_t> &pos,
                                const std::vector<size_t> &pref) const {
  static constexpr size_t kJumpThreshold = 1 << 17;
  if (insr.op != PINSR_J) {
    ofs << kRV64InsrCode.at(insr.op);
    if (insr.op != PINSR_RET) ofs << '\t';
  }
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
        ofs << "lui t0, \%hi(" << GetLabel(insr) << ")\n";
        ofs << "jalr x0, \%lo(" << GetLabel(insr) << ")(t0)\n";
        return 1;
      } else {
        ofs << kRV64InsrCode.at(insr.op) << ' ' << GetLabel(insr);
        return 0;
      }
    }
    case PINSR_CALL:
    case PINSR_TAIL:
      if (insr.imm_type == IRInsr::kConst) {
        ofs << IMM(insr);
      } else {
        assert(insr.imm_type == IRInsr::kLabel);
        ofs << GetLabel(insr);
      }
      break;
    case PINSR_LA:
      assert(insr.imm_type == IRInsr::kData);
      ofs << RD(insr) << ", " << GetData(insr);
      break;
    case PINSR_MV:
    case PINSR_FMV_S:
      ofs << RD(insr) << ", " << RS1(insr);
      break;
    case PINSR_LI: {
      ofs << RD(insr) << ", " << IMM(insr);
      break;
    }
  }
  return 0;
}

// Serialize RV64 instructions
size_t InsrGen::PrintInsr(std::ofstream &ofs, const RV64Insr &insr, size_t p,
                          const std::vector<size_t> &pos,
                          std::vector<size_t> &pref) const {
  ofs << "\t";
  if (insr.op >= kFloatingPointInsr) {
    return PrintPseudoInsr(ofs, insr, p, pos, pref);
  }
  ofs << kRV64InsrCode.at(insr.op) << '\t';
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
          ofs << RD(insr) << ", " << GetData(insr) << "(" << RS1(insr) << ")";
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
        ofs << RS2(insr) << ", " << GetData(insr) << "(" << RS1(insr) << ")";
      }
      break;
    case B_TYPE:
      if (insr.imm_type == IRInsr::kConst) {
        ofs << RS1(insr) << ", " << RS2(insr) << ", " << IMM(insr);
      } else {
        assert(insr.imm_type == IRInsr::kLabel);
        ofs << RS1(insr) << ", " << RS2(insr) << ", " << GetLabel(insr);
      }
      break;
    case J_TYPE:
      if (insr.imm_type == IRInsr::kConst) {
        ofs << RD(insr) << ", " << IMM(insr);
      } else {
        assert(insr.imm_type == IRInsr::kLabel);
        ofs << RD(insr) << ", " << GetLabel(insr);
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

namespace {

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

InsrGen::InsrGen(const std::string &file, CodeGenInfo code_gen)
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
    if constexpr (std::is_same_v<T, int>) {
      int_used_[reg.id] = true;
      return reg.id;
    } else {
      float_used_[128 ^ reg.id] = true;
      return reg.id;
    }
  }
  size_t id = reg.id;
  if (loc[id].in_register) {
    ctrl.Push(loc[id].reg);
    return loc[id].reg;
  }
  size_t to_replace = (size_t)-1;
  uint8_t rg = ctrl.GetSavedReg(to_replace, dirty);
  if (to_replace != (size_t)-1) {
    // store the register back to memory if the register is dirty
    loc[to_replace].in_register = false;
    if (dirty[to_replace]) {
      if (loc[to_replace].addr == MemoryLocation::kUnAllocated) {
        loc[to_replace].addr = -sp_offset_ - 8;
        sp_offset_ += 8;
      }
      if (std::is_same_v<T, int>) {
        PushInsr(INSR_SD, rg, rv64::kFp, IRInsr::kConst, loc[to_replace].addr);
      } else {
        PushInsr(INSR_FSD, rg, rv64::kFp, IRInsr::kConst, loc[to_replace].addr);
      }
    }
    dirty[to_replace] = false;
  }
  loc[id].in_register = true;
  loc[id].reg = rg;
  ctrl.SetPseudoReg(rg, id);
  if (load) {
    assert(loc[id].addr != MemoryLocation::kUnAllocated);
    // load the pseudo register from memory
    if (std::is_same_v<T, int>) {
      PushInsr(INSR_LD, rg, rv64::kFp, IRInsr::kConst, loc[id].addr);
    } else {
      PushInsr(INSR_FLD, rg, rv64::kFp, IRInsr::kConst, loc[id].addr);
    }
  }
  if constexpr (std::is_same_v<T, int>) {
    int_used_[rg] = true;
  } else {
    float_used_[rg ^ 128] = true;
  }
  return rg;
}

template <class... Args>
void InsrGen::PushPInsr(Opcode op, Args &&... args) {
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
    case PINSR_MV:
    case PINSR_FMV_S: {
      uint8_t rd = static_cast<uint8_t>(param[0]);
      uint8_t rs = static_cast<uint8_t>(param[1]);
      buf_.emplace_back(op, rs, 0, 0, rd, IRInsr::kConst, 0);
      break;
    }
    case PINSR_RET: {
      buf_.emplace_back(op, 0, 0, 0, 0, IRInsr::kConst, 0);
      break;
    }
    case PINSR_LI: {
      uint8_t rd = static_cast<uint8_t>(param[0]);
      IRInsr::ImmType imm_type = IRInsr::ImmType(param[1]);
      buf_.emplace_back(op, 0, 0, 0, rd, imm_type, param[2]);
    }
  }
}

void InsrGen::PushInsr(const RV64Insr &v) { buf_.push_back(v); }

template <class... Args>
void InsrGen::PushInsr(Opcode op, Args &&... args) {
  if (IsPseudoOp(op)) {
    PushPInsr(op, std::forward<Args>(args)...);
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

void InsrGen::PushCalleeRegs(int64_t offset,
                             const std::vector<uint8_t> &saved) {
  for (size_t i = 0; i < saved.size(); ++i) {
    uint8_t rg = saved[i];
    PushInsr(rg >= 32 ? INSR_FSD : INSR_SD, rg, rv64::kSp, IRInsr::kConst,
             -sp_offset_ + offset + 8 * int64_t(i));
  }
  // for (size_t i = 0; i < rv64::kNumCalleeSaved; ++i) {
  //   uint8_t reg = rv64::kCalleeSaved[i];
  //   if (reg == rv64::kSp) continue;
  //   PushInsr(INSR_SD, reg, rv64::kSp, IRInsr::kConst,
  //                offset + 8 * int64_t(reg));
  // }
  // for (size_t i = 0; i < rv64::kNumFloatSavedRegs; ++i) {
  //   uint8_t reg = rv64::kFloatSavedRegs[i];
  //   PushInsr(INSR_FSD, reg, rv64::kSp, IRInsr::kConst,
  //                offset + 8 * int64_t(reg - 96));
  // }
}

void InsrGen::PushCallerRegs(int64_t offset,
                             const std::vector<uint8_t> &caller_saved) {
  // For now the caller saved registers only include int and float arguments.
  for (size_t i = 0; i < caller_saved.size(); ++i) {
    PushInsr(INSR_SD, caller_saved[i], rv64::kSp, IRInsr::kConst,
             offset + 8 * int64_t(i));
  }
  // for (size_t i = 0; i < rv64::kNumCalleeSaved; ++i) {
  //   PushInsr(INSR_SD, rv64::kCallerSaved[i], rv64::kSp, IRInsr::kConst,
  //            offset + 8 * int64_t(rv64::kCallerSaved[i]));
  // }
}

void InsrGen::PopCalleeRegs(int64_t offset, const std::vector<uint8_t> &saved) {
  for (size_t i = 0; i < saved.size(); ++i) {
    uint8_t rg = saved[i];
    PushInsr(rg >= 32 ? INSR_FLD : INSR_LD, rg, rv64::kSp, IRInsr::kConst,
             offset + 8 * int64_t(i));
  }
  // for (size_t i = 0; i < rv64::kNumCalleeSaved; ++i) {
  //   uint8_t reg = rv64::kCalleeSaved[i];
  //   if (reg == rv64::kSp) continue;
  //   PushInsr(INSR_LD, reg, rv64::kSp, IRInsr::kConst,
  //                offset + 8 * int64_t(reg));
  // }
  // for (size_t i = 0; i < rv64::kNumFloatSavedRegs; ++i) {
  //   uint8_t reg = rv64::kFloatSavedRegs[i];
  //   PushInsr(INSR_FLD, reg, rv64::kSp, IRInsr::kConst,
  //                offset + 8 * int64_t(reg - 96));
  // }
}

void InsrGen::PopCallerRegs(int64_t offset,
                            const std::vector<uint8_t> &caller_saved) {
  // For now the caller saved registers only include int and float arguments.
  for (size_t i = 0; i < caller_saved.size(); ++i) {
    PushInsr(INSR_LD, caller_saved[i], rv64::kSp, IRInsr::kConst,
             offset + 8 * int64_t(i));
  }
  // for (size_t i = 0; i < rv64::kNumCalleeSaved; ++i) {
  //   PushInsr(INSR_LD, rv64::kCallerSaved[i], rv64::kSp, IRInsr::kConst,
  //            offset + 8 * int64_t(rv64::kCallerSaved[i]));
  // }
}

void InsrGen::GeneratePrologue(size_t local,
                               const std::vector<uint8_t> &saved) {
  // PushInsr(INSR_ADDI, rv64::kSp, rv64::kSp, IRInsr::kConst, -sp_offset_);
  PushCalleeRegs(local, saved);
  // PushInsr(INSR_ADDI, rv64::kFp, rv64::kSp, IRInsr::kConst, sp_offset_);
}

void InsrGen::GenerateEpilogue(size_t local,
                               const std::vector<uint8_t> &saved) {
  PopCalleeRegs(local, saved);
  PushInsr(INSR_ADDI, rv64::kSp, rv64::kSp, IRInsr::kConst, sp_offset_);
  PushInsr(PINSR_RET);
}

void InsrGen::GenerateRTypeInsr(const IRInsr &ir) {
  uint8_t rd, rs1, rs2;
  if (IsIntOp(ir.op)) {
    rd = GetSavedReg(ir.rd, false, int_loc_, int_dirty_, int_reg_);
    rs1 = GetSavedReg(ir.rs1, true, int_loc_, int_dirty_, int_reg_);
    rs2 = GetSavedReg(ir.rs2, true, int_loc_, int_dirty_, int_reg_);
    if (!ir.rd.is_real) int_dirty_[ir.rd.id] = 1;
  } else {
    if (IsLogicIFFOp(ir.op)) {
      rd = GetSavedReg(ir.rd, false, int_loc_, int_dirty_, int_reg_);
      if (!ir.rd.is_real) int_dirty_[ir.rd.id] = 1;
    } else {
      rd = GetSavedReg(ir.rd, false, float_loc_, float_dirty_, float_reg_);
      if (!ir.rd.is_real) float_dirty_[ir.rd.id] = 1;
    }
    rs1 = GetSavedReg(ir.rs1, true, float_loc_, float_dirty_, float_reg_);
    rs2 = GetSavedReg(ir.rs2, true, float_loc_, float_dirty_, float_reg_);
  }
  PushInsr(ir.op, rd, rs1, rs2);
}

namespace {

constexpr Opcode ItoR(Opcode op) {
  switch (op) {
    case INSR_ADDI:
      return INSR_ADD;
    case INSR_SLTI:
      return INSR_SLT;
    case INSR_SLTIU:
      return INSR_SLTU;
    case INSR_XORI:
      return INSR_XOR;
    case INSR_ORI:
      return INSR_OR;
    case INSR_ANDI:
      return INSR_AND;
    case INSR_SLLI:
      return INSR_SLL;
    case INSR_SRLI:
      return INSR_SRL;
    case INSR_SRAI:
      return INSR_SRA;
    case INSR_ADDIW:
      return INSR_ADDW;
    case INSR_SLLIW:
      return INSR_SLLW;
    case INSR_SRLIW:
      return INSR_SRLW;
    case INSR_SRAIW:
      return INSR_SRAW;
  }
  __builtin_unreachable();
}

}  // namespace

template <class T>
std::vector<RV64Insr> InsrGen::ExpandImm(const T &v, uint8_t rd,
                                         uint8_t rs1) const {
  std::vector<RV64Insr> res;
  if (IsLoadOp(v.op)) {
    res.resize(3);
    res[0] = RV64Insr{PINSR_LI, 0, 0, 0, rv64::kT0, IRInsr::kConst, v.imm};
    res[1] =
        RV64Insr{INSR_ADD, rs1, rv64::kT0, 0, rv64::kT0, IRInsr::kConst, 0};
    res[2] = RV64Insr{v.op, rv64::kT0, 0, 0, rd, IRInsr::kConst, 0};
  } else {
    res.resize(2);
    res[0] = RV64Insr{PINSR_LI, 0, 0, 0, rv64::kT0, IRInsr::kConst, v.imm};
    res[1] = RV64Insr{ItoR(v.op), rs1, rv64::kT0, 0, rd, IRInsr::kConst, 0};
  }
  return res;
}

void InsrGen::GenerateITypeInsr(const IRInsr &ir) {
  if (IsLoadOp(ir.op) && ir.imm_type == IRInsr::kData) {
    PushInsr(PINSR_LA, rv64::kT0, IRInsr::kData, ir.imm);
    uint8_t rd;
    if (IsIntOp(ir.op)) {
      rd = GetSavedReg(ir.rd, false, int_loc_, int_dirty_, int_reg_);
      if (!ir.rd.is_real) int_dirty_[ir.rd.id] = 1;
    } else {
      rd = GetSavedReg(ir.rd, false, float_loc_, float_dirty_, float_reg_);
      if (!ir.rd.is_real) float_dirty_[ir.rd.id] = 1;
    }
    PushInsr(ir.op, rd, rv64::kT0, IRInsr::kConst, 0);
    return;
  }
  uint8_t rd, rs1;
  if (IsIntOp(ir.op)) {
    rd = GetSavedReg(ir.rd, false, int_loc_, int_dirty_, int_reg_);
    rs1 = GetSavedReg(ir.rs1, true, int_loc_, int_dirty_, int_reg_);
    if (!ir.rd.is_real) int_dirty_[ir.rd.id] = 1;
  } else {
    rd = GetSavedReg(ir.rd, false, float_loc_, float_dirty_, float_reg_);
    if (!IsLoadOp(ir.op)) {
      rs1 = GetSavedReg(ir.rs1, true, float_loc_, float_dirty_, float_reg_);
    } else {
      rs1 = GetSavedReg(ir.rs1, true, int_loc_, int_dirty_, int_reg_);
    }
    if (!ir.rd.is_real) float_dirty_[ir.rd.id] = 1;
  }
  if (std::abs(ir.imm) >= (1 << 11)) {
    for (auto &v : ExpandImm(ir, rd, rs1)) PushInsr(v);
  } else {
    PushInsr(ir.op, rd, rs1, ir.imm_type, ir.imm);
  }
}

void InsrGen::GenerateSTypeInsr(const IRInsr &ir) {
  if (ir.imm_type == IRInsr::kData) {
    PushInsr(PINSR_LA, rv64::kT0, IRInsr::kData, ir.imm);
    uint8_t rs2;
    if (IsIntOp(ir.op)) {
      rs2 = GetSavedReg(ir.rs2, true, int_loc_, int_dirty_, int_reg_);
    } else {
      rs2 = GetSavedReg(ir.rs2, true, float_loc_, float_dirty_, float_reg_);
    }
    PushInsr(ir.op, rs2, rv64::kT0, IRInsr::kConst, 0);
    return;
  }
  uint8_t rs1, rs2;
  if (IsIntOp(ir.op)) {
    rs1 = GetSavedReg(ir.rs1, true, int_loc_, int_dirty_, int_reg_);
    rs2 = GetSavedReg(ir.rs2, true, int_loc_, int_dirty_, int_reg_);
  } else {
    rs1 = GetSavedReg(ir.rs1, true, int_loc_, int_dirty_, int_reg_);
    rs2 = GetSavedReg(ir.rs2, true, float_loc_, float_dirty_, float_reg_);
  }
  // assert(ir.imm_type == IRInsr::kConst);
  PushInsr(ir.op, rs2, rs1, ir.imm_type, ir.imm);
}

void InsrGen::GenerateUTypeInsr(const IRInsr &ir) {
  uint8_t rd = GetSavedReg(ir.rd, false, int_loc_, int_dirty_, int_reg_);
  if (!ir.rd.is_real) int_dirty_[ir.rd.id] = 1;
  assert(ir.imm_type == IRInsr::kConst);
  PushInsr(ir.op, rd, ir.imm_type, ir.imm);
}

void InsrGen::GenerateBTypeInsr(const IRInsr &ir) {
  uint8_t rs1 = GetSavedReg(ir.rs1, true, int_loc_, int_dirty_, int_reg_);
  uint8_t rs2 = GetSavedReg(ir.rs2, true, int_loc_, int_dirty_, int_reg_);
  PushInsr(ir.op, rs1, rs2, ir.imm_type, ir.imm);
}

void InsrGen::GenerateJTypeInsr(const IRInsr &ir) {
  uint8_t rd = GetSavedReg(ir.rd, false, int_loc_, int_dirty_, int_reg_);
  if (ir.op == INSR_JAL) {
    if (!ir.rd.is_real) int_dirty_[ir.rd.id] = 1;
  }
  PushInsr(ir.op, rd, ir.imm_type, ir.imm);
}

void InsrGen::GenerateR0TypeInsr(const IRInsr &ir) {
  if (IsCvtOp(ir.op)) {
    assert(!IsCvtFFOp(ir.op));  // double is not supported
    uint8_t rd, rs1;
    if (IsCvtIFOp(ir.op)) {
      rd = GetSavedReg(ir.rd, false, int_loc_, int_dirty_, int_reg_);
      rs1 = GetSavedReg(ir.rs1, true, float_loc_, float_dirty_, float_reg_);
      if (!ir.rd.is_real) int_dirty_[ir.rd.id] = 1;
      assert(ir.imm_type == IRInsr::kRoundingMode);
      PushInsr(ir.op, rd, rs1, ir.imm);
    } else {
      rd = GetSavedReg(ir.rd, false, float_loc_, float_dirty_, float_reg_);
      rs1 = GetSavedReg(ir.rs1, true, int_loc_, int_dirty_, int_reg_);
      if (!ir.rd.is_real) float_dirty_[ir.rd.id] = 1;
      PushInsr(ir.op, rd, rs1);
    }
  } else {
    uint8_t rd = 0, rs1 = 0;
    if (ir.op == INSR_FMV_X_W) {
      rd = GetSavedReg(ir.rd, false, int_loc_, int_dirty_, int_reg_);
      rs1 = GetSavedReg(ir.rs1, true, float_loc_, float_dirty_, float_reg_);
      if (!ir.rd.is_real) int_dirty_[ir.rd.id] = 1;
    } else {
      // std::cerr << "FVM_W_X\n";
      rd = GetSavedReg(ir.rd, false, float_loc_, float_dirty_, float_reg_);
      rs1 = GetSavedReg(ir.rs1, true, int_loc_, int_dirty_, int_reg_);
      if (!ir.rd.is_real) float_dirty_[ir.rd.id] = 1;
      // std::cerr << "rd = " << int(rd) << " rs1 = " << int(rs1) << "\n";
    }
    PushInsr(ir.op, rd, rs1);
  }
}

// optimization only
void InsrGen::GenerateR4TypeInsr(const IRInsr &ir) {}

void InsrGen::GeneratePseudoInsr(const IRInsr &ir, int64_t offset) {
  switch (ir.op) {
    case PINSR_J: {
      PushInsr(ir.op, ir.imm_type, ir.imm);
      break;
    }
    case PINSR_LA: {
      uint8_t rd = GetSavedReg(ir.rd, false, int_loc_, int_dirty_, int_reg_);
      if (!ir.rd.is_real) int_dirty_[ir.rd.id] = 1;
      PushInsr(ir.op, rd, ir.imm_type, ir.imm);
      break;
    }
    case PINSR_CALL:
      PushInsr(ir.op, ir.imm_type, ir.imm);
      break;
    case PINSR_RET:
      PushInsr(PINSR_J, IRInsr::kLabel, int64_t(tot_label_));
      break;
    case PINSR_MV: {
      uint8_t rd = GetSavedReg(ir.rd, false, int_loc_, int_dirty_, int_reg_);
      uint8_t rs1 = GetSavedReg(ir.rs1, true, int_loc_, int_dirty_, int_reg_);
      if (!ir.rd.is_real) int_dirty_[ir.rd.id] = 1;
      PushInsr(ir.op, rd, rs1);
      break;
    }
    case PINSR_FMV_S: {
      uint8_t rd =
          GetSavedReg(ir.rd, false, float_loc_, float_dirty_, float_reg_);
      uint8_t rs1 =
          GetSavedReg(ir.rs1, true, float_loc_, float_dirty_, float_reg_);
      if (!ir.rd.is_real) float_dirty_[ir.rd.id] = 1;
      PushInsr(ir.op, rd, rs1);
      break;
    }
  }
}

void InsrGen::GenerateInsrImpl(const IRInsr &v) {
  switch (kRV64InsrFormat.at(v.op)) {
    case R_TYPE:
      return GenerateRTypeInsr(v);
    case I_TYPE:
      return GenerateITypeInsr(v);
    case S_TYPE:
      return GenerateSTypeInsr(v);
    case U_TYPE:
      return GenerateUTypeInsr(v);
    case B_TYPE:
      return GenerateBTypeInsr(v);
    case J_TYPE:
      return GenerateJTypeInsr(v);
    case R0_TYPE:
      return GenerateR0TypeInsr(v);
    case R4_TYPE:
      return GenerateR4TypeInsr(v);
  }
}

void InsrGen::Initialize(size_t ireg, size_t freg) {
  int_reg_.Clear();
  float_reg_.Clear();
  buf_.clear();
  std::fill(int_used_.begin(), int_used_.end(), false);
  std::fill(float_used_.begin(), float_used_.end(), false);
  int_used_[rv64::kFp] = true;
  int_used_[rv64::kRa] = true;
  int_dirty_.assign(ireg, 0);
  float_dirty_.assign(freg, 0);
  int_loc_.assign(ireg, MemoryLocation());
  float_loc_.assign(freg, MemoryLocation());
  sp_offset_ = 0;
}

void InsrGen::GenerateAR(size_t local, size_t ireg, size_t freg,
                         size_t next_func, bool is_main) {
  Initialize(ireg, freg);
#ifdef INSRGEN_DEBUG
  std::cerr << "ireg = " << ireg << " freg = " << freg << "\n";
#endif
  std::vector<std::pair<size_t, size_t>> lab;
  size_t ed =
      next_func < label_.size() ? label_[next_func].ir_pos : ir_insr_.size();
#ifdef INSRGEN_DEBUG
  std::cerr << "ir_pos_ = " << ir_pos_ << " ed = " << ed << "\n";
#endif
  while (ir_pos_ < ed) {
    const IRInsr &v = ir_insr_[ir_pos_];
    while (label_pos_ < next_func && label_[label_pos_].ir_pos == ir_pos_) {
      lab.emplace_back(buf_.size(), label_pos_);
      label_pos_++;
    }
#ifdef INSRGEN_DEBUG
    std::cerr << "instr = " << kRV64InsrCode.find(v.op)->second << "\n";
#endif
    if (v.op == PINSR_PUSH_SP) {
      PushInsr(INSR_ADDI, rv64::kSp, rv64::kSp, IRInsr::kConst, kNegSpOffset);
      PushInsr(INSR_ADDI, rv64::kFp, rv64::kSp, IRInsr::kConst, kPosSpOffset);
      // push_sp = buf_.size();
      // GeneratePrologue(local);
    } else if (IsPseudoOp(v.op)) {
      GeneratePseudoInsr(v, local);
    } else {
      GenerateInsrImpl(v);
    }
    ir_pos_++;
  }
  while (label_pos_ < next_func) {
    lab.emplace_back(buf_.size(), label_pos_);
    label_pos_++;
  }
  std::vector<uint8_t> callee_saved;
  for (uint8_t i : rv64::kCalleeSaved) {
    if (int_used_[i]) callee_saved.push_back(i);
  }
  for (uint8_t i : rv64::kFloatSavedRegs) {
    if (float_used_[i ^ 128]) callee_saved.push_back(i);
  }
  // std::vector<uint8_t> caller_saved;
  // for (uint8_t i : rv64::kIntArgs) {
  //   if (int_used_[i]) caller_saved.push_back(i);
  // }
  // for (uint8_t i : rv64::kFloatArgs) {
  //   if (float_used_[i ^ 128]) caller_saved.push_back(i);
  // }
  sp_offset_ += 8 * callee_saved.size() + local;
  for (auto &v : buf_) {
    if (v.imm == kPosSpOffset) v.imm = sp_offset_;
    if (v.imm == kNegSpOffset) v.imm = -sp_offset_;
  }
  if (is_main) PushInsr(PINSR_MV, rv64::kA0, rv64::kZero);
  // move all the instructions in the buffer to insr_
  size_t st_prologue = buf_.size();
  GeneratePrologue(local, callee_saved);
  size_t ed_prologue = buf_.size();
  lab.emplace_back(buf_.size(), tot_label_++);
  GenerateEpilogue(local, callee_saved);

  auto Push = [this](const RV64Insr &v) {
    auto it = kRV64InsrFormat.find(v.op);
    if (it != kRV64InsrFormat.end() && it->second == I_TYPE &&
        v.imm_type == IRInsr::kConst && std::abs(v.imm) >= (1 << 11)) {
      auto e = ExpandImm(v, v.rd, v.rs1);
      insr_.insert(insr_.end(), e.begin(), e.end());
    } else {
      insr_.push_back(v);
    }
  };

  for (size_t i = 0, j = 0; i < buf_.size(); ++i) {
    while (j < lab.size() && lab[j].first == i) {
      insr_.push_back(lab[j++].second);
    }
    if (i == 0) {
      for (size_t k = st_prologue; k < ed_prologue; ++k) Push(buf_[k]);
    }
    if (i >= st_prologue && i < ed_prologue) continue;
    Push(buf_[i]);
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
        ofs << "\t";
        int32_t init = 0;
        assert(v.size() == 4);
        for (size_t i = 0; i < 4; ++i) init |= v[i] << (i << 3);
        ofs << init;
      }
      break;
    }
    case 1: {  // std::string
      std::string s = std::get<std::string>(data);
      ofs << ".string\t\"" << EscapeString(s) << '\"';
      break;
    }
    case 2: {  // size_t
      ofs << ".zero\t" << std::get<size_t>(data);
      break;
    }
  }
  ofs << "\n\t.align\t3";
}

}  // namespace

void InsrGen::GenerateData(const std::vector<CodeData> &data) {
  data_.insert(data_.end(), data.begin(), data.end());
}

void InsrGen::GenerateData(std::vector<CodeData> &&data) {
  std::move(data.begin(), data.end(), std::back_inserter(data_));
}

void InsrGen::Flush() {
  str_cache_.clear();
  for (size_t i = 0; i < data_.size(); ++i) {
    if (data_[i].index() == 1) {
      // .string, can be optimized
      std::string &str = std::get<std::string>(data_[i]);
      if (str_cache_.find(str) != str_cache_.end()) continue;
      str_cache_[str] = i;
    }
    if (data_[i].index() == 2) {
      ofs_ << ".bss\n";
    } else {
      ofs_ << ".data\n";
    }
    ofs_ << ".D" << i << ":\n";
    PrintData(ofs_, data_[i]);
    ofs_ << "\n";
  }
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
  for (size_t p = 0; p < insr_.size(); ++p) {
    try {
      pref[p] = PrintInsr(ofs_, std::get<RV64Insr>(insr_[p]), p, pos, pref);
      ofs_ << "\n";
    } catch (std::bad_variant_access &) {
      size_t lb = std::get<size_t>(insr_[p]);
      auto s = GetLabel(lb);
      if (s == "main") s = "_start_MAIN";
      ofs_ << s << ":\n";
    }
    if (p > 0) pref[p] += pref[p - 1];
  }
  insr_.clear();
  data_.clear();
}

void InsrGen::InitLabel() {
  func_name_.resize(func_.size());
  label_func_.resize(label_.size());
  for (size_t i = 0, j = 0; i < label_.size(); ++i) {
    if (label_[i].is_func) {
      std::string str(func_[j].second);
      func_name_[j++] = str;
      label_func_[i] = str;
    }
  }
}

void InsrGen::GenerateRV64() {
  InitLabel();
  assert(label_.empty() || label_[0].is_func);
  for (size_t i = 0; i < func_.size(); ++i) {
    const auto &attr = tab_[func_[i].first].GetValue<FunctionAttr>();
    size_t next_pos = label_pos_ + 1;
    while (next_pos < label_.size() && !label_[next_pos].is_func) ++next_pos;
#ifdef INSRGEN_DEBUG
    std::cerr << "label_pos = " << label_pos_ << " next_pos = " << next_pos
              << "\n";
    std::cerr << "sp_offset = " << attr.sp_offset << "\n";
#endif
    GenerateAR(attr.sp_offset, attr.tot_preg.ireg, attr.tot_preg.freg, next_pos,
               std::string(func_[i].second) == "main");
  }
  Flush();
}
