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

constexpr bool IsStoreOp(Opcode op) {
  return op == INSR_SB || op == INSR_SH || op == INSR_SW || op == INSR_SD ||
         op == INSR_FSD || op == INSR_FSW;
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
  if (label_[imm].is_func) {
    if (label_func_[imm] == "main") return "_start_MAIN";
    return label_func_[imm];
  }
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

void InsrGen::PrintPseudoInsr(std::ofstream &ofs, const RV64Insr &insr,
                              size_t p, const std::vector<size_t> &pos) const {
  switch (insr.op) {
    case PINSR_J: {
      if (insr.imm_type == IRInsr::kConst) {
        ofs << kRV64InsrCode.at(insr.op) << ' ' << IMM(insr);
        break;
      }
      PrintJal(ofs, p, pos, insr.imm);
      assert(insr.imm_type == IRInsr::kLabel);
      break;
    }
    case PINSR_CALL:
    case PINSR_TAIL:
      if (insr.imm_type == IRInsr::kConst) {
        ofs << "\t" << kRV64InsrCode.at(insr.op) << " " << IMM(insr);
      } else {
        assert(insr.imm_type == IRInsr::kLabel);
        ofs << "\t" << kRV64InsrCode.at(insr.op) << " " << GetLabel(insr);
      }
      break;
    case PINSR_LA:
      assert(insr.imm_type == IRInsr::kData);
      ofs << "\t" << kRV64InsrCode.at(insr.op) << " " << RD(insr) << ", "
          << GetData(insr);
      break;
    case PINSR_MV:
    case PINSR_FMV_S:
      ofs << "\t" << kRV64InsrCode.at(insr.op) << " " << RD(insr) << ", "
          << RS1(insr);
      break;
    case PINSR_LI: {
      ofs << "\t" << kRV64InsrCode.at(insr.op) << " " << RD(insr) << ", "
          << IMM(insr);
      break;
    }
    case PINSR_RET:
      ofs << "\tret";
      break;
  }
}

void InsrGen::PrintNegBranch(std::ofstream &ofs, const RV64Insr &insr) const {
  switch (insr.op) {
    case INSR_BEQ:
      ofs << "\tbne";
      break;
    case INSR_BNE:
      ofs << "\tbeq";
      break;
    case INSR_BLT:
      ofs << "\tbge";
      break;
    case INSR_BGE:
      ofs << "\tblt";
      break;
    case INSR_BLTU:
      ofs << "\tbgeu";
      break;
    case INSR_BGEU:
      ofs << "\tbltu";
      break;
    default:
      assert(false);
  }
  ofs << " " << RS1(insr) << ", " << RS2(insr) << ", .NEXT"
      << num_branch_expand_ << "\n";
}

void InsrGen::PrintJal(std::ofstream &ofs, size_t p,
                       const std::vector<size_t> &pos, int64_t imm) const {
  static constexpr size_t kExpandThresh = 1 << 17;
  int64_t dist = int64_t(p) - int64_t(pos[imm]);
  if (((size_t)std::abs(dist) << 2) < kExpandThresh) {
    ofs << "\tjal x0, " << GetLabel(imm);
    return;
  }
  ofs << "\tlui t0, \%hi(" << GetLabel(imm) << ")\n";
  ofs << "\tjalr x0, \%lo(" << GetLabel(imm) << ")(t0)";
}

void InsrGen::PrintBranchInsr(std::ofstream &ofs, const RV64Insr &insr,
                              size_t p, const std::vector<size_t> &pos) {
  static constexpr size_t kExpandThresh = 1 << 10;
  int64_t dist = int64_t(p) - int64_t(pos[insr.imm]);
  if (((size_t)std::abs(dist) << 2) < kExpandThresh) {
    ofs << "\t" << kRV64InsrCode.at(insr.op) << ' ' << RS1(insr) << ", "
        << RS2(insr) << ", " << GetLabel(insr);
    return;
  }
  PrintNegBranch(ofs, insr);
  PrintJal(ofs, p, pos, insr.imm);
  ofs << "\n.NEXT" << (num_branch_expand_++) << ":";
}

// Serialize RV64 instructions
void InsrGen::PrintInsr(std::ofstream &ofs, const RV64Insr &insr, size_t p,
                        const std::vector<size_t> &pos) {
  if (insr.op >= kFloatingPointInsr) {
    PrintPseudoInsr(ofs, insr, p, pos);
    return;
  }
  if (kRV64InsrFormat.at(insr.op) == B_TYPE) {
    PrintBranchInsr(ofs, insr, p, pos);
    return;
  }
  ofs << "\t";
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
uint8_t InsrGen::GetRealReg(const IRInsr::Register &reg, bool load,
                            std::vector<MemoryLocation> &loc) {
  if (reg.is_real) {
    if constexpr (std::is_same_v<T, int>) {
      int_used_[reg.id] = true;
    } else {
      float_used_[128 ^ reg.id] = true;
    }
    return reg.id;
  }
  if constexpr (std::is_same_v<T, int>) {
    if (reg.id < rv64::kNumIntSavedRegs) {
      int_used_[rv64::kIntSavedRegs[reg.id]] = true;
      return rv64::kIntSavedRegs[reg.id];
    }
    uint8_t rg = rv64::kIntTempRegs[int_tmp_++];
    if (load && loc[reg.id].addr != MemoryLocation::kUnAllocated) {
      PushInsr(INSR_LD, rg, rv64::kFp, IRInsr::kConst, loc[reg.id].addr);
    }
    return rg;
  } else {
    if (reg.id < rv64::kNumFloatSavedRegs) {
      float_used_[rv64::kFloatSavedRegs[reg.id] ^ 128] = true;
      return rv64::kFloatSavedRegs[reg.id];
    }
    uint8_t rg = rv64::kFloatTempRegs[float_tmp_++];
    if (load && loc[reg.id].addr != MemoryLocation::kUnAllocated) {
      PushInsr(INSR_FLD, rg, rv64::kFp, IRInsr::kConst, loc[reg.id].addr);
    }
    return rg;
  }
}

template <class T>
void InsrGen::PushStack(IRInsr::Register reg, uint8_t rg,
                        std::vector<MemoryLocation> &loc) {
  if (loc[reg.id].addr == MemoryLocation::kUnAllocated) {
    loc[reg.id].addr = -sp_offset_ - 8;
    sp_offset_ += 8;
  }
  if constexpr (std::is_same_v<T, int>) {
    PushInsr(INSR_SD, rg, rv64::kFp, IRInsr::kConst, loc[reg.id].addr);
  } else {
    PushInsr(INSR_FSD, rg, rv64::kFp, IRInsr::kConst, loc[reg.id].addr);
  }
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
}

void InsrGen::PushCallerRegs(int64_t offset,
                             const std::vector<uint8_t> &caller_saved) {
  // For now the caller saved registers only include int and float arguments.
  for (size_t i = 0; i < caller_saved.size(); ++i) {
    PushInsr(INSR_SD, caller_saved[i], rv64::kSp, IRInsr::kConst,
             offset + 8 * int64_t(i));
  }
}

void InsrGen::PopCalleeRegs(int64_t offset, const std::vector<uint8_t> &saved) {
  for (size_t i = 0; i < saved.size(); ++i) {
    uint8_t rg = saved[i];
    PushInsr(rg >= 32 ? INSR_FLD : INSR_LD, rg, rv64::kSp, IRInsr::kConst,
             offset + 8 * int64_t(i));
  }
}

void InsrGen::PopCallerRegs(int64_t offset,
                            const std::vector<uint8_t> &caller_saved) {
  // For now the caller saved registers only include int and float arguments.
  for (size_t i = 0; i < caller_saved.size(); ++i) {
    PushInsr(INSR_LD, caller_saved[i], rv64::kSp, IRInsr::kConst,
             offset + 8 * int64_t(i));
  }
}

void InsrGen::GeneratePrologue(size_t local,
                               const std::vector<uint8_t> &saved) {
  PushCalleeRegs(local, saved);
}

void InsrGen::GenerateEpilogue(size_t local,
                               const std::vector<uint8_t> &saved) {
  PopCalleeRegs(local, saved);
  PushInsr(INSR_ADDI, rv64::kSp, rv64::kSp, IRInsr::kConst, sp_offset_);
  PushInsr(PINSR_RET);
}

void InsrGen::GenerateRTypeInsr(const IRInsr &ir) {
  if (IsIntOp(ir.op)) {
    uint8_t rd = GetRealReg<int>(ir.rd, false, int_loc_);
    uint8_t rs1 = GetRealReg<int>(ir.rs1, true, int_loc_);
    uint8_t rs2 = GetRealReg<int>(ir.rs2, true, int_loc_);
    PushInsr(ir.op, rd, rs1, rs2);
    if (!ir.rd.is_real && !rv64::IsSavedReg<int>(rd)) {
      PushStack<int>(ir.rd, rd, int_loc_);
    }
  } else {
    if (IsLogicIFFOp(ir.op)) {
      uint8_t rd = GetRealReg<int>(ir.rd, false, int_loc_);
      uint8_t rs1 = GetRealReg<float>(ir.rs1, true, float_loc_);
      uint8_t rs2 = GetRealReg<float>(ir.rs2, true, float_loc_);
      PushInsr(ir.op, rd, rs1, rs2);
      if (!ir.rd.is_real && !rv64::IsSavedReg<int>(rd)) {
        PushStack<int>(ir.rd, rd, int_loc_);
      }
    } else {
      uint8_t rd = GetRealReg<float>(ir.rd, false, float_loc_);
      uint8_t rs1 = GetRealReg<float>(ir.rs1, true, float_loc_);
      uint8_t rs2 = GetRealReg<float>(ir.rs2, true, float_loc_);
      PushInsr(ir.op, rd, rs1, rs2);
      if (!ir.rd.is_real && !rv64::IsSavedReg<float>(rd)) {
        PushStack<float>(ir.rd, rd, float_loc_);
      }
    }
  }
  UnlockRegs();
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
  } else if (IsStoreOp(v.op)) {
    uint8_t rs2 = rd;
    res.resize(3);
    res[0] = RV64Insr{PINSR_LI, 0, 0, 0, rv64::kT0, IRInsr::kConst, v.imm};
    res[1] =
        RV64Insr{INSR_ADD, rs1, rv64::kT0, 0, rv64::kT0, IRInsr::kConst, 0};
    res[2] = RV64Insr{v.op, rv64::kT0, rs2, 0, 0, IRInsr::kConst, 0};
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
    if (IsIntOp(ir.op)) {
      uint8_t rd = GetRealReg<int>(ir.rd, false, int_loc_);
      PushInsr(ir.op, rd, rv64::kT0, IRInsr::kConst, 0);
      if (!ir.rd.is_real && !rv64::IsSavedReg<int>(rd)) {
        PushStack<int>(ir.rd, rd, int_loc_);
      }
    } else {
      uint8_t rd = GetRealReg<float>(ir.rd, false, float_loc_);
      PushInsr(ir.op, rd, rv64::kT0, IRInsr::kConst, 0);
      if (!ir.rd.is_real && !rv64::IsSavedReg<float>(rd)) {
        PushStack<float>(ir.rd, rd, float_loc_);
      }
    }
    UnlockRegs();
    return;
  }
  if (IsIntOp(ir.op)) {
    uint8_t rd = GetRealReg<int>(ir.rd, false, int_loc_);
    uint8_t rs1 = GetRealReg<int>(ir.rs1, true, int_loc_);
    PushInsr(ir.op, rd, rs1, ir.imm_type, ir.imm);
    if (!ir.rd.is_real && !rv64::IsSavedReg<int>(rd)) {
      PushStack<int>(ir.rd, rd, int_loc_);
    }
  } else {
    uint8_t rd = GetRealReg<float>(ir.rd, false, float_loc_);
    uint8_t rs1;
    if (!IsLoadOp(ir.op)) {
      rs1 = GetRealReg<float>(ir.rs1, true, float_loc_);
    } else {
      rs1 = GetRealReg<int>(ir.rs1, true, int_loc_);
    }
    PushInsr(ir.op, rd, rs1, ir.imm_type, ir.imm);
    if (!ir.rd.is_real && !rv64::IsSavedReg<float>(rd)) {
      PushStack<float>(ir.rd, rd, float_loc_);
    }
  }
  UnlockRegs();
  // if (std::abs(ir.imm) >= (1 << 11)) {
  //   for (auto &v : ExpandImm(ir, rd, rs1)) PushInsr(v);
  // } else {
  //   PushInsr(ir.op, rd, rs1, ir.imm_type, ir.imm);
  // }
}

void InsrGen::GenerateSTypeInsr(const IRInsr &ir) {
  if (ir.imm_type == IRInsr::kData) {
    PushInsr(PINSR_LA, rv64::kT0, IRInsr::kData, ir.imm);
    uint8_t rs2;
    if (IsIntOp(ir.op)) {
      rs2 = GetRealReg<int>(ir.rs2, true, int_loc_);
    } else {
      rs2 = GetRealReg<float>(ir.rs2, true, float_loc_);
    }
    PushInsr(ir.op, rs2, rv64::kT0, IRInsr::kConst, 0);
    UnlockRegs();
    return;
  }
  uint8_t rs1, rs2;
  if (IsIntOp(ir.op)) {
    rs1 = GetRealReg<int>(ir.rs1, true, int_loc_);
    rs2 = GetRealReg<int>(ir.rs2, true, int_loc_);
  } else {
    rs1 = GetRealReg<int>(ir.rs1, true, int_loc_);
    rs2 = GetRealReg<float>(ir.rs2, true, float_loc_);
  }
  PushInsr(ir.op, rs2, rs1, ir.imm_type, ir.imm);
  UnlockRegs();
}

void InsrGen::GenerateUTypeInsr(const IRInsr &ir) {
  uint8_t rd = GetRealReg<int>(ir.rd, false, int_loc_);
  PushInsr(ir.op, rd, ir.imm_type, ir.imm);
  if (!ir.rd.is_real && !rv64::IsSavedReg<int>(rd)) {
    PushStack<int>(ir.rd, rd, int_loc_);
  }
  UnlockRegs();
}

void InsrGen::GenerateBTypeInsr(const IRInsr &ir) {
  uint8_t rs1 = GetRealReg<int>(ir.rs1, true, int_loc_);
  uint8_t rs2 = GetRealReg<int>(ir.rs2, true, int_loc_);
  PushInsr(ir.op, rs1, rs2, ir.imm_type, ir.imm);
  UnlockRegs();
}

void InsrGen::GenerateJTypeInsr(const IRInsr &ir) {
  uint8_t rd = GetRealReg<int>(ir.rd, false, int_loc_);
  PushInsr(ir.op, rd, ir.imm_type, ir.imm);
  if (ir.op == INSR_JAL) {
    if (!ir.rd.is_real && !rv64::IsSavedReg<int>(rd)) {
      PushStack<int>(ir.rd, rd, int_loc_);
    }
  }
  UnlockRegs();
}

void InsrGen::GenerateR0TypeInsr(const IRInsr &ir) {
  if (IsCvtOp(ir.op)) {
    assert(!IsCvtFFOp(ir.op));  // double is not supported
    if (IsCvtIFOp(ir.op)) {
      uint8_t rd = GetRealReg<int>(ir.rd, false, int_loc_);
      uint8_t rs1 = GetRealReg<float>(ir.rs1, true, float_loc_);
      assert(ir.imm_type == IRInsr::kRoundingMode);
      PushInsr(ir.op, rd, rs1, ir.imm);
      if (!ir.rd.is_real && !rv64::IsSavedReg<int>(rd)) {
        PushStack<int>(ir.rd, rd, int_loc_);
      }
    } else {
      uint8_t rd = GetRealReg<float>(ir.rd, false, float_loc_);
      uint8_t rs1 = GetRealReg<int>(ir.rs1, true, int_loc_);
      PushInsr(ir.op, rd, rs1);
      if (!ir.rd.is_real && !rv64::IsSavedReg<float>(rd)) {
        PushStack<float>(ir.rd, rd, float_loc_);
      }
    }
    UnlockRegs();
  } else {
    if (ir.op == INSR_FMV_X_W) {
      uint8_t rd = GetRealReg<int>(ir.rd, false, int_loc_);
      uint8_t rs1 = GetRealReg<float>(ir.rs1, true, float_loc_);
      PushInsr(ir.op, rd, rs1);
      if (!ir.rd.is_real && !rv64::IsSavedReg<int>(rd)) {
        PushStack<int>(ir.rd, rd, int_loc_);
      }
    } else {
      uint8_t rd = GetRealReg<float>(ir.rd, false, float_loc_);
      uint8_t rs1 = GetRealReg<int>(ir.rs1, true, int_loc_);
      PushInsr(ir.op, rd, rs1);
      if (!ir.rd.is_real && !rv64::IsSavedReg<float>(rd)) {
        PushStack<float>(ir.rd, rd, float_loc_);
      }
    }
    UnlockRegs();
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
      uint8_t rd = GetRealReg<int>(ir.rd, false, int_loc_);
      PushInsr(ir.op, rd, ir.imm_type, ir.imm);
      if (!ir.rd.is_real && !rv64::IsSavedReg<int>(rd)) {
        PushStack<int>(ir.rd, rd, int_loc_);
      }
      UnlockRegs();
      break;
    }
    case PINSR_CALL:
      PushInsr(ir.op, ir.imm_type, ir.imm);
      break;
    case PINSR_RET:
      PushInsr(PINSR_J, IRInsr::kLabel, int64_t(tot_label_));
      break;
    case PINSR_MV: {
      uint8_t rd = GetRealReg<int>(ir.rd, false, int_loc_);
      uint8_t rs1 = GetRealReg<int>(ir.rs1, true, int_loc_);
      PushInsr(ir.op, rd, rs1);
      if (!ir.rd.is_real && !rv64::IsSavedReg<int>(rd)) {
        PushStack<int>(ir.rd, rd, int_loc_);
      }
      UnlockRegs();
      break;
    }
    case PINSR_FMV_S: {
      uint8_t rd = GetRealReg<float>(ir.rd, false, float_loc_);
      uint8_t rs1 = GetRealReg<float>(ir.rs1, true, float_loc_);
      PushInsr(ir.op, rd, rs1);
      if (!ir.rd.is_real && !rv64::IsSavedReg<float>(rd)) {
        PushStack<float>(ir.rd, rd, float_loc_);
      }
      UnlockRegs();
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
  buf_.clear();
  std::fill(int_used_.begin(), int_used_.end(), false);
  std::fill(float_used_.begin(), float_used_.end(), false);
  int_used_[rv64::kFp] = true;
  int_used_[rv64::kRa] = true;
  int_loc_.assign(ireg, MemoryLocation());
  float_loc_.assign(freg, MemoryLocation());
  sp_offset_ = 0;
}

void InsrGen::UnlockRegs() {
  int_tmp_ = 0;
  float_tmp_ = 0;
}

void InsrGen::GenerateAR(size_t local, size_t ireg, size_t freg,
                         size_t next_func, bool is_main) {
  Initialize(ireg, freg);
  std::vector<std::pair<size_t, size_t>> lab;
  size_t ed =
      next_func < label_.size() ? label_[next_func].ir_pos : ir_insr_.size();
  while (ir_pos_ < ed) {
    const IRInsr &v = ir_insr_[ir_pos_];
    while (label_pos_ < next_func && label_[label_pos_].ir_pos == ir_pos_) {
      lab.emplace_back(buf_.size(), label_pos_);
      label_pos_++;
    }
    if (v.op == PINSR_PUSH_SP) {
      PushInsr(INSR_ADDI, rv64::kSp, rv64::kSp, IRInsr::kConst, kNegSpOffset);
      PushInsr(INSR_ADDI, rv64::kFp, rv64::kSp, IRInsr::kConst, kPosSpOffset);
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
    } else if (IsStoreOp(v.op) && std::abs(v.imm) >= (1 << 11)) {
      auto e = ExpandImm(v, v.rs2, v.rs1);
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
  for (size_t p = 0; p < insr_.size(); ++p) {
    if (std::holds_alternative<size_t>(insr_[p])) {
      size_t l = std::get<size_t>(insr_[p]);
      pos[l] = p;
    }
  }
  for (size_t p = 0; p < insr_.size(); ++p) {
    if (std::holds_alternative<RV64Insr>(insr_[p])) {
      PrintInsr(ofs_, std::get<RV64Insr>(insr_[p]), p, pos);
      ofs_ << "\n";
    } else {
      size_t lb = std::get<size_t>(insr_[p]);
      auto s = GetLabel(lb);
      ofs_ << s << ":\n";
    }
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
    GenerateAR(attr.sp_offset, attr.tot_preg.ireg, attr.tot_preg.freg, next_pos,
               std::string(func_[i].second) == "main");
  }
  Flush();
}
