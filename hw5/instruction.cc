#include "instruction.h"

#include <algorithm>

const IRInsr::NoRD IRInsr::kNoRD;

namespace {

#define RD(v) (v.op < kIntegerInsr ? "x" : "f") << int(v.rd)
#define RS1(v) (v.op < kIntegerInsr ? "x" : "f") << int(v.rs1)
#define RS2(v) (v.op < kIntegerInsr ? "x" : "f") << int(v.rs2)
#define RS3(v) (v.op < kIntegerInsr ? "x" : "f") << int(v.rs3)
#define IMM(v) (v.imm)

void PrintPseudoInsr(std::ofstream &ofs, const RV64Insr &insr) {
  switch (insr.op) {
    case PINSR_J:
    case PINSR_CALL:
    case PINSR_TAIL:
      ofs << IMM(insr);
      break;
    case PINSR_LA:
      ofs << RD(insr) << ", " << IMM(insr);
      break;
  }
}

// Serialize RV64 instructions
std::ofstream &operator<<(std::ofstream &ofs, const RV64Insr &insr) {
  ofs << kRV64InsrCode.at(insr.op) << ' ';
  if (insr.op >= kFloatingPointInsr) {
    PrintPseudoInsr(ofs, insr);
  } else {
    switch (kRV64InsrFormat.at(insr.op)) {
      case R_TYPE:
        ofs << RD(insr) << ", " << RS1(insr) << ", " << RS2(insr);
        break;
      case I_TYPE:
        ofs << RD(insr) << ", " << RS1(insr) << IMM(insr);
        break;
      case U_TYPE:
        ofs << RD(insr) << ", " << IMM(insr);
        break;
      case S_TYPE:
        ofs << RS2(insr) << ", " << IMM(insr) << "(" << RS1(insr) << ")";
        break;
      case B_TYPE:
        ofs << RS1(insr) << ", " << RS2(insr) << ", " << IMM(insr);
        break;
      case J_TYPE:
        ofs << RD(insr) << ", " << IMM(insr);
        break;
      case R0_TYPE:
        ofs << RD(insr) << ", " << RS1(insr);
        break;
      case R4_TYPE:
        ofs << RD(insr) << ", " << RS1(insr) << ", " << RS2(insr) << ", "
            << RS3(insr);
        break;
    }
  }
  return ofs;
}

}  // namespace

uint8_t InsrGen::GetSavedRegister(const IRInsr::Register &reg, bool load,
                                  std::vector<MemoryLocation> &loc,
                                  std::vector<uint8_t> &dirty) {
  if (reg.is_real) {
    // TODO: Check the current usage of the specified register, and store it
    // back to memory if neccesary. For now the reserved register will not be
    // used, so leave this to optimzation.
    return reg.id;
  }
  size_t id = reg.id;
  if (loc[id].in_register) return std::get<uint8_t>(loc[id].mem);
  size_t to_replace = (size_t)-1;
  uint8_t rg = reg_.GetSavedRegister(to_replace, dirty);
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
  if (load) {
    // load the pseudo register from memory
    GenerateInsr(INSR_LD, rg, rv64::kFp, -int64_t(id) * 8);
  }
  return rg;
}

template <class... Args>
void InsrGen::GeneratePInsr(Opcode op, Args &&... args) {
  std::vector<int64_t> param{args...};
  switch (op) {
    case PINSR_J:
    case PINSR_CALL:
    case PINSR_TAIL:
      insr_.push_back({op, 0, 0, 0, 0, param[0]});
      break;
    case PINSR_LA: {
      uint8_t rd = static_cast<uint8_t>(param[0]);
      int64_t imm = param[1];
      insr_.push_back({op, 0, 0, 0, rd, imm});
      break;
    }
  }
}

template <class... Args>
void InsrGen::GenerateInsr(Opcode op, Args &&... args) {
  if (op >= kFloatingPointInsr) GeneratePInsr(op, std::forward<Args>(args)...);
  std::vector<int64_t> param{args...};
  switch (kRV64InsrFormat.at(op)) {
    case R_TYPE: {
      uint8_t rd = static_cast<uint8_t>(param[0]);
      uint8_t rs1 = static_cast<uint8_t>(param[1]);
      uint8_t rs2 = static_cast<uint8_t>(param[2]);
      insr_.push_back({op, rs1, rs2, 0, rd, 0});
      break;
    }
    case I_TYPE: {
      uint8_t rd = static_cast<uint8_t>(param[0]);
      uint8_t rs1 = static_cast<uint8_t>(param[1]);
      int64_t imm = param[2];
      insr_.push_back({op, rs1, 0, 0, rd, imm});
      break;
    }
    case U_TYPE: {
      uint8_t rd = static_cast<uint8_t>(param[0]);
      int64_t imm = param[1];
      insr_.push_back({op, 0, 0, 0, rd, imm});
      break;
    }
    case S_TYPE: {
      uint8_t rs2 = static_cast<uint8_t>(param[0]);
      int64_t imm = param[1];
      uint8_t rs1 = static_cast<uint8_t>(param[2]);
      insr_.push_back({op, rs1, rs2, 0, 0, imm});
      break;
    }
    case B_TYPE: {
      uint8_t rs1 = static_cast<uint8_t>(param[0]);
      uint8_t rs2 = static_cast<uint8_t>(param[1]);
      int64_t imm = param[2];
      insr_.push_back({op, rs1, rs2, 0, 0, imm});
      break;
    }
    case J_TYPE: {
      uint8_t rd = static_cast<uint8_t>(param[0]);
      int64_t imm = param[1];
      insr_.push_back({op, 0, 0, 0, rd, imm});
      break;
    }
    case R0_TYPE: {
      uint8_t rd = static_cast<uint8_t>(param[0]);
      uint8_t rs1 = static_cast<uint8_t>(param[1]);
      insr_.push_back({op, rs1, 0, 0, rd, 0});
      break;
    }
    case R4_TYPE: {
      uint8_t rd = static_cast<uint8_t>(param[0]);
      uint8_t rs1 = static_cast<uint8_t>(param[1]);
      uint8_t rs2 = static_cast<uint8_t>(param[2]);
      uint8_t rs3 = static_cast<uint8_t>(param[3]);
      insr_.push_back({op, rs1, rs2, rs3, rd, 0});
      break;
    }
  }
}

void InsrGen::PushCalleeRegisters(int64_t offset) {
  for (size_t i = 0; i < rv64::kNumCalleeSaved; ++i) {
    GenerateInsr(INSR_SD, rv64::kCalleeSaved[i], rv64::kSp,
                 offset + 8 * int64_t(i));
  }
}

void InsrGen::PopCalleeRegisters(int64_t offset) {
  for (size_t i = 0; i < rv64::kNumCalleeSaved; ++i) {
    GenerateInsr(INSR_LD, rv64::kCalleeSaved[i], rv64::kSp,
                 offset + 8 * int64_t(i));
  }
}

void InsrGen::GeneratePrologue(int64_t sp_offset, size_t local) {
  GenerateInsr(INSR_ADDI, rv64::kFp, rv64::kSp, 0);  // addi fp, sp, 0
  GenerateInsr(INSR_ADDI, rv64::kSp, rv64::kSp,
               -sp_offset);  // addi sp, sp, -offset
  PushCalleeRegisters(local);
}

void InsrGen::GenerateEpilogue(int64_t sp_offset, size_t local,
                               std::vector<RV64Insr> &buf) {
  PopCalleeRegisters(local);
  GenerateInsr(INSR_ADDI, rv64::kSp, rv64::kSp,
               sp_offset);  // addi sp, sp, offset
}

void InsrGen::GenerateAR(const std::vector<IRInsr> &ir, size_t local,
                         size_t num_register) {
  std::vector<RV64Insr> buf;
  std::vector<MemoryLocation> loc(num_register);
  std::vector<uint8_t> dirty(num_register);
  int64_t sp_offset = 0;  // TODO
  GeneratePrologue(sp_offset, local);
  GenerateEpilogue(sp_offset, local, buf);
  // move all the instructions in the buffer to insr_
  std::move(buf.begin(), buf.end(), std::back_inserter(insr_));
}

void InsrGen::Flush() {
  for (auto &x : insr_) ofs_ << x << "\n";
  insr_.clear();
}
