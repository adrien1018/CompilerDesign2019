#include "instruction.h"

#include <algorithm>

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

void InsrGen::RegController::SetRegister(uint8_t pos, size_t id) {
  regs_[pos] = id;
}

template <size_t N>
uint8_t InsrGen::RegController::GetRegister(const std::array<uint8_t, N> &pool,
                                            size_t &replaced) {
  uint8_t rp = 0;
  for (size_t i = 0; i < N; ++i) {
    if (regs_[pool[i]] == kEmpty) return pool[i];
    if (regs_[pool[i]] != kReserved) rp = pool[i];
  }
  replaced = regs_[rp];
  return rp;
}

uint8_t InsrGen::RegController::GetSavedRegister(size_t &replaced) {
  return GetRegister(rv64::kSavedRegisters, replaced);
}

uint8_t InsrGen::RegController::GetTempRegister(size_t &replaced) {
  return GetRegister(rv64::kTempRegisters, replaced);
}

uint8_t InsrGen::GetSavedRegister(const IRInsr::Register &reg,
                                  std::vector<MemoryLocation> &loc) {
  if (reg.is_real) {
    // TODO: Check the current usage of the specified register, and store it
    // back to memory if neccesary.
  }
  size_t id = reg.id;
  if (loc[id].in_register) return std::get<uint8_t>(loc[id].mem);
  size_t to_replace = (size_t)-1;
  uint8_t rg = reg_.GetSavedRegister(to_replace);
  if (to_replace != (size_t)-1) {
    // store the register back to memory
    loc[to_replace].in_register = false;
    loc[to_replace].mem = -int64_t(to_replace) * 4;
    GenerateInsr(INSR_SW, rg, rv64::kFp, -int64_t(to_replace) * 4);
  }
  loc[id].in_register = true;
  loc[id].mem = rg;
  return rg;
}

template <class... Args>
void InsrGen::GenerateInsr(Opcode op, Args &&... args) {}

void InsrGen::PushCalleeRegisters(size_t offset) {
  for (size_t i = 0; i < rv64::kNumCalleeSaved; ++i) {
    GenerateInsr(INSR_SD, rv64::kCalleeSaved[i], rv64::kSp, offset + 8 * i);
  }
}

void InsrGen::PopCalleeRegisters(size_t offset) {
  for (size_t i = 0; i < rv64::kNumCalleeSaved; ++i) {
    GenerateInsr(INSR_LD, rv64::kCalleeSaved[i], rv64::kSp, offset + 8 * i);
  }
}

void InsrGen::GenerateRTypeInsr(const IRInsr &insr,
                                std::vector<MemoryLocation> &loc) {
  uint8_t rd = GetSavedRegister(insr.rd, loc);
  uint8_t rs1 = GetSavedRegister(insr.rs1, loc);
  uint8_t rs2 = GetSavedRegister(insr.rs2, loc);
  GenerateInsr(insr.op, rd, rs1, rs2);
}

void InsrGen::GeneratePrologue(size_t sp_offset, size_t local) {
  GenerateInsr(INSR_ADDI, rv64::kFp, rv64::kSp, 0);  // addi fp, sp, 0
  GenerateInsr(INSR_ADDI, rv64::kSp, rv64::kSp,
               -int64_t(sp_offset));  // addi sp, sp, -offset
  PushCalleeRegisters(local);
}

void InsrGen::GenerateEpilogue(size_t sp_offset, size_t local,
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
  size_t sp_offset = 0;  // TODO
  GeneratePrologue(sp_offset, local);
  GenerateEpilogue(sp_offset, local, buf);
  // move all the instructions in the buffer to insr_
  std::move(buf.begin(), buf.end(), std::back_inserter(insr_));
}

void InsrGen::Flush() {
  for (auto &x : insr_) ofs_ << x << "\n";
  insr_.clear();
}
