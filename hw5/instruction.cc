#include "instruction.h"

namespace {

#define RD(v) (v.op < kIntegerInsr ? "x" : "f") << int(v.rd)
#define RS1(v) (v.op < kIntegerInsr ? "x" : "f") << int(v.rs1)
#define RS2(v) (v.op < kIntegerInsr ? "x" : "f") << int(v.rs2)
#define RS3(v) (v.op < kIntegerInsr ? "x" : "f") << int(v.rs3)

void PrintPseudoInsr(std::ofstream &ofs, const RV64Insr &insr) {
  switch (insr.op) {
    case PINSR_J:
      ofs << "JAL x0, " << insr.imm;
      break;
    case PINSR_CALL:
      ofs << "AUIPC x6, " << (insr.imm >> 12) << '\n';
      ofs << "JALR x1, x6, " << (insr.imm & 4095);
      break;
    case PINSR_TAIL:
      ofs << "AUIPC x6, " << (insr.imm >> 12) << '\n';
      ofs << "JALR x0, x6, " << (insr.imm & 4095);
      break;
    case PINSR_RET:
      ofs << "JALR x0, x1, 0";
      break;
  }
}

// Serialize RV64 instructions
std::ofstream &operator<<(std::ofstream &ofs, const RV64Insr &insr) {
  ofs << kRV64InsrCode.at(insr.op) << ' ';
  switch (kRV64InsrFormat.at(insr.op)) {
    case R_TYPE:
      ofs << RD(insr) << ", " << RS1(insr) << ", " << RS2(insr);
      break;
    case I_TYPE:
      ofs << RD(insr) << ", " << RS1(insr) << insr.imm;
      break;
    case U_TYPE:
      ofs << RD(insr) << ", " << insr.imm;
      break;
    case S_TYPE:
      ofs << RS2(insr) << ", " << insr.imm << "(" << RS1(insr) << ")";
      break;
    case B_TYPE:
      ofs << RS1(insr) << ", " << RS2(insr) << ", " << insr.imm;
      break;
    case J_TYPE:
      ofs << RD(insr) << ", " << insr.imm;
      break;
    case R0_TYPE:
      ofs << RD(insr) << ", " << RS1(insr);
      break;
    case R4_TYPE:
      ofs << RD(insr) << ", " << RS1(insr) << ", " << RS2(insr) << ", "
          << RS3(insr);
      break;
    case PSEUDO:
      PrintPseudoInsr(ofs, insr);
      break;
  }
  return ofs;
}

}  // namespace
