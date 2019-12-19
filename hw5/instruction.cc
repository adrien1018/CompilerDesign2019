#include "instruction.h"

namespace {

#define RD(v) (v.op < kIntegerInsr ? "x" : "f") << int(v.rd)
#define RS1(v) (v.op < kIntegerInsr ? "x" : "f") << int(v.rs1)
#define RS2(v) (v.op < kIntegerInsr ? "x" : "f") << int(v.rs2)
#define RS3(v) (v.op < kIntegerInsr ? "x" : "f") << int(v.rs3)
#define IMM(v) (std::get<int64_t>(v.imm))
#define SYM(v) (std::get<std::string>(v.imm))

void PrintPseudoInsr(std::ofstream &ofs, const RV64Insr &insr) {
  switch (insr.op) {
    case PINSR_J:
    case PINSR_CALL:
    case PINSR_TAIL:
      ofs << IMM(insr);
      break;
    case PINSR_LA:
      ofs << RD(insr) << ", " << SYM(insr);
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
