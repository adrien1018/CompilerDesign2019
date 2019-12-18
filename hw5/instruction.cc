#include "instruction.h"

namespace {

std::ofstream &operator<<(std::ofstream &ofs, const RV64Insr &insr) {
  ofs << kRV64InsrCode.at(insr.op) << ' ';
  switch (kRV64InsrFormat.at(insr.op)) {
    case R_TYPE:
      ofs << "x" << int(insr.rd) << ",x" << int(insr.rs1) << ",x"
          << int(insr.rs2);
      break;
    case I_TYPE:
      ofs << "x" << int(insr.rd) << ",x" << int(insr.rs1) << "," << insr.imm;
      break;
    case U_TYPE:
      ofs << "x" << int(insr.rd) << "," << insr.imm;
      break;
    case S_TYPE:
      ofs << "x" << int(insr.rs2) << "," << insr.imm << "(" << int(insr.rs1)
          << ")";
      break;
    case B_TYPE:
      ofs << "x" << int(insr.rs1) << ",x" << int(insr.rs2) << "," << insr.imm;
      break;
    case J_TYPE:
      ofs << "x" << int(insr.rd) << "," << insr.imm;
      break;
  }
  return ofs;
}

}  // namespace
