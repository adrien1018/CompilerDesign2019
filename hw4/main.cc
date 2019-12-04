#include <fstream>
#include <iostream>

#include "analysis.h"
#include "driver.h"
#include "gv.h"

const std::string kDefaultOutput = "AST_Graph.gv";

void PrintUsage(const char* name) {
  std::cout << "Usage: " << name << " [-h|--help]\n"
            << "       " << name << " [--debug] [-o out_gv] file\n";
}

int main(int argc, char* argv[]) {
  if (argc < 2) {
    PrintUsage(argc ? argv[0] : "./parser");
    return 1;
  }
  bool debug = false;
  int i = 1;
  if (std::string(argv[i]) == "-h" || std::string(argv[i]) == "--help") {
    PrintUsage(argv[0]);
    return 0;
  }
  std::string outfile = kDefaultOutput;
  for (; i < argc; ++i) {
    if (std::string(argv[i]) == "--debug") {
      debug = true;
    } else if (std::string(argv[i]) == "-o") {
      if (++i == argc) {
        PrintUsage(argv[0]);
        return 1;
      }
      outfile = argv[i];
    } else {
      Driver drv(argv[i]);
      exit(0);
      if (drv.Parse(debug) == 0) {
        if (debug) {
          drv.BuildSymbolTable();
          // drv.SemanticAnalysis();
        }
        PrintGV(drv.prog, outfile);
        return 0;
      } else {
        return 1;
      }
    }
  }
  PrintUsage(argv[0]);
  return 1;
}
