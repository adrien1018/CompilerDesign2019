#include <fstream>
#include <iostream>

#include "driver.h"
#include "gv.h"
#include "argparse.h"

int main(int argc, const char* argv[]) {
  argparse::ArgumentParser parser;
  parser.add_argument("--debug")
      .help("Print debug messages of scanner & lexer")
      .default_value(false)
      .implicit_value(true);
  parser.add_argument("-o")
      .help("Output assembly file")
      .default_value(std::string("output.s"));
  parser.add_argument("-O")
      .help("Run register allocation optimization")
      .default_value(false)
      .implicit_value(true);
  parser.add_argument("file").help("Input C-- file");

  try {
    parser.parse_args(argc, argv);
  } catch (const std::runtime_error& err) {
    std::cout << err.what() << std::endl << parser;
    return 1;
  }
  bool debug = parser.get<bool>("--debug");
  std::string infile = parser.get<std::string>("file");
  std::string outfile = parser.get<std::string>("-o");
  CodeGenOptions opt;
  opt.register_alloc = parser.get<bool>("-O");

  try {
    Driver drv(infile);
    if (drv.Parse(debug) == 0 && drv.SemanticAnalysis()) {
      drv.CodeGeneration(outfile, opt);
      return 0;
    } else {
      return 1;
    }
  } catch (std::runtime_error&) {
    std::cerr << "Compilation terminated.\n";
    return 1;
  }
}
