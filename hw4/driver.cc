#include "driver.h"

#include <iomanip>
#include <iostream>
#include "error.h"
#include "analysis.h"

int Driver::Parse(bool debug) {
  yy::parser parser(*this);
  if (debug) parser.set_debug_level(1);
  int res = parser();
  return res;
}

void Driver::PrintError(const Location& l, const std::string& m) {
  ::PrintError(stream_, l, filename_, m, color_output_);
}

void Driver::SemanticAnalysis() {
  Analyzer analyzer(filename_, stream_, color_output_);
  analyzer.BuildSymbolTable(prog);
  analyzer.SemanticAnalysis(prog);
}

int yyFlexLexer::yylex() {
  std::cout << "yylex called()!!\n";
  throw;
}
