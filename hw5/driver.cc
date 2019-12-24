#include "driver.h"

#include <iomanip>
#include <iostream>

void Driver::DeleteAst_() {
  if (prog) RecursiveDelete(prog);
}

int Driver::Parse(bool debug) {
  yy::parser parser(*this);
  if (debug) parser.set_debug_level(1);
  int res = parser();
  return res;
}

void Driver::PrintError(const Location& l, const std::string& m) {
  ::PrintMsg(file_, l, ERROR, m);
}

bool Driver::SemanticAnalysis() {
  Analyzer analyzer(file_);
  if (!analyzer.BuildSymbolTable(prog)) return false;
  bool success = analyzer.SemanticAnalysis(prog);
  std::cerr << "here\n";
  std::tie(tab_, mp_) = analyzer.MoveSymbolTable();
  return success;
}

void Driver::CodeGeneration(const std::string& outfile) {
  CodeGen generator(std::move(tab_));
  generator.CodeGeneration(prog);
  InsrGen insr(outfile, std::move(generator.MoveInfo()));
  insr.GenerateRV64();
}

int yyFlexLexer::yylex() {
  std::cout << "yylex called()!!\n";
  throw;
}
