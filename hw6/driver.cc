#include "driver.h"

#include <iomanip>
#include <iostream>

void Driver::DeleteAst_() {
  if (prog_) RecursiveDelete(prog_);
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
  if (!analyzer.BuildSymbolTable(prog_)) return false;
  bool success = analyzer.SemanticAnalysis(prog_);
  auto p = analyzer.MoveSymbolTable();
  tab_ = std::move(std::get<0>(p));
  mp_ = std::move(std::get<1>(p));
  return success;
}

void Driver::CodeGeneration(const std::string& outfile,
                            const CodeGenOptions& opt) {
  CodeGen generator(std::move(tab_));
  generator.CodeGeneration(prog_, opt);
  InsrGen insr(outfile, generator.MoveInfo());
  insr.GenerateRV64();
}

int yyFlexLexer::yylex() {
  std::cout << "yylex called()!!\n";
  throw;
}
