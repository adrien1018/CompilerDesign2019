#include "driver.h"

#include <iomanip>
#include <iostream>
#include "error.h"
#include "analysis.h"

namespace {

void RecursiveDelete(AstNode* nd) {
  for (AstNode* x : nd->child) RecursiveDelete(x);
  delete nd;
}

} // namespace

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
  return analyzer.SemanticAnalysis(prog);
}

int yyFlexLexer::yylex() {
  std::cout << "yylex called()!!\n";
  throw;
}