#ifndef ANALYZE_H_
#define ANALYZE_H_

#include <utility>
#include <vector>

#include "entry.h"
#include "symtab.h"

using SymTab = std::vector<TableEntry>;
using SymMap = SymbolMap<std::string>;
using ErrList = std::vector<SemanticError>;

std::pair<std::vector<TableEntry>, std::vector<SemanticError>> BuildSymbolTable(
    AstNode *prog);

void SemanticAnalyze(AstNode *prog, const SymTab &tab, ErrList &err);

#endif  // ANALYZE_H_
