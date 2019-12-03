#ifndef ANALYSIS_H_
#define ANALYSIS_H_

#include <utility>
#include <vector>

#include "entry.h"
#include "symtab.h"

using SymTab = std::vector<TableEntry>;
using SymMap = SymbolMap<std::string>;
using ErrList = std::vector<SemanticError>;

std::pair<std::vector<TableEntry>, std::vector<SemanticError>> BuildSymbolTable(
    AstNode *prog);

void SemanticAnalysis(AstNode *prog, const SymTab &tab, ErrList &err);

#endif  // ANALYSIS_H_
