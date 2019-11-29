#ifndef ANALYZE_H_
#define ANALYZE_H_

#include "entry.h"
#include "symtab.h"

std::pair<std::vector<TableEntry>, std::vector<SemanticError>>
    BuildSymbolTable(AstNode* prog);

#endif  // ANALYZE_H_
