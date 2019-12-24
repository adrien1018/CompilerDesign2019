#include "ast.h"

void RecursiveDelete(AstNode* nd) {
  for (AstNode* x : nd->child) RecursiveDelete(x);
  delete nd;
}
