#include <cstdlib>
#include "header.h"

extern int linenumber;

AstNode *Allocate(AstType type) {
  AstNode *temp = new AstNode();
  temp->nodeType = type;
  temp->dataType = NONE_TYPE;
  temp->child = nullptr;
  temp->rightSibling = nullptr;
  temp->parent = nullptr;
  // Notice that leftmostSibling is not initialized as NULL
  temp->leftmostSibling = temp;
  temp->linenumber = linenumber;
  return temp;
}
