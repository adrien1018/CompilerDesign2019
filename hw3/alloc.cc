#include <cstdlib>
#include "header.h"

extern int linenumber;

AstNode *Allocate(AstType type) {
  AstNode *temp = new AstNode();
  temp->node_type = type;
  temp->data_type = NONE_TYPE;
  temp->child = nullptr;
  temp->right_sibling = nullptr;
  temp->parent = nullptr;
  // Notice that leftmostSibling is not initialized as NULL
  temp->leftmost_sibling = temp;
  temp->linenumber = linenumber;
  return temp;
}
