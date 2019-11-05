#include <cstdlib>
#include "header.h"

AstNode *Allocate(AstType type) {
  AstNode *temp = new AstNode();
  temp->node_type = type;
  temp->data_type = NONE_TYPE;
  temp->parent = nullptr;
  // Notice that leftmostSibling is not initialized as NULL
  //temp->linenumber = linenumber;
  return temp;
}
