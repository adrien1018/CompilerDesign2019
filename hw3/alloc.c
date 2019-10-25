#include "header.h"
#include <stdlib.h>

extern int linenumber;

AstNode *Allocate(AST_TYPE type){
    AstNode *temp;
    temp = (AstNode*)malloc(sizeof(struct AstNode));
    temp->nodeType = type;
    temp->dataType = NONE_TYPE;
    temp->child = NULL;
    temp->rightSibling = NULL;
    temp->parent = NULL;
    // Notice that leftmostSibling is not initialized as NULL
    temp->leftmostSibling = temp;
    temp->linenumber = linenumber;
    return temp;
}
