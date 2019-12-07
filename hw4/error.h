#ifndef ERROR_H_
#define ERROR_H_

#include <string>
#include <iostream>

#include "ast.h"

struct SemanticError {
  enum ErrorType {
    VAR_REDECL,          // redeclaration of variable
    VAR_UNDECL,          // variable undeclared
    INCOMPAT_ARRAY_DIM,  // incompatible array dimensions
    TOO_FEW_ARGS,        // too few arguments
    TOO_MANY_ARGS,       // too many arguments
  } error;
  Location loc1, loc2;
  std::string msg;

  SemanticError() = default;
  SemanticError(ErrorType err) : error(err) {}
  SemanticError(ErrorType err, std::string&& s) : error(err), msg(s) {}
};

void PrintError(std::istream* streamptr, const Location& l,
                const std::string& filename, const std::string& m,
                bool color_output = false);
void PrintWarning(std::istream* streamptr, const Location& l,
                  const std::string& filename, const std::string& m,
                  bool color_output = false);

#endif
