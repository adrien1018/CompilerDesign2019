#ifndef ERROR_H_
#define ERROR_H_

#include <string>
#include <iostream>

#include "ast.h"

void PrintError(std::istream* streamptr, const Location& l,
                const std::string& filename, const std::string& m,
                bool color_output = false);
void PrintWarning(std::istream* streamptr, const Location& l,
                  const std::string& filename, const std::string& m,
                  bool color_output = false);

#endif
