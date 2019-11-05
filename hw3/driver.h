#ifndef DRIVER_H_
#define DRIVER_H_

#include <string>
#include "header.h"
#include "parser.hh"

#define YY_DECL yy::parser::symbol_type yylex(Driver& drv)
// ... and declare it for the parser's sake.
YY_DECL;

class Driver {
 public:
  Driver() {}
  int result;
  std::string file;
  yy::location location;
  AstNode* prog;
  void scan_begin();
  void scan_end();
  int parse(const std::string& f) {
    file = f;
    location.initialize(&file);
    scan_begin();
    yy::parser parse(*this);
    int res = parse();
    scan_end();
    return res;
  }
};

#endif  // DRIVER_H_
