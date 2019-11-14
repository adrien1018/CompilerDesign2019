#ifndef DRIVER_H_
#define DRIVER_H_

#include <string>
#include "header.h"
#include "parser.hh"

#if _WIN32
#include <io.h>
#define DRIVER_ISATTY_ _isatty
#define DRIVER_FILENO_ _fileno
#else
#include <unistd.h>
#define DRIVER_ISATTY_ isatty
#define DRIVER_FILENO_ fileno
#endif

#define YY_DECL yy::parser::symbol_type yylex(Driver& drv)
// ... and declare it for the parser's sake.
YY_DECL;

class Driver {
 public:
  Driver() {
    color_output = DRIVER_ISATTY_(DRIVER_FILENO_(stderr));
  }
  std::string file;
  yy::location location;
  AstNode* prog;
  bool color_output;

  void scan_begin();
  void scan_end();
  int Parse(const std::string& f, bool debug = false);
  void PrintError(const yy::location& l, const std::string& m);
};

#endif  // DRIVER_H_
