#ifndef DRIVER_H_
#define DRIVER_H_

#include <fstream>
#include <string>
#include "ast.h"
#include "parser.hh"

#ifndef yyFlexLexer
#include <FlexLexer.h>
#endif

#if _WIN32
#include <io.h>
#define DRIVER_ISATTY_ _isatty
#define DRIVER_FILENO_ _fileno
#else
#include <unistd.h>
#define DRIVER_ISATTY_ isatty
#define DRIVER_FILENO_ fileno
#endif

#define YY_DECL yy::parser::symbol_type Driver::yylex_a()

class Driver : public yyFlexLexer {
  void SetColor_() { color_output = DRIVER_ISATTY_(DRIVER_FILENO_(stderr)); }
  std::istream* InitStream_(const std::string& str) {
    stream_ = new std::ifstream(str);
    return stream_;
  }
  std::istream* stream_;
  bool stream_create_;
  std::string filename_;

 public:
  Driver()
      : yyFlexLexer(),
        stream_(&std::cin),
        stream_create_(false),
        filename_("<stdin>") {
    SetColor_();
    filename_ = "<stdin>";
  }
  Driver(std::istream* i = 0, std::ostream* o = 0)
      : yyFlexLexer(i, o),
        stream_(i),
        stream_create_(false),
        filename_("<unknown>") {
    SetColor_();
  }
  Driver(std::istream& i, std::ostream& o)
      : yyFlexLexer(&i, &o),
        stream_(&i),
        stream_create_(false),
        filename_("<unknown>") {
    SetColor_();
  }
  Driver(const std::string& file)
      : yyFlexLexer(InitStream_(file)), stream_create_(true), filename_(file) {
    SetColor_();
  }

  ~Driver() {
    if (stream_create_) delete stream_;
  }
  Location location;
  AstNode* prog;
  bool color_output;

  int Parse(bool debug = false);

  void PrintError(const Location& l, const std::string& m);
  yy::parser::symbol_type yylex_a();
};

inline yy::parser::symbol_type yylex(Driver& drv) { return drv.yylex_a(); }

#endif  // DRIVER_H_
