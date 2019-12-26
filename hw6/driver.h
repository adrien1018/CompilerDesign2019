#ifndef DRIVER_H_
#define DRIVER_H_

#include <fstream>
#include <string>

#include "analysis.h"
#include "ast.h"
#include "codegen.h"
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
 private:
  void SetColor_() {
    file_.color_output = DRIVER_ISATTY_(DRIVER_FILENO_(stderr));
  }
  std::istream* InitStream_(const std::string& str) {
    SetColor_();
    std::ifstream* ptr = new std::ifstream(str);
    if (!ptr->is_open()) {
      PrintFileError(file_, str);
      throw std::runtime_error("");
    }
    file_.streamptr = ptr;
    return file_.streamptr;
  }
  FileInfor file_;
  bool stream_create_;
  std::vector<TableEntry> tab_;
  SymbolMap<std::string::value_type> mp_;

  void DeleteAst_();

 public:
  Location location;
  AstNode* prog = nullptr;

  Driver() : yyFlexLexer(), file_(&std::cin, "<stdin>"), stream_create_(false) {
    SetColor_();
  }
  Driver(std::istream* i = nullptr, std::ostream* o = nullptr)
      : yyFlexLexer(i, o), file_(i, "<unknown>"), stream_create_(false) {
    SetColor_();
  }
  Driver(std::istream& i, std::ostream& o)
      : yyFlexLexer(&i, &o), file_(&i, "<unknown>"), stream_create_(false) {
    SetColor_();
  }
  Driver(const std::string& file)
      : yyFlexLexer(InitStream_(file)), stream_create_(true) {
    file_.filename = file;
  }

  ~Driver() {
    //if (stream_create_) delete file_.streamptr;
    DeleteAst_();
  }

  int Parse(bool debug = false);

  void PrintError(const Location& l, const std::string& m);

  bool SemanticAnalysis();
  void CodeGeneration(const std::string& outfile);

  yy::parser::symbol_type yylex_a();
};

inline yy::parser::symbol_type yylex(Driver& drv) { return drv.yylex_a(); }

#endif  // DRIVER_H_
