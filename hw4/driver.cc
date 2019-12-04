#include "driver.h"

#include <iomanip>
#include <iostream>

int Driver::Parse(bool debug) {
  yy::parser parser(*this);
  if (debug) parser.set_debug_level(1);
  int res = parser();
  return res;
}

void Driver::PrintError(const Location& l, const std::string& m) {
  if (color_output) std::cerr << "\033[01m\033[K";
  std::cerr << filename_ << ':' << l.begin.line << ':' << l.begin.column << ':';
  if (color_output) std::cerr << "\033[m\033[K";
  std::cerr << ' ';
  if (color_output) std::cerr << "\033[01;31m\033[K";
  std::cerr << "error: ";
  if (color_output) std::cerr << "\033[m\033[K";
  std::cerr << m << '\n';
  if (l.begin.line == l.end.line) {
    std::cerr << std::setw(5) << l.begin.line << " | ";
    auto pos = stream_->tellg();
    stream_->seekg(l.begin.offset - l.begin.column + 1);
    std::string line;
    std::getline(*stream_, line);
    stream_->seekg(pos);
    std::cerr << line.substr(0, l.begin.column - 1);
    if (color_output) std::cerr << "\033[01;31m\033[K";
    std::cerr << line.substr(l.begin.column - 1, l.end.column - l.begin.column);
    if (color_output) std::cerr << "\033[m\033[K";
    std::cerr << line.substr(l.end.column - 1) << '\n';
    std::cerr << "      | " << std::string(l.begin.column - 1, ' ');
    if (color_output) std::cerr << "\033[01;31m\033[K";
    std::cerr << '^' << std::string(l.end.column - l.begin.column - 1, '~');
    if (color_output) std::cerr << "\033[m\033[K";
    std::cerr << '\n';
  }
}

int yyFlexLexer::yylex() {
  std::cout << "yylex called()!!\n";
  throw;
}
