#include "driver.h"

int Driver::Parse(const std::string& f, bool debug) {
  file = f;
  location.initialize(&file);
  scan_begin();
  yy::parser parser(*this);
  if (debug) parser.set_debug_level(1);
  int res = parser();
  scan_end();
  return res;
}

void Driver::PrintError(const yy::location& l, const std::string& m) {
  if (color_output) std::cerr << "\033[01m\033[K";
  std::cerr << file << ':' << l.begin.line << ':' << l.begin.column << ':';
  if (color_output) std::cerr << "\033[m\033[K";
  std::cerr << ' ';
  if (color_output) std::cerr << "\033[01;31m\033[K";
  std::cerr << "error: ";
  if (color_output) std::cerr << "\033[m\033[K";
  std::cerr << m << '\n';
}
