#include "error.h"

#include <iomanip>

static inline void PrintMsg(std::istream* streamptr, const Location& l,
                            const std::string& filename, const std::string& m,
                            bool color_output, bool is_error, bool print_code) {
  auto StartColor = [color_output,is_error]() {
    if (color_output) {
      std::cerr << (is_error ? "\033[01;31m\033[K" : "\033[01;35m\033[K");
    }
  };
  auto EndColor = [color_output]() {
    if (color_output) std::cerr << "\033[m\033[K";
  };
  if (color_output) std::cerr << "\033[01m\033[K";
  std::cerr << filename << ':' << l.begin.line << ':' << l.begin.column << ':';
  EndColor();
  std::cerr << ' ';
  StartColor();
  std::cerr << (is_error ? "error: " : "warning: ");
  EndColor();
  std::cerr << m << '\n';
  if (!print_code) return;
  std::cerr << std::setw(5) << l.begin.line << " | ";
  auto pos = streamptr->tellg();
  streamptr->seekg(l.begin.offset - l.begin.column + 1);
  std::string line;
  std::getline(*streamptr, line);
  streamptr->seekg(pos);
  std::cerr << line.substr(0, l.begin.column - 1);
  size_t end_col = l.begin.line == l.end.line ? l.end.column : line.size();
  StartColor();
  std::cerr << line.substr(l.begin.column - 1, end_col - l.begin.column);
  EndColor();
  std::cerr << line.substr(end_col - 1) << '\n';
  std::cerr << "      | " << std::string(l.begin.column - 1, ' ');
  StartColor();
  std::cerr << '^' << std::string(end_col - l.begin.column - 1, '~');
  EndColor();
  std::cerr << '\n';
}

void PrintError(std::istream* streamptr, const Location& l,
                const std::string& filename, const std::string& m,
                bool color_output) {
  PrintMsg(streamptr, l, filename, m, color_output, true, true);
}

void PrintWarning(std::istream* streamptr, const Location& l,
                  const std::string& filename, const std::string& m,
                  bool color_output) {
  PrintMsg(streamptr, l, filename, m, color_output, false, true);
}
