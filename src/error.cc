#include "error.h"

#include <errno.h>
#include <cassert>
#include <iomanip>

namespace {

inline void StartColor(MsgClass type, bool color_output) {
  if (color_output) {
    switch (type) {
      case ERROR:
        std::cerr << "\033[01;31m\033[K";
        break;
      case WARNING:
        std::cerr << "\033[01;35m\033[K";
        break;
      case NOTE:
        std::cerr << "\033[01;36m\033[K";
        break;
    }
  }
}
inline void StartEmph(bool color_output) {
  if (color_output) std::cerr << "\033[01m\033[K";
}
inline void EndColor(bool color_output) {
  if (color_output) std::cerr << "\033[m\033[K";
}

template <class Func>
inline void PrintMsg(const FileInfor& f, const Location& l, Func&& msg_callback,
                     MsgClass type, bool print_code) {
  auto StartColor = [f, type]() { ::StartColor(type, f.color_output); };
  auto EndColor = [f]() { ::EndColor(f.color_output); };
  StartEmph(f.color_output);
  std::cerr << f.filename << ':' << l.begin.line << ':' << l.begin.column
            << ':';
  EndColor();
  std::cerr << ' ';
  StartColor();
  switch (type) {
    case ERROR:
      std::cerr << "error: ";
      break;
    case WARNING:
      std::cerr << "warning: ";
      break;
    case NOTE:
      std::cerr << "note: ";
      break;
  }
  EndColor();
  msg_callback();
  std::cerr << '\n';
  if (print_code) {
    std::cerr << std::setw(5) << l.begin.line << " | ";
    auto pos = f.streamptr->tellg();
    f.streamptr->seekg(l.begin.offset - l.begin.column + 1);
    std::string line;
    std::getline(*f.streamptr, line);
    f.streamptr->seekg(pos);
    std::cerr << line.substr(0, l.begin.column - 1);
    size_t end_col =
        l.begin.line == l.end.line ? l.end.column : line.size() + 1;
    StartColor();
    std::cerr << line.substr(l.begin.column - 1, end_col - l.begin.column);
    EndColor();
    std::cerr << line.substr(end_col - 1) << '\n';
    std::cerr << "      | " << std::string(l.begin.column - 1, ' ');
    StartColor();
    std::cerr << '^';
    if (end_col > l.begin.column + 1) {
      std::cerr << std::string(end_col - l.begin.column - 1, '~');
    }
    EndColor();
    std::cerr << '\n';
  }
  std::cerr << std::flush;
}

inline std::string TypeStr(DataType a) {
  switch (a) {
    case INT_TYPE:
    case BOOLEAN_TYPE:
      return "int";
    case FLOAT_TYPE:
      return "float";
    default:
      throw;
  }
}

}  // namespace

void PrintFileError(const FileInfor& f, const std::string& file) {
  StartColor(ERROR, f.color_output);
  std::cerr << "error: ";
  EndColor(f.color_output);
  std::cerr << file << ": " << strerror(errno) << std::endl;
}

void PrintMsg(const FileInfor& f, const Location& l, MsgClass cl,
              const std::string& m) {
  PrintMsg(
      f, l, [&m]() { std::cerr << m; }, cl, true);
}

void PrintMsg(const FileInfor& f, const Location& l, MsgType err) {
  PrintMsg(
      f, l,
      [&]() {
        switch (err) {
          case ERR_SUBSCRIPT_NOT_INT: {
            std::cerr << "array subscript is not an integer";
            break;
          }
          case ERR_ARR_DIMEN: {
            std::cerr << "incompatible array dimensions";
            break;
          }
          case ERR_VOID_ASSIGN: {
            std::cerr << "void value not ignored as it ought to be";
            break;
          }
          case ERR_UNARY_MINUS_STRING: {
            std::cerr << "wrong type argument to unary minus";
            break;
          }
          case ERR_INVALID_INIT: {
            std::cerr << "invalid initializer";
            break;
          }
          case ERR_INIT_NONCONST: {
            std::cerr << "initializer element is not constant";
            break;
          }
          case WARN_VOID_RETURN: {
            StartEmph(f.color_output);
            std::cerr << "‘return’";
            EndColor(f.color_output);
            std::cerr << " with a value, in function returning void";
            break;
          }
          case WARN_RETURN_NOVAL: {
            StartEmph(f.color_output);
            std::cerr << "‘return’";
            EndColor(f.color_output);
            std::cerr << " with no value, in function returning non-void";
            break;
          }
          default: assert(false); // incorrect parameters
        }
      },
      GetMsgClass(err), true);
}

void PrintMsg(const FileInfor& f, const Location& l, MsgType err,
              const std::string_view& var) {
  PrintMsg(
      f, l,
      [&]() {
        switch (err) {
          case ERR_UNDECL: {
            StartEmph(f.color_output);
            std::cerr << "‘" << var << "’";
            EndColor(f.color_output);
            std::cerr << " undeclared";
            break;
          }
          case ERR_TYPE_UNDECL: {
            std::cerr << "unknown type name ";
            StartEmph(f.color_output);
            std::cerr << "‘" << var << "’";
            EndColor(f.color_output);
            break;
          }
          case ERR_NOT_TYPE: {
            StartEmph(f.color_output);
            std::cerr << "‘" << var << "’";
            EndColor(f.color_output);
            std::cerr << " was not declared as a type";
            break;
          }
          case ERR_NOT_VAR: {
            StartEmph(f.color_output);
            std::cerr << "‘" << var << "’";
            EndColor(f.color_output);
            std::cerr << " is not a variable";
            break;
          }
          case ERR_NOT_CALLABLE: {
            std::cerr << "called object ";
            StartEmph(f.color_output);
            std::cerr << "‘" << var << "’";
            EndColor(f.color_output);
            std::cerr << " is not a function";
            break;
          }
          case ERR_ARGS_TOO_FEW: {  // write
            std::cerr << "too few arguments to builtin function ";
            StartEmph(f.color_output);
            std::cerr << "‘" << var << "’";
            EndColor(f.color_output);
            break;
          }
          case ERR_ARGS_TOO_MANY: {  // write
            std::cerr << "too many arguments to builtin function ";
            StartEmph(f.color_output);
            std::cerr << "‘" << var << "’";
            EndColor(f.color_output);
            break;
          }
          case ERR_SCALAR_SUBSCRIPT: {
            std::cerr << "subscripted value ";
            StartEmph(f.color_output);
            std::cerr << "‘" << var << "’";
            EndColor(f.color_output);
            std::cerr << " is not an array";
            break;
          }
          case ERR_ARR_TO_SCALAR: {  // write
            std::cerr << "initialize scalar parameter from array ";
            StartEmph(f.color_output);
            std::cerr << "‘" << var << "’";
            EndColor(f.color_output);
            std::cerr << " on call to builtin function ";
            StartEmph(f.color_output);
            std::cerr << "‘write’";
            EndColor(f.color_output);
            break;
          }
          case ERR_DIMEN_NOT_INT: {
            std::cerr << "size of array ";
            StartEmph(f.color_output);
            std::cerr << "‘" << var << "’";
            EndColor(f.color_output);
            std::cerr << " has non-integer type";
            break;
          }
          case ERR_DIMEN_NEG: {
            std::cerr << "size of array ";
            StartEmph(f.color_output);
            std::cerr << "‘" << var << "’";
            EndColor(f.color_output);
            std::cerr << " is negative";
            break;
          }
          case ERR_VOID_ARRAY: {
            std::cerr << "declaration of ";
            StartEmph(f.color_output);
            std::cerr << "‘" << var << "’";
            EndColor(f.color_output);
            std::cerr << " as array of voids";
            break;
          }
          case ERR_RETURN_ARRAY: {
            StartEmph(f.color_output);
            std::cerr << "‘" << var << "’";
            EndColor(f.color_output);
            std::cerr << " declared as function returning an array";
            break;
          }
          default: assert(false); // incorrect parameters
        }
      },
      GetMsgClass(err), true);
}

void PrintMsg(const FileInfor& f, const Location& l, MsgType err,
              const Location& l2, const std::string_view& var) {
  PrintMsg(
      f, l,
      [&]() {
        switch (err) {
          case ERR_REDECL: {
            std::cerr << "redefinition of ";
            StartEmph(f.color_output);
            std::cerr << "‘" << var << "’";
            EndColor(f.color_output);
            break;
          }
          case ERR_REDECL_PARAM: {
            std::cerr << "redefinition of parameter ";
            StartEmph(f.color_output);
            std::cerr << "‘" << var << "’";
            EndColor(f.color_output);
            break;
          }
          case ERR_REDECL_CONFLICT: {
            std::cerr << "conflicting types for ";
            StartEmph(f.color_output);
            std::cerr << "‘" << var << "’";
            EndColor(f.color_output);
            break;
          }
          case ERR_REDECL_TYPE: {
            StartEmph(f.color_output);
            std::cerr << "‘" << var << "’";
            EndColor(f.color_output);
            std::cerr << " redeclared as different kind of symbol";
            break;
          }
          case ERR_ARGS_TOO_FEW: {
            std::cerr << "too few arguments to function ";
            StartEmph(f.color_output);
            std::cerr << "‘" << var << "’";
            EndColor(f.color_output);
            break;
          }
          case ERR_ARGS_TOO_MANY: {
            std::cerr << "too many arguments to function ";
            StartEmph(f.color_output);
            std::cerr << "‘" << var << "’";
            EndColor(f.color_output);
            break;
          }
          default: assert(false); // incorrect parameters
        }
      },
      GetMsgClass(err), true);
  PrintMsg(
      f, l2,
      [&]() {
        switch (err) {
          case ERR_REDECL:
          case ERR_REDECL_PARAM:
          case ERR_REDECL_CONFLICT:
          case ERR_REDECL_TYPE: {
            std::cerr << "the first declaration of ";
            StartEmph(f.color_output);
            std::cerr << "‘" << var << "’";
            EndColor(f.color_output);
            std::cerr << " was here";
            break;
          }
          case ERR_ARGS_TOO_FEW:
          case ERR_ARGS_TOO_MANY: {
            std::cerr << "declared here";
            break;
          }
          default: assert(false); // incorrect parameters
        }
      },
      NOTE, true);
}

void PrintMsg(const FileInfor& f, const Location& l, MsgType err,
              const Location& l2, size_t arg, const std::string_view& var2) {
  PrintMsg(
      f, l,
      [&]() {
        switch (err) {
          case ERR_SCALAR_TO_ARR: {
            std::cerr << "initialize array parameter from scalar";
            break;
          }
          default: assert(false); // incorrect parameters
        }
      },
      GetMsgClass(err), true);
  PrintMsg(
      f, l2,
      [&]() {
        switch (err) {
          case ERR_SCALAR_TO_ARR: {
            std::cerr << "passing argument " << arg << " of ";
            StartEmph(f.color_output);
            std::cerr << "‘" << var2 << "’";
            EndColor(f.color_output);
            break;
          }
          default:;
        }
      },
      NOTE, true);
}

void PrintMsg(const FileInfor& f, const Location& l, MsgType err,
              const Location& l2, size_t arg, DataType type,
              const std::string_view& var2) {
  PrintMsg(
      f, l,
      [&]() {
        switch (err) {
          case ERR_STRING_TO_SCALAR: {
            std::cerr << "incompatible type for argument " << arg << " of ";
            StartEmph(f.color_output);
            std::cerr << "‘" << var2 << "’";
            EndColor(f.color_output);
            break;
          }
          default: assert(false); // incorrect parameters
        }
      },
      GetMsgClass(err), true);
  PrintMsg(
      f, l2,
      [&]() {
        switch (err) {
          case ERR_STRING_TO_SCALAR: {
            std::cerr << "expected ";
            StartEmph(f.color_output);
            std::cerr << "‘" << (type == INT_TYPE ? "int" : "float") << "’";
            EndColor(f.color_output);
            std::cerr << " but argument is of type ";
            StartEmph(f.color_output);
            std::cerr << "‘char*’";
            EndColor(f.color_output);
            break;
          }
          default:;
        }
      },
      NOTE, true);
}

void PrintMsg(const FileInfor& f, const Location& l, MsgType err,
              const Location& l2, size_t arg, const std::string_view& var1,
              const std::string_view& var2) {
  PrintMsg(
      f, l,
      [&]() {
        switch (err) {
          case ERR_SCALAR_TO_ARR: {
            std::cerr << "initialize array parameter from scalar ";
            StartEmph(f.color_output);
            std::cerr << "‘" << var1 << "’";
            EndColor(f.color_output);
            break;
          }
          case ERR_ARR_TO_SCALAR: {
            std::cerr << "initialize scalar parameter from array ";
            StartEmph(f.color_output);
            std::cerr << "‘" << var1 << "’";
            EndColor(f.color_output);
            break;
          }
          case WARN_INCOMPAT_DIMEN: {
            std::cerr << "initializing array parameter from array ";
            StartEmph(f.color_output);
            std::cerr << "‘" << var1 << "’";
            EndColor(f.color_output);
            std::cerr << " with incompatible dimensions";
            break;
          }
          case WARN_INCOMPAT_ARR_TYPE: {
            std::cerr << "initializing array parameter from array ";
            StartEmph(f.color_output);
            std::cerr << "‘" << var1 << "’";
            EndColor(f.color_output);
            std::cerr << " with different type";
            break;
          }
          default: assert(false); // incorrect parameters
        }
      },
      GetMsgClass(err), true);
  PrintMsg(
      f, l2,
      [&]() {
        switch (err) {
          case ERR_SCALAR_TO_ARR:
          case ERR_ARR_TO_SCALAR:
          case WARN_INCOMPAT_DIMEN:
          case WARN_INCOMPAT_ARR_TYPE: {
            std::cerr << "passing argument " << arg << " of ";
            StartEmph(f.color_output);
            std::cerr << "‘" << var2 << "’";
            EndColor(f.color_output);
            break;
          }
          default:;
        }
      },
      NOTE, true);
}

void PrintMsg(const FileInfor& f, const Location& l, MsgType err, DataType t1,
              DataType t2) {
  PrintMsg(
      f, l,
      [&]() {
        switch (err) {
          case WARN_CONVERSION: {
            std::cerr << "conversion from ";
            StartEmph(f.color_output);
            std::cerr << "‘" << TypeStr(t1) << "’";
            EndColor(f.color_output);
            std::cerr << " to ";
            StartEmph(f.color_output);
            std::cerr << "‘" << TypeStr(t2) << "’";
            EndColor(f.color_output);
            std::cerr << " may change value";
            break;
          }
          default: assert(false); // incorrect parameters
        }
      },
      GetMsgClass(err), true);
}
