#ifndef ERROR_H_
#define ERROR_H_

#include <iostream>
#include <string>

#include "ast.h"

struct FileInfor {
  std::istream* streamptr;
  std::string filename;
  bool color_output;
  FileInfor() = default;
  FileInfor(std::istream* streamptr, const std::string& filename)
      : streamptr(streamptr), filename(filename) {}
};

enum MsgType {
  ERR_UNDECL,
  ERR_TYPE_UNDECL,
  ERR_NOT_TYPE,
  ERR_NOT_VAR,
  ERR_NOT_CALLABLE,
  ERR_REDECL,
  ERR_REDECL_PARAM,
  ERR_REDECL_CONFLICT,
  ERR_REDECL_TYPE,
  ERR_ARGS_TOO_FEW,
  ERR_ARGS_TOO_MANY,
  ERR_SUBSCRIPT_NOT_INT,
  ERR_ARR_DIMEN,
  ERR_SCALAR_SUBSCRIPT,
  ERR_SCALAR_TO_ARR,
  ERR_ARR_TO_SCALAR,
  ERR_DIMEN_NOT_INT,
  ERR_DIMEN_NEG,
  ERR_VOID_ASSIGN,
  ERR_VOID_ARRAY,
  ERR_UNARY_MINUS_STRING,
  ERR_STRING_TO_SCALAR,
  ERR_INVALID_INIT,
  ERR_RETURN_ARRAY,
  ERR_INIT_NONCONST,
  ERR_INVALID_USE_OF_VOID,

  WARN_BEGIN = 30,

  WARN_CONVERSION,
  WARN_VOID_RETURN,
  WARN_RETURN_NOVAL,
  WARN_INCOMPAT_DIMEN,
  WARN_INCOMPAT_ARR_TYPE,

  ERR_NOTHING
};

enum MsgClass { ERROR, WARNING, NOTE };

inline MsgClass GetMsgClass(MsgType err) {
  return (int)err < (int)WARN_BEGIN ? ERROR : WARNING;
}

void PrintFileError(const FileInfor&, const std::string&);

void PrintMsg(const FileInfor&, const Location&, MsgClass, const std::string&);

void PrintMsg(const FileInfor&, const Location&, MsgType);
void PrintMsg(const FileInfor&, const Location&, MsgType,
              const std::string_view&);
void PrintMsg(const FileInfor&, const Location&, MsgType, const Location&,
              const std::string_view&);
void PrintMsg(const FileInfor&, const Location&, MsgType, const Location&,
              size_t, const std::string_view&);
void PrintMsg(const FileInfor&, const Location&, MsgType, const Location&,
              size_t, DataType, const std::string_view&);
void PrintMsg(const FileInfor&, const Location&, MsgType, const Location&,
              size_t, const std::string_view&, const std::string_view&);
void PrintMsg(const FileInfor&, const Location&, MsgType, DataType, DataType);

#endif  // ERROR_H_
