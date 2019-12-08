#ifndef ERROR_H_
#define ERROR_H_

#include <string>
#include <iostream>

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
  ERR_REDECL, // TODO: change to 2 location (print "declared here" note)
  ERR_ARGS_TOO_FEW, //
  ERR_ARGS_TOO_MANY, //
  ERR_SUBSCRIPT_NOT_INT,
  ERR_ARR_DIMEN,
  ERR_SCALAR_TO_ARR, // TODO: change to 2 location (print "initializing argument")
  ERR_ARR_TO_SCALAR, //
  ERR_DIMEN_NOT_INT,
  ERR_DIMEN_NEG,
  ERR_VOID_ASSIGN,

  WARN_BEGIN = 30,

  WARN_CONVERSION,
  WARN_VOID_RETURN,
  WARN_RETURN_NOVAL,
  WARN_INCOMPAT_PTR,

  ERR_NOTHING
};

enum MsgClass {
  ERROR,
  WARNING,
  NOTE
};

inline MsgClass GetMsgClass(MsgType err) {
  return (int)err < (int)WARN_BEGIN ? ERROR : WARNING;
}

void PrintFileError(const FileInfor&, const std::string&);

void PrintMsg(const FileInfor&, const Location&, MsgClass, const std::string&);

void PrintMsg(const FileInfor&, const Location&, MsgType);
void PrintMsg(const FileInfor&, const Location&, MsgType,
              const std::string&);
void PrintMsg(const FileInfor&, const Location&, MsgType,
              const Location&, const std::string&);
void PrintMsg(const FileInfor&, const Location&, MsgType,
              const std::string&, const std::string&);
void PrintMsg(const FileInfor&, const Location&, MsgType,
              DataType, DataType);

#endif
