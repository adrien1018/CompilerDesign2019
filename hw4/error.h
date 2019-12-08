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

enum ErrorType {
  ERR_UNDECL,
  ERR_TYPE_UNDECL,
  ERR_NOT_TYPE,
  ERR_NOT_CALLABLE,
  ERR_REDECL, // TODO: change to 2 location (print "declared here" note)
  ERR_ARGS_TOO_FEW, //
  ERR_ARGS_TOO_MANY, //
};
enum WarningType {
  WARN_CONVERSION,
  WARN_VOID_RETURN,
  WARN_RETURN_NOVAL,
};

void PrintError(const FileInfor&, const Location&, const std::string&);
void PrintWarning(const FileInfor&, const Location&, const std::string&);
void PrintNote(const FileInfor&, const Location&, const std::string&);

void PrintError(const FileInfor&, const Location&, ErrorType,
                const std::string&);
void PrintError(const FileInfor&, const Location&, ErrorType,
                const Location&, const std::string&);
void PrintWarning(const FileInfor&, const Location&, WarningType);
void PrintWarning(const FileInfor&, const Location&, WarningType,
                  DataType, DataType);

#endif
