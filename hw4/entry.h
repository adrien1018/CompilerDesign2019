#ifndef ENTRY_H_
#define ENTRY_H_

#include <variant>
#include <vector>

#include "ast.h"

struct VariableType {
  DataType data_type;
  std::vector<size_t> dims;

  VariableType() = default;
  VariableType(DataType type) : data_type(type) {}
  VariableType(DataType type, std::vector<size_t> &&dim)
      : data_type(type), dims(dim) {}

  bool IsArray() const noexcept { return !dims.empty(); }
  size_t GetDimension() const noexcept { return dims.size(); }
  DataType GetType() const noexcept { return data_type; }
};

struct AliasType {
  DataType canonical_type;

  AliasType() = default;
  AliasType(DataType type) : canonical_type(type) {}
};

struct FunctionType {
  DataType return_type;
  std::vector<VariableType> params;

  DataType GetReturnType() const noexcept { return return_type; }
};

enum EntryType { VARIABLE, ARRAY, FUNCTION, TYPE_ALIAS };

class TableEntry {
 private:
  EntryType type_;
  std::variant<VariableType, AliasType, FunctionType> value_;

 public:
  TableEntry() = default;
  TableEntry(EntryType type) : type_(type) {}

  template <class... Param>
  TableEntry(EntryType type, Param &&... param) : type_(type) {
    if (type == VARIABLE || type == ARRAY)
      value_ = VariableType(std::forward<Param>(param)...);
    else if (type == FUNCTION)
      value_ = AliasType(std::forward<Param>(param)...);
    else if (type == TYPE_ALIAS)
      value_ = FunctionType(std::forward<Param>(param)...);
  }

  EntryType GetType() const noexcept { return type_; }

  template <class T>
  T &GetValue() {
    return std::get<T>(value_);
  }
};

#endif  // ENTRY_H_
