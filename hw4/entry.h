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

  template <class Iterator>
  VariableType(DataType type, Iterator bg, Iterator ed)
      : data_type(type), dims(bg, ed) {}

  bool IsArray() const noexcept { return !dims.empty(); }
  size_t GetDimension() const noexcept { return dims.size(); }
  DataType GetType() const noexcept { return data_type; }

  VariableType Slice(size_t dim) const noexcept {
    return VariableType(data_type, dims.begin() + dim, dims.end());
  }

  bool operator==(const VariableType& rhs) const {
    return data_type == rhs.data_type && dims == rhs.dims;
  }
  bool operator!=(const VariableType& rhs) const { return !operator==(rhs); }
};

struct FunctionType {
  DataType return_type;
  std::vector<VariableType> params;

  FunctionType() = default;
  FunctionType(DataType type, std::vector<VariableType> &&params)
      : return_type(type), params(params) {}

  DataType GetReturnType() const noexcept { return return_type; }
  size_t NumParam() const noexcept { return params.size(); }
};

enum EntryType { VARIABLE, FUNCTION, TYPE_ALIAS };

class TableEntry {
 private:
  AstNode* nd_;
  EntryType type_;
  std::variant<VariableType, DataType, FunctionType> value_;

 public:
  TableEntry() = default;
  TableEntry(AstNode* nd, EntryType type) : nd_(nd), type_(type) {}
  EntryType GetType() const noexcept { return type_; }

  template <class T>
  T &GetValue() {
    return std::get<T>(value_);
  }
  template <class T>
  const T &GetValue() const {
    return std::get<T>(value_);
  }

  template <class T, class... Param>
  void SetValue(Param &&... param) {
    value_ = T(std::forward<Param>(param)...);
  }

  AstNode* GetNode() { return nd_; }
};

template <EntryType T, class... Param>
TableEntry BuildEntry(AstNode* nd, Param&&... param) {
  TableEntry entry(nd, T);
  if constexpr (T == VARIABLE)
    entry.SetValue<VariableType>(std::forward<Param>(param)...);
  else if constexpr (T == TYPE_ALIAS)
    entry.SetValue<DataType>(std::forward<Param>(param)...);
  else if constexpr (T == FUNCTION)
    entry.SetValue<FunctionType>(std::forward<Param>(param)...);
  return entry;
}

#endif  // ENTRY_H_
