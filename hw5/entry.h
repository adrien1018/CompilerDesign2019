#ifndef ENTRY_H_
#define ENTRY_H_

#include <variant>
#include <vector>

#include "ast.h"

struct TypeAttr {
  DataType data_type;
  std::vector<size_t> dims;

  TypeAttr() = default;
  TypeAttr(DataType type) : data_type(type) {}

  template <class V>
  TypeAttr(DataType type, V&& dim)
      : data_type(type), dims(std::forward<V>(dim)) {}

  bool IsArray() const { return !dims.empty(); }
  bool operator==(const TypeAttr& rhs) const {
    return data_type == rhs.data_type && dims == rhs.dims;
  }
};

struct VariableAttr {
  DataType data_type;
  std::vector<size_t> dims;
  size_t size;
  int32_t offset;

  VariableAttr() = default;
  VariableAttr(DataType type) : data_type(type), size(4), offset(0) {}

  template <class V>
  VariableAttr(DataType type, V&& dim)
      : data_type(type), dims(std::forward<V>(dim)), offset(0) {
    size = 4;
    for (size_t d : dims) size *= d;
  }

  VariableAttr(const TypeAttr& rhs)
      : data_type(rhs.data_type), dims(rhs.dims), offset(0) {
    size = 4;
    for (size_t d : dims) size *= d;
  }

  template <class Iterator>
  VariableAttr(DataType type, Iterator bg, Iterator ed)
      : data_type(type), dims(bg, ed) {
    size = 4;
    for (size_t d : dims) size *= d;
  }

  bool IsArray() const noexcept { return !dims.empty(); }
  size_t GetDimension() const noexcept { return dims.size(); }
  DataType GetType() const noexcept { return data_type; }

  VariableAttr Slice(size_t dim) const noexcept {
    return VariableAttr(data_type, dims.begin() + dim, dims.end());
  }

  bool operator==(const VariableAttr& rhs) const {
    return data_type == rhs.data_type && dims == rhs.dims;
  }
  bool operator!=(const VariableAttr& rhs) const { return !operator==(rhs); }
};

struct FunctionAttr {
  DataType return_type;
  std::vector<VariableAttr> params;
  size_t fp_offset, sp_offset;
  // fp_offset: fp = old_sp - fp_offset
  // sp_offset: sp = old_sp - sp_offset

  FunctionAttr() = default;

  template <class V>
  FunctionAttr(DataType type, V&& params_)
      : return_type(type), params(std::forward<V>(params_)) {
    fp_offset = 8;
    for (auto& v : params) {
      v.offset = fp_offset + 8;
      if (v.IsArray()) {
        // pointer type
        fp_offset += 8;
      } else {
        // int or float
        fp_offset += 4;
      }
    }
    sp_offset = fp_offset + 8;  // old fp
  }

  DataType GetReturnType() const noexcept { return return_type; }
  size_t NumParam() const noexcept { return params.size(); }
};

enum EntryType { VARIABLE, FUNCTION, TYPE_ALIAS };

class TableEntry {
 private:
  AstNode* nd_;
  EntryType type_;
  std::variant<VariableAttr, TypeAttr, FunctionAttr> value_;

 public:
  TableEntry() = default;
  TableEntry(AstNode* nd, EntryType type) : nd_(nd), type_(type) {}
  EntryType GetType() const noexcept { return type_; }

  template <class T>
  T& GetValue() {
    return std::get<T>(value_);
  }
  template <class T>
  const T& GetValue() const {
    return std::get<T>(value_);
  }

  template <class T, class... Param>
  void SetValue(Param&&... param) {
    value_ = T(std::forward<Param>(param)...);
  }

  AstNode* GetNode() const { return nd_; }
};

template <EntryType T, class... Param>
TableEntry BuildEntry(AstNode* nd, Param&&... param) {
  TableEntry entry(nd, T);
  if constexpr (T == VARIABLE)
    entry.SetValue<VariableAttr>(std::forward<Param>(param)...);
  else if constexpr (T == TYPE_ALIAS)
    entry.SetValue<TypeAttr>(std::forward<Param>(param)...);
  else if constexpr (T == FUNCTION)
    entry.SetValue<FunctionAttr>(std::forward<Param>(param)...);
  return entry;
}

#endif  // ENTRY_H_
