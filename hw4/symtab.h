#ifndef SYMTAB_H_
#define SYMTAB_H_
/**
 * Symbol map class, assigning ids (position) to each inserted symbol with
 *   type std::basic_string<T>.
 *
 * Member functions: (complexity is O(1) if not specified)
 *
 * SymbolMap();
 *   Initialize an empty symbol table.
 *
 * void PushScope();
 *   Open a new scope.
 *   Complexity: amortized O(1)
 *
 * void PopScope();
 *   Close the current scope.
 *   Has no effect if the current scope is global scope.
 *   Complexity: amortized O(#keys in the current scope)
 *
 * size_t Query(const KeyType& name) const;
 *   Query a symbol with key `name` at the current position.
 *   Returns the ID of the symbol. If there's no symbol with key `name` at the
 *     queried position, returns SymbolMap::npos.
 *   Complexity: expected O(1)
 *
 * size_t QueryScope(const KeyType& name) const;
 *   Similar to `Query`, except that it returns SymbolMap::npos when the symbol
 *     cannot be found in the current scope.
 *   Complexity: expected O(1)
 *
 * std::pair<std::pair<size_t, StringRef>, bool> Insert(const KeyType& name);
 *   Insert a symbol with key `name` into the current position.
 *   If a symbol with key `name` exists in the current scope, it returns
 *     {{[id of the existing symbol], [string of the symbol]}, false}.
 *     Otherwise, it returns
 *     {{[id of the inserted symbol], [string of the symbol]}, true}.
 *   The string is returned as StringRef, which contains a pointer to the string
 *     pool inside the SymbolMap class. If the SymbolMap is destructed or
 *     cleared, the pointer would be invalidated.
 *   Complexity: amortized + expected O(1)
 *
 * size_t GetScopeDepth(size_t id) const;
 * size_t GetScopeDepth() const;
 *   Get the scope depth of the given symbol / current position.
 *   If the given symbol does not exist, the behavior is undefined.
 *
 * size_t GetPosition() const;
 *   Get the current position (which is the ID of the previously inserted
 *     symbol).
 *
 * void Clear();
 *   Clear the symbol table.
 *   Complexity: O(#symbols inserted)
 */

#include <string>
#include <vector>
#include <algorithm>
#include <forward_list>
#include <unordered_map>
#include <unordered_set>
#include <bits/hash_bytes.h> // byte hashing hack

template <class T> class SymbolMap {
 public:
  typedef std::basic_string<T> KeyType;
  struct StringRef {
    const T* ptr_;
    size_t size_;
    StringRef(const T* ptr, size_t sz) : ptr_(ptr), size_(sz) {}
    StringRef(const KeyType& str) : ptr_(str.data()), size_(str.size()) {}
   public:
    StringRef() {}
    operator KeyType() const { return KeyType(ptr_, size_); }
    bool operator==(const StringRef& str) const {
      return size_ == str.size_ && std::equal(ptr_, ptr_ + size_, str.ptr_);
    }
    bool operator!=(const StringRef& str) const { return !operator==(str); }
    friend class StringRefHash;
  };

 private:
  static const size_t kBlockSize_ = 1024;
  struct StringRefHash_ {
    size_t operator()(const StringRef& str) const {
      return std::_Hash_bytes(str.ptr_, sizeof(T) * str.size_, 0xc70f6907UL);
    }
  };
  std::unordered_map<StringRef, std::vector<size_t>, StringRefHash_> map_;
  std::vector<size_t> scope_;
  std::vector<std::unordered_set<typename decltype(map_)::pointer>> scope_lst_;
  std::forward_list<std::vector<T>> str_pool_;
  StringRef InsertPool_(const KeyType& str) {
    if (!str_pool_.empty() &&
        str_pool_.front().size() + str.size() <= kBlockSize_) {
      auto& front = str_pool_.front();
      StringRef ret(front.data() + front.size(), str.size());
      front.insert(front.end(), str.begin(), str.end());
      return ret;
    } else {
      str_pool_.emplace_front(str.begin(), str.end());
      auto& front = str_pool_.front();
      front.reserve(kBlockSize_);
      return StringRef(front.data(), str.size());
    }
  }

 public:
  static const size_t npos;
  void PushScope() { scope_lst_.emplace_back(); }
  void PopScope() {
    if (scope_lst_.empty()) return;
    for (auto& i : scope_lst_.back()) i->second.pop_back();
    scope_lst_.pop_back();
  }

  size_t Query(const KeyType& name) const {
    auto it = map_.find(StringRef(name));
    if (it == map_.end()) return npos;
    return it->second.back();
  }
  size_t QueryScope(const KeyType& name) const {
    auto it = map_.find(StringRef(name));
    if (it == map_.end() || scope_[it->second.back()] != scope_lst_.size()) {
      return npos;
    }
    return it->second.back();
  }
  std::pair<std::pair<size_t, StringRef>, bool> Insert(const KeyType& name) {
    size_t x = scope_.size();
    auto it = map_.emplace(StringRef(name), std::vector<size_t>());
    if (!it.second && scope_[it.first->second.back()] == scope_lst_.size()) {
      return {{it.first->second.back(), it.first->first}, false};
    }
    *const_cast<StringRef*>(&it.first->first) = InsertPool_(name); // map hack
    scope_.push_back(scope_lst_.size());
    it.first->second.push_back(x);
    if (scope_lst_.size()) scope_lst_.back().insert(&*it.first);
    return {{x, it.first->first}, true};
  }
  size_t GetScopeDepth(size_t id) const { return scope_[id]; }
  size_t GetScopeDepth() const { return scope_lst_.size(); }
  size_t GetPosition() const { return scope_.size() - 1; }
  void ClearMap() {
    map_.clear();
    scope_.clear();
    scope_lst_.clear();
  }
  void Clear() {
    map_.clear();
    scope_.clear();
    scope_lst_.clear();
    str_pool_.clear();
  }
};

template <class T>
const size_t SymbolMap<T>::npos = (size_t)-1;

#endif  // SYMTAB_H_
