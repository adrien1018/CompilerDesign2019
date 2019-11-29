#ifndef SYMTAB_H_
#define SYMTAB_H_
/**
 * Symbol map class, assigning ids (position) to each inserted symbol with
 *   type KeyType.
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
 * std::pair<size_t, bool> Insert(const KeyType& name);
 *   Insert a symbol with key `name` into the current position.
 *   If a symbol with key `name` exists in the current scope, it returns
 *     std::make_pair([id of the existing symbol], false). Otherwise, it returns
 *     std::make_pair([id of the inserted symbol], true).
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
 */

#include <unordered_map>
#include <unordered_set>
#include <vector>

template <class KeyType> class SymbolMap {
 private:
  std::unordered_map<std::string, std::vector<size_t>> map_;
  std::vector<size_t> scope_;
  std::vector<std::unordered_set<typename decltype(map_)::pointer>> scope_lst_;

 public:
  static const size_t npos;
  void PushScope() {
    scope_lst_.emplace_back();
  }
  void PopScope() {
    if (scope_lst_.empty()) return;
    for (auto& i : scope_lst_.back()) {
      i->second.pop_back();
      if (i->second.empty()) map_.erase(i->first);
    }
    scope_lst_.pop_back();
  }

  size_t Query(const KeyType& name) const {
    auto it = map_.find(name);
    if (it == map_.end()) return npos;
    return it->second.back();
  }
  size_t QueryScope(const KeyType& name) const {
    auto it = map_.find(name);
    if (it == map_.end() || scope_[it->second.back()] != scope_lst_.size()) {
      return npos;
    }
    return it->second.back();
  }
  std::pair<size_t, bool> Insert(const KeyType& name) {
    size_t x = scope_.size();
    auto it = map_.emplace(name, std::vector<size_t>());
    if (!it.second && scope_[it.first->second.back()] == scope_lst_.size()) {
      return {it.first->second.back(), false};
    }
    scope_.push_back(scope_lst_.size());
    it.first->second.push_back(x);
    if (scope_lst_.size()) scope_lst_.back().insert(&*it.first);
    return {x, true};
  }
  size_t GetScopeDepth(size_t id) const { return scope_[id]; }
  size_t GetScopeDepth() const { return scope_lst_.size(); }
  size_t GetPosition() const { return scope_.size() - 1; }
};

template <class KeyType>
const size_t SymbolMap<KeyType>::npos = (size_t)-1;

#endif  // SYMTAB_H_
