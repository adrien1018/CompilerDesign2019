#ifndef SYMTAB_H_
#define SYMTAB_H_
/**
 * Persistent symbol table class, preserving mapping from KeyType to EntryType.
 * Allow expressions between declarations.
 *
 * Member functions: (complexity is O(1) if not specified)
 *
 * SymTab();
 *   Initialize an empty symbol table.
 *
 * void PushScope();
 *   Open a new scope.
 *   Increments the current position.
 *   Complexity: amortized O(1)
 *
 * void PopScope();
 *   Close the current scope and revert to its parent scope.
 *   Increments the current position.
 *   Has no effect if the current scope is global scope.
 *   Complexity: amortized O(1)
 *
 * EntryPtr Query(const KeyType& str, size_t pos);
 * EntryPtr Query(const KeyType& str);
 * ConstEntryPtr Query(const KeyType& name, size_t pos) const;
 * ConstEntryPtr Query(const KeyType& name) const;
 *   Query a symbol with key `name` at the given/current position.
 *   Returns a EntryPtr `ptr`. One can dereference it into a EntryType, or query
 *     its key with `ptr.Name()`.
 *   If there's no symbol with key `name` at the queried position, returns a
 *     invalid EntryPtr (`ptr.Valid() == false`). Attempting to dereference an
 *     invalid EntryPtr results in undefined behavior.
 *   Complexity: expected O(scope_depth)
 *
 * EntryPtr QueryScope(const KeyType& str, size_t pos);
 * EntryPtr QueryScope(const KeyType& str);
 * ConstEntryPtr QueryScope(const KeyType& name, size_t pos) const;
 * ConstEntryPtr QueryScope(const KeyType& name) const;
 *   Similar to `Query`, except that it returns an invalid EntryPtr when the
 *     symbol cannot be found in the scope of the queried position.
 *   Complexity: expected O(1)
 *
 * EntryPtr Insert(const KeyType& name, const EntryType& val);
 * EntryPtr Insert(const KeyType& name, EntryType&& val);
 *   Insert a symbol with key `name` and value `val` into the current position.
 *   If a symbol with key `name` exists in the current scope, it returns an
 *     EntryPtr pointing to the symbol. Otherwise, it returns an invalid
 *     EntryPtr and increments the current position.
 *   Complexity: amortized + expected O(1)
 *
 * std::pair<size_t, size_t> ScopeRange(size_t scope) const;
 * std::pair<size_t, size_t> ScopeRange() const;
 *   Get the position range (closed interval) of the given/current scope.
 *
 * size_t GetScope(size_t pos) const;
 * size_t GetScope() const;
 *   Get the scope of the given/current position.
 *
 * size_t GetPosition() const;
 *   Get the current position.
 *
 * size_t ScopeCount() const;
 *   Get the total number of scopes.
 *
 * size_t GetParentScope(size_t scope) const;
 * size_t GetParentScope() const;
 *   Get the parent scope of the given/current scope. Return 0 (global scope) if
 *     the queried scope is the global scope.
 *
 * size_t GetScopeDepth(size_t pos) const;
 * size_t GetScopeDepth() const;
 *   Get the scope depth of the given/current scope.
 */

#include <vector>
#include <unordered_map>

template <class KeyType, class EntryType> class SymTab {
 public:
  struct Entry {
    const size_t pos;
    EntryType val;
  };
 private:
  struct Node_ {
    Node_* parent;
    size_t depth, begin, end;
    std::unordered_map<KeyType, Entry> map;
    Node_(size_t pos) : parent(nullptr), depth(0), begin(pos), end(pos) {}
    Node_(Node_* nxt, size_t pos) : parent(nxt), depth(nxt->depth + 1),
        begin(pos), end(pos) {}
  };
  std::vector<Node_*> scope_;
  std::vector<size_t> scopemap_;
  Node_* CurNode_() const { return scope_[scopemap_.back()]; }

 public:
  class EntryPtr {
    typename std::unordered_map<KeyType, Entry>::pointer it_;
    EntryPtr(decltype(it_) it) : it_(it) {}
   public:
    EntryPtr() : it_(nullptr) {}
    bool Valid() const { return it_; }
    const KeyType& Name() const { return it_->first; }
    Entry& operator*() const { return it_->second; }
    Entry* operator->() const { return &it_->second; }
    bool operator==(const EntryPtr& ptr) const { return ptr.it_ == it_; }
    bool operator!=(const EntryPtr& ptr) const { return ptr.it_ != it_; }
    friend class SymTab<KeyType, EntryType>;
  };
  class ConstEntryPtr {
    typename std::unordered_map<KeyType, Entry>::const_pointer it_;
    ConstEntryPtr(decltype(it_) it) : it_(it) {}
   public:
    ConstEntryPtr() : it_(nullptr) {}
    bool Valid() const { return it_; }
    const KeyType& Name() const { return it_->first; }
    const Entry& operator*() const { return it_->second; }
    const Entry* operator->() const { return &it_->second; }
    bool operator==(const ConstEntryPtr& ptr) const { return ptr.it_ == it_; }
    bool operator!=(const ConstEntryPtr& ptr) const { return ptr.it_ != it_; }
    friend class SymTab<KeyType, EntryType>;
  };

  SymTab() {
    scope_.emplace_back(new Node_(scopemap_.size()));
    scopemap_.push_back(0);
  }
  ~SymTab() {
    for (; scope_.size(); scope_.pop_back()) delete scope_.back();
  }
  void PushScope() {
    Node_* tmp = CurNode_();
    scopemap_.push_back(scope_.size());
    scope_.emplace_back(new Node_(tmp, scopemap_.size() - 1));
  }
  void PopScope() {
    Node_* pa = CurNode_()->parent;
    if (!pa) return;
    pa->end = scopemap_.size();
    scopemap_.push_back(scopemap_[pa->begin]);
  }

  EntryPtr Query(const KeyType& name, size_t pos) {
    if (pos >= scopemap_.size()) return EntryPtr();
    for (Node_* nd = scope_[scopemap_[pos]]; nd; nd = nd->parent) {
      auto it = nd->map.find(name);
      if (it != nd->map.end() && it->second.pos <= pos) return &*it;
    }
    return EntryPtr();
  }
  ConstEntryPtr Query(const KeyType& name, size_t pos) const {
    if (pos >= scopemap_.size()) return ConstEntryPtr();
    for (Node_* nd = scope_[scopemap_[pos]]; nd; nd = nd->parent) {
      auto it = nd->map.find(name);
      if (it != nd->map.end() && it->second.pos <= pos) return &*it;
    }
    return ConstEntryPtr();
  }
  EntryPtr QueryScope(const KeyType& name, size_t pos) {
    if (pos >= scopemap_.size()) return EntryPtr();
    Node_* nd = scope_[scopemap_[pos]];
    auto it = nd->map.find(name);
    if (it != nd->map.end() && it->second.pos <= pos) return &*it;
    return EntryPtr();
  }
  ConstEntryPtr QueryScope(const KeyType& name, size_t pos) const {
    if (pos >= scopemap_.size()) return ConstEntryPtr();
    Node_* nd = scope_[scopemap_[pos]];
    auto it = nd->map.find(name);
    if (it != nd->map.end() && it->second.pos <= pos) return &*it;
    return ConstEntryPtr();
  }
  EntryPtr Query(const KeyType& name) {
    return Query(name, scopemap_.size() - 1);
  }
  ConstEntryPtr Query(const KeyType& name) const {
    return Query(name, scopemap_.size() - 1);
  }
  EntryPtr QueryScope(const KeyType& name) {
    return QueryScope(name, scopemap_.size() - 1);
  }
  ConstEntryPtr QueryScope(const KeyType& name) const {
    return QueryScope(name, scopemap_.size() - 1);
  }

  EntryPtr Insert(const KeyType& name, const EntryType& val) {
    Node_* ptr = CurNode_();
    auto it = ptr->map.insert({name, {scopemap_.size(), val}});
    if (!it.second) return EntryPtr(&*it.first);
    ptr->end = scopemap_.size();
    scopemap_.push_back(scopemap_.back());
    return EntryPtr();
  }
  EntryPtr Insert(const KeyType& name, EntryType&& val) {
    Node_* ptr = CurNode_();
    auto it = ptr->map.insert({name, {scopemap_.size(), std::move(val)}});
    if (!it.second) return EntryPtr(&*it.first);
    ptr->end = scopemap_.size();
    scopemap_.push_back(scopemap_.back());
    return EntryPtr();
  }
  std::pair<size_t, size_t> ScopeRange(size_t scope) const {
    return {scope_[scope]->begin, scope_[scope]->end};
  }
  std::pair<size_t, size_t> ScopeRange() const {
    return {scope_.back()->begin, scope_.back()->end};
  }
  size_t GetScope(size_t pos) const { return scopemap_[pos]; }
  size_t GetScope() const { return scopemap_.back(); }
  size_t GetPosition() const { return scopemap_.size() - 1; }
  size_t ScopeCount() const { return scope_.size(); }
  size_t GetParentScope(size_t scope) const {
    if (!scope_[scope]->parent) return 0;
    return scopemap_[scope_[scope]->parent->begin];
  }
  size_t GetParentScope() const { return GetParentScope(GetScope()); }
  size_t GetScopeDepth(size_t scope) const { return scope_[scope]->depth; }
  size_t GetScopeDepth() const { return GetScopeDepth(GetScope()); }
};

#endif
