#ifndef SYMTAB_H_
#define SYMTAB_H_

#include <vector>
#include <unordered_map>

template <class EntryType> class SymTab {
 public:
  struct Entry {
    const size_t pos;
    EntryType val;
  };
 private:
  struct Node_ {
    Node_* parent;
    size_t depth, begin, end;
    std::unordered_map<std::string, Entry> map;
    Node_(size_t pos) : parent(nullptr), depth(0), begin(pos), end(pos) {}
    Node_(Node_* nxt, size_t pos) : parent(nxt), depth(nxt->depth + 1),
        begin(pos), end(pos) {}
  };
  std::vector<Node_*> scopes_;
  std::vector<size_t> scopemap_;
  size_t cur_;
  Node_* CurNode_() const { return scopes_[scopemap_[cur_]]; }

 public:
  class EntryPtr {
    typename std::unordered_map<std::string, Entry>::pointer it_;
    EntryPtr(decltype(it_) it) : it_(it) {}
   public:
    EntryPtr() : it_(nullptr) {}
    bool Valid() const { return it_; }
    const std::string& Name() const { return it_->first; }
    Entry& operator*() const { return it_->second; }
    Entry* operator->() const { return &it_->second; }
    bool operator==(const EntryPtr& ptr) const { return ptr.it_ == it_; }
    bool operator!=(const EntryPtr& ptr) const { return ptr.it_ != it_; }
    friend class SymTab<EntryType>;
  };
  class ConstEntryPtr {
    typename std::unordered_map<std::string, Entry>::const_pointer it_;
    ConstEntryPtr(decltype(it_) it) : it_(it) {}
   public:
    ConstEntryPtr() : it_(nullptr) {}
    bool Valid() const { return it_; }
    const std::string& Name() const { return it_->first; }
    const Entry& operator*() const { return it_->second; }
    const Entry* operator->() const { return &it_->second; }
    bool operator==(const ConstEntryPtr& ptr) const { return ptr.it_ == it_; }
    bool operator!=(const ConstEntryPtr& ptr) const { return ptr.it_ != it_; }
    friend class SymTab<EntryType>;
  };

  SymTab() : cur_(0) {
    scopemap_.push_back(0);
    scopes_.emplace_back(new Node_(cur_));
  }
  ~SymTab() {
    for (; scopes_.size(); scopes_.pop_back()) delete scopes_.back();
  }
  void PushScope() {
    Node_* tmp = CurNode_();
    scopemap_.push_back(scopes_.size());
    scopes_.emplace_back(new Node_(tmp, ++cur_));
  }
  void PopScope() {
    Node_* pa = CurNode_()->parent;
    if (!pa) return;
    scopemap_.push_back(scopemap_[pa->begin]);
    pa->end = ++cur_;
  }
  EntryPtr Query(const std::string& name) {
    for (Node_* nd = CurNode_(); nd; nd = nd->parent) {
      auto it = nd->map.find(name);
      if (it != nd->map.end() && it->second.pos <= cur_) return &*it;
    }
    return EntryPtr();
  }
  ConstEntryPtr Query(const std::string& name) const {
    for (Node_* nd = CurNode_(); nd; nd = nd->parent) {
      auto it = nd->map.find(name);
      if (it != nd->map.end() && it->second.pos <= cur_) return &*it;
    }
    return ConstEntryPtr();
  }
  EntryPtr QueryScope(const std::string& name) {
    auto it = CurNode_()->map.find(name);
    if (it != CurNode_()->map.end() && it->second.pos <= cur_) return &*it;
    return EntryPtr();
  }
  ConstEntryPtr QueryScope(const std::string& name) const {
    auto it = CurNode_()->map.find(name);
    if (it != CurNode_()->map.end() && it->second.pos <= cur_) return &*it;
    return ConstEntryPtr();
  }
  EntryPtr Insert(const std::string& name, const EntryType& val) {
    if (scopemap_.size() != cur_ + 1) return EntryPtr();
    Node_* ptr = CurNode_();
    auto it = ptr->map.insert({name, {cur_ + 1, val}});
    if (!it.second) return EntryPtr();
    scopemap_.push_back(scopemap_.back());
    ptr->end = ++cur_;
    return EntryPtr(&*it.first);
  }
  EntryPtr Insert(const std::string& name, EntryType&& val) {
    if (scopemap_.size() != cur_ + 1) return EntryPtr();
    Node_* ptr = CurNode_();
    auto it = ptr->map.insert({name, {cur_ + 1, std::move(val)}});
    if (!it.second) return EntryPtr();
    scopemap_.push_back(scopemap_.back());
    ptr->end = ++cur_;
    return EntryPtr(&*it.first);
  }
  void MoveToScopeStart() { cur_ = CurNode_()->begin; }
  void MoveToScopeEnd() { cur_ = CurNode_()->end; }
  void MoveToScopeStart(size_t scope) {
    if (scope >= scopes_.size()) return;
    cur_ = scopes_[scope]->begin;
  }
  void MoveToScopeEnd(size_t scope) {
    if (scope >= scopes_.size()) return;
    cur_ = scopes_[scope]->end;
  }
  void MoveToPosition(size_t pos) {
    if (pos >= scopemap_.size()) return;
    cur_ = pos;
  }
  size_t GetPosition() const { return cur_; }
  size_t GetScope() const { return scopemap_[cur_]; }
  size_t ScopeCount() const { return scopes_.size(); }
  size_t PositionCount() const { return scopemap_.size(); }
  size_t GetParentScope() const {
    if (!CurNode_()->parent) return 0;
    return scopemap_[CurNode_()->parent->begin];
  }
  size_t GetScopeDepth() const { return CurNode_()->depth; }
};

#endif
