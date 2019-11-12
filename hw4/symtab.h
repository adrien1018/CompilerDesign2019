#ifndef SYMTAB_H_
#define SYMTAB_H_

#include <vector>
#include <unordered_map>

template <class EntryType> class SymTab {
 public:
  struct Position {
    size_t id, scope;
  };
  struct Entry {
    const Position pos;
    EntryType val;
  };
 private:
  struct Node_ {
    Node_* parent;
    int depth, refcount;
    Position pos;
    std::unordered_map<std::string, Entry> map;
    Node_(Position pos) : parent(nullptr), depth(0),
        refcount(0), pos(pos) {}
    Node_(Node_* nxt, Position pos) : parent(nxt), depth(nxt->depth + 1),
        refcount(0), pos(pos) {
      parent->refcount++;
    }
    ~Node_() { if (parent) parent->refcount--; }
  };
  std::vector<Node_*> scopes_;
  Position cur_pos_;
  Node_* CurNode_() const { return scopes_[cur_pos_.scope]; }

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

  SymTab() : cur_pos_{0, 0} {
    scopes_.emplace_back(new Node_(cur_pos_));
  }
  ~SymTab() {
    for (; scopes_.size(); scopes_.pop_back()) delete scopes_.back();
  }
  void PushScope() {
    Node_* tmp = CurNode_();
    cur_pos_ = {cur_pos_.id + 1, scopes_.size()};
    scopes_.emplace_back(new Node_(tmp, cur_pos_));
  }
  void PopScope() {
    cur_pos_ = {cur_pos_.id + 1, CurNode_()->parent->pos.scope};
  }
  EntryPtr Query(const std::string& name) {
    for (Node_* nd = CurNode_(); nd; nd = nd->parent) {
      auto it = nd->map.find(name);
      if (it != nd->map.end()) return EntryPtr(&*it);
    }
    return EntryPtr();
  }
  EntryPtr QueryScope(const std::string& name) {
    auto it = CurNode_()->map.find(name);
    if (it != CurNode_()->map.end()) return EntryPtr(&*it);
    return EntryPtr();
  }
  EntryPtr Insert(const std::string& name, const EntryType& val) {
    cur_pos_.id++;
    auto it = CurNode_()->map.insert({name, {cur_pos_, val}});
    if (!it.second) return EntryPtr();
    return EntryPtr(&*it.first);
  }
  EntryPtr Insert(const std::string& name, EntryType&& val) {
    cur_pos_.id++;
    auto it = CurNode_()->map.insert({name, {cur_pos_, std::move(val)}});
    if (!it.second) return EntryPtr();
    return EntryPtr(&*it.first);
  }
  void MoveToScope(size_t scope) {
    cur_pos_ = scopes_[scope]->pos;
  }
  void MoveToPosition(Position pos) {
    cur_pos_ = pos;
  }
  Position GetPosition() const { return cur_pos_; }
};

#endif
