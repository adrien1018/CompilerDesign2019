#include <cstdio>

#include "header.h"

using SymTab = std::unordered_map<std::string, Symbol>;

void PrintStrings(const std::vector<std::string>& vec) {
  for (const std::string& i : vec) puts(i.c_str());
}

void PrintSym(const SymTab::value_type& ent) {
  printf("%s\t%d\n", ent.first.c_str(), ent.second.counter);
}

void PrintSymTab(const SymTab& symtab) {
  puts("\nFrequency of identifiers:");
  int x = 0;
  for (const auto& i : symtab) PrintSym(i);
}
