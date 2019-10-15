#include <cstdio>

#include "header.h"

void PrintStrings(const std::vector<std::string>& vec) {
  for (const std::string& i : vec) puts(i.c_str());
}

void PrintSym(const SymTab::value_type& ent) {
  printf("%s\t%d\n", ent.first.c_str(), ent.second.counter);
}

void PrintSymTab(const SymTab& symtab) {
  puts("\nFrequency of identifiers:");
  for (const auto& i : symtab) PrintSym(i);
}
