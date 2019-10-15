#include <string>
#include <vector>
#include <unordered_map>

struct Symbol {
  int line;
  int counter;
};

using SymTab = std::unordered_map<std::string, Symbol>;

void PrintStrings(const std::vector<std::string>&);
void PrintSymTab(const SymTab&);
