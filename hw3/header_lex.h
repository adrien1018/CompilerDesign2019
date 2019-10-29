#include <map>
#include <string>
#include <vector>

struct Symbol {
  int line;
  int counter;
};

using SymTab = std::map<std::string, Symbol>;

void PrintStrings(const std::vector<std::string>&);
void PrintSymTab(const SymTab&);
