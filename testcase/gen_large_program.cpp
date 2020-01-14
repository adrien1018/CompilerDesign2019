#include <cstdio>
#include <string>
#include <random>
#include <vector>

using mrand = std::uniform_int_distribution<int>;
std::mt19937_64 gen;
std::vector<std::string> stk;
const char kChr[] = "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM";
const char kOp[][3] = {"+", "-", "*", "/", "<", ">", "<=", ">=", "!=", "=="};
const char kType[][6] = {"int", "float", "void"};
const int kS = sizeof(kChr) - 1;

std::string GenRandStr() {
  int Q = mrand(0, 100000000)(gen);
  std::string str;
  while (Q >= kS) {
    Q -= kS;
    str.push_back(kChr[Q % kS]);
    Q /= kS;
  }
  str.push_back(kChr[Q]);
  return str;
}

void Var() {
  printf("%s", stk[mrand(0, stk.size() - 1)(gen)].c_str());
}
void Op() {
  if (mrand(0, 50)(gen) < 25) {
    if (mrand(0, 3)(gen)) {
      Op();
    } else {
      putchar('('); Op(); putchar(')');
    }
    printf(" %s ", kOp[mrand(0, 9)(gen)]);
    if (mrand(0, 3)(gen)) {
      Op();
    } else {
      putchar('('); Op(); putchar(')');
    }
  } else {
    if (stk.empty() || (gen() & 1)) {
      printf("%d", mrand(-1000000, 1000000)(gen));
    } else {
      Var();
    }
  }
}
void Decl() {
  std::string str = GenRandStr();
  if (gen() & 1) {
    printf("%s %s;\n", kType[gen() & 1], str.c_str());
  } else {
    printf("%s %s = ", kType[gen() & 1], str.c_str());
    Op();
    puts(";");
  }
  stk.push_back(str);
}
void Assign() {
  if (stk.empty()) return;
  Var(); printf(" = "); Op(); puts(";");
}

void Block() {
  size_t now = stk.size();
  int sz = mrand(0, 400)(gen);
  for (int i = 0; i < sz; i++) Decl();
  sz = mrand(0, 400)(gen);
  for (int i = 0; i < sz; i++) Assign();
  stk.resize(now);
}

void Func() {
  int type = mrand(0, 2)(gen);
  printf("%s %s() {\n", kType[type], GenRandStr().c_str());
  Block();
  if (type == 2) puts("return;");
  else printf("return "), Op(), puts(";");
  puts("}");
}

int main() {
  for (int i = 0; i < 100; i++) Func();
}
