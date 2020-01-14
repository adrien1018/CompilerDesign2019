#include <cstdio>

constexpr int kNumInsr = 1 << 21;
int main() {
  printf("int main() {\n");
  printf("int x;\n");
  printf("x = read();\n");
  printf("if (x == 2) {\n");
  for (int i = 0; i < kNumInsr; ++i) {
    printf("x = x + 1;\n");
  }
  printf("}\n");
  printf("}\n");
}
