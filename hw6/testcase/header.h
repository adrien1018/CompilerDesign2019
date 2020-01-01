#include <cstdio>

int read() {
  int a;
  scanf("%d", &a);
  return a;
}
float fread() {
  float a;
  scanf("%f", &a);
  return a;
}
void write(const char* x) {
  fputs(x, stdout);
}
void write(int x) {
  printf("%d", x);
}
void write(float x) {
  printf("%f", x);
}
