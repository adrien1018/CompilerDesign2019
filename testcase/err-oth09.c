float meow () {}
void meow2 () {}

int main() {
  int A[100];
  float B[100];
  int a;
  float b = 0.1;
  A[7.5] = 1;
  A["yeeeeeeee"] = 1;
  A[b] = 1;
  a = A[b];
  A[B[0]] = 1;
  a = A[B[0]];
  a = A[B[0] + 1];
  a = A[meow()];
  a = A[meow() + 1];
  a = A[meow2()];
  a = A[meow2() + 1];
}
