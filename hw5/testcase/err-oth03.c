void fn() {}

void fn2(int a) {}

int fn3() { return 0.5; }

int main() {
  fn(1);
  fn2(1, 1);
  fn2();
}
