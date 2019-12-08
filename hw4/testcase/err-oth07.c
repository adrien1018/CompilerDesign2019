void fn(int a, int x[][1000000]) {}

int a[1000][1000000];

int main() {
  int b = a[1];
  int c = a;
  int d = a[1][1][1];
  int e = a[0.5][1];
  fn(c, a[0][0]);
  fn(c, a[0]);
  fn(c, a);
  fn(c, c[0][0]);
}
