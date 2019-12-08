int func1(int a[][10]) {}

int func2(int a) {}

int main() {
  int i, j, k, a[10][10][10];
  int x[10], b[10][10][10];
  i = func1(a[0][1]);
  j = func1(a[0][1][1]);
  k = func2(a[0][1][1]);
  i = func1(x[j][5]);
  k = func1(b[1]);

  i = func1(a[0][1] + 1);
  j = func1(a[0][1][1] + 1);
  k = func2(a[0][1][1] + 1);
  i = func1(x[j][5] + 1);
  k = func1(b[1] + 1);

  return 0;
}
