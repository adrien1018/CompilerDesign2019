int A(int a) {
  return a;
}
int B(int c[][3][4]) {
  return c[0][1][1];
}
int C(int d[2]) {
  return d[1];
}

int D() {
  int a[4];
  int b = 3;
  b = b + B(a); // OK 3c
  b = b + C(a); // OK 3c
  b = b + A(a); // ERR
  b = b + A(a[2]); // OK
  b = b + B(a[3]); // ERR
  b = b + C(a[1]); // ERR
}
