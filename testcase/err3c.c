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
  int z[2][3][5];
  int a[4][5];
  float c[3][2];
  int b = 3;
  b = b + A(a); /* ERR */
  b = b + B(a); /* OK 3c */
  b = b + B(z); /* OK 3c */
  b = b + C(a); /* OK 3c */
  b = b + A(a[2]); /* ERR */
  b = b + B(a[3]); /* OK 3c */
  b = b + C(a[1]); /* OK */
  b = b + A(a[1][1]); /* OK */
  b = b + B(a[1][1]); /* ERR */
  b = b + C(a[1][1]); /* ERR */
  b = b + C(c[1]); /* ERR 3d */
  b = b + C(3);
  b = b + C(3.4);
  b = b + C(3 + b);
  b = b + C(c[0][1]);
  b = b + C(c[0][0] + b);
  b = b + C(c[0][0] + c[0][1]);
  return b;
}
