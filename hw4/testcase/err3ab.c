void A() {
  int a[3][5][6], b;
  b = a[2][3][4];
  b = a[2][3];
  b = a[2][3][4][5];
}

int B() {
  int c[3];
  float a = 4;
  return c[a];
}
