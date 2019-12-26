int f1() {
  return 8.7;
  return;
}

float f2() {
  int a, b;
  float c, d;
  return 87;
  return a + b;
  return;
  return a + c; /* OK */
  return c + d; /* OK */
}

void f3() {
  int a, b;
  return 8.7;
  return a + b;
  return; /* OK */
}
