int A(int a) {
  return a;
}

int B() {
  int c = 3;
  int a = A(3) + A(c);
  a = a + A(3, 3);
  a = A(c, c);
  a = A();
}
