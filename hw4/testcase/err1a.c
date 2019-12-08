int A() {
  int c = 2;
  a = c + 2;
  c = c + 1;
  c = a + 3;
  return c;
}

int B() {
  int c, s = 0;
  for (c = 0; c < 10; c = c + 1) {
    if (c == 3) {
      int t = c + 1;
      s = s + t;
      c = c + a + t;
    }
  }
  return s;
}

void C() {
  typedef int a;
  typedef a c;
  typedef b d;
}

void D() {
  int a;
  if (1) {
    typedef a e;
  }
}

void E() {
  AA(1,
     1);
}

void F() {
  int a;
  a();
}

int Gx() {
  return 1;
}

int G() {
  int c[3];
  float a = Gx;
  return c[1];
}
