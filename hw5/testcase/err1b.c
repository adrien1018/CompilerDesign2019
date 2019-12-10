int A() {
  int a = 3;
  int a = 4;
  float a = 5;
  return a;
}

void B() {
  int a;
  typedef int a;
}

void B() {}

void B(int a) {}

void C() {
  typedef int a;
  int a;
}

int C() {}

void D(int a, int a) {}

void E() {
  typedef int a;
  typedef float a;
}

int E;

void F() {
  int a;
  int a[3];
}

void nothing() {
  typedef int a;
  typedef int a;
  typedef a a;
}
