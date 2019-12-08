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

void C() {
  typedef int a;
  int a;
}

int C() {}
