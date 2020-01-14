int A() {
  int c[3];
  float a = 1;
  return c[a];
}

float Bx() {
  return 1;
}

int B() {
  int c[3];
  float a = 1;
  return c[Bx()];
}

int nx() {
  return 1;
}

int nothing() {
  int c[3];
  float a = 1;
  return c[nx()];
}
