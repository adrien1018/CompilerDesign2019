int IntFunction() {
  return 3;
}

float FloatFunction() {
  return 4.5;
}

void IntOperation(int a, int b) {
  write(a + b); write(" ");
  write(a - b); write(" ");
  write(a * b); write(" ");
  write(a / b); write(" ");
  write(a && b); write(" ");
  write(a || b); write(" ");
  write(!a); write(" ");
  write(-a); write("\n");
}

void FloatOperation(float a, float b) {
  write(a + b); write(" ");
  write(a - b); write(" ");
  write(a * b); write(" ");
  write(a / b); write("\n");
  write(a && b); write(" ");
  write(a || b); write(" ");
  write(!a); write(" ");
  write(-a); write("\n");
}

int Convert(float a, int b) {
  int c = a;
  float d = b;
  int e = a + 5;
  return c + d + e;
}

void IntCompare(int a, int b) {
  write(a < b); write(" ");
  write(a <= b); write(" ");
  write(a > b); write(" ");
  write(a >= b); write(" ");
  write(a == b); write(" ");
  write(a != b); write("\n");
}

void FloatCompare(float a, float b) {
  write(a < b); write(" ");
  write(a <= b); write(" ");
  write(a > b); write(" ");
  write(a >= b); write(" ");
  write(a == b); write(" ");
  write(a != b); write("\n");
}

int IfStatement() {
  int a = 2, b = 3, c = 0;
  if (a < b) { c = c + 2; }
  if (a > b) { c = c + 1; }
  return c;
}

int IfElseStatement() {
  int a = 2, b = 3, c = 0;
  if (a < b) {
    c = c + 8;
  } else {
    c = c + 4;
  }
  if (a > b) {
    c = c + 2;
  } else {
    c = c + 1;
  }
  return c;
}

int WhileStatement() {
  int c = 1048576, a = 0;
  while (c) {
    c = c / 2;
    a = a + 1;
  }
  return a;
}

void LocalArray() {
  if (1) {
    int a[8][8];
    int i, j;
    i = 0;
    while (i < 8) {
      a[0][i] = 0;
      i = i + 1;
    }
    a[0][0] = 1;
    i = 1;
    while (i < 8) {
      j = 1;
      a[i][0] = 1;
      while (j < 8) {
        a[i][j] = a[i - 1][j] + a[i - 1][j - 1];
        j = j + 1;
      }
      i = i + 1;
    }
    i = 0;
    while (i < 8) {
      j = 0;
      while (j <= i) {
        write(a[i][j]);
        write(" ");
        j = j + 1;
      }
      write("\n");
      i = i + 1;
    }
  }
  if (1) {
    float a[5];
    int i = 1;
    a[0] = 1;
    while (i < 5) {
      a[i] = a[i - 1] * 0.7;
      write(a[i]);
      write(" ");
      i = i + 1;
    }
    write("\n");
  }
}

int glob_1 = 3;
/* int glob_2 = 4 + 5; */

int main() {
  write(IntFunction()); /* 3 */
  write("\n");
  write(FloatFunction()); /* 4.5 */
  write("\n");
  IntOperation(100, 3); /* 103 97 300 33 1 1 0 -100 */
  IntOperation(0, 1);   /* 1 -1 0 0 0 1 1 0 */
  IntOperation(1, 0);   /* 1 1 0 -1 0 1 0 -1 */
  FloatOperation(100.0, 3.0); /* 103.000000 97.000000 300.000000 33.333332 */
                              /* 1 1 0 -100.000000 */
  FloatOperation(0.0, 0.0); /* 0.000000 0.000000 0.000000 nan */
                            /* 0 0 1 -0.000000 */
  write(Convert(3.6, 6)); /* 17 */
  write("\n");
  FloatCompare(1.0, 2.0); /* 1 1 0 0 0 1 */
  FloatCompare(1.5, 1.5); /* 0 1 0 1 1 0 */
  write(IfStatement()); /* 2 */
  write("\n");
  write(IfElseStatement()); /* 9 */
  write("\n");
  write(WhileStatement()); /* 21 */
  write("\n");
  LocalArray();
  /* 1
   * 1 1
   * 1 2 1
   * 1 3 3 1
   * 1 4 6 4 1
   * 1 5 10 10 5 1
   * 1 6 15 20 15 6 1
   * 1 7 21 35 35 21 7 1
   * 0.700000 0.490000 0.343000 0.240100 */
}
