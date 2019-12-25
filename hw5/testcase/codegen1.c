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
    float a[5], b = 0.5;
    int i = 1;
    a[0] = 1;
    b = 0.7;
    while (i < 5) {
      a[i] = a[i - 1] * b;
      write(a[i]);
      write(" ");
      i = i + 1;
    }
    write("\n");
  }
}

int PassArray1(int c[]) {
  return c[1];
}

int PassArray2(int c[][4]) {
  return PassArray1(c[1]);
}

int PassArray3(int c[4][4][4]) {
  return PassArray2(c[2]);
}

float PassArray4(float c[1]) {
  return c[3];
}

void PassArray() {
  int a[5][4][4];
  float b[5];
  int i = 0;
  while (i < 5) {
    int j = 0;
    b[i] = i * 0.1;
    while (j < 4) {
      int k = 0;
      while (k < 4) {
        a[i][j][k] = i * 16 + j * 4 + k;
        k = k + 1;
      }
      j = j + 1;
    }
    i = i + 1;
  }
  write(PassArray1(a[1][2])); write(" ");
  write(PassArray2(a[3])); write(" ");
  write(PassArray3(a)); write(" ");
  write(PassArray4(b)); write("\n");
}

void PrintLargeNum() {
  int a = 100000;
  write(a);
  write("\n");
}

void ManyArguments(int i1, float f1, int i2, float f2, int i3, float f3,
                   int i4, float f4, int i5, float f5, int i6, float f6,
                   int i7, float f7, int i8, float f8, int i9, float f9) {
  write(i1); write(" "); write(f1); write(" ");
  write(i2); write(" "); write(f2); write(" ");
  write(i3); write(" "); write(f3); write("\n");
  write(i4); write(" "); write(f4); write(" ");
  write(i5); write(" "); write(f5); write(" ");
  write(i6); write(" "); write(f6); write("\n");
  write(i7); write(" "); write(f7); write(" ");
  write(i8); write(" "); write(f8); write(" ");
  write(i9); write(" "); write(f9); write("\n");
}

int glob_1 = 3;
int glob_2 = !((4 + 5 - 6 * 7) && 0);
float glob_3 = 3.5 + 7;
float glob_4 = !4.4 || 5;
int glob_5;
int glob_arr_1[5];
float glob_arr_2[5][4];

int ReadGlobal() {
  int c = glob_1 + glob_2; /* 4 */
  float d = glob_3 + glob_4; /* 11.5 */
  return c + 2 * d + glob_arr_1[3] + glob_arr_2[1][1] + glob_5; /* 27 */
}

int WriteGlobal() {
  glob_4 = glob_4 + 0.5;
  glob_5 = glob_4 * 2 + 3;
  return glob_5;
}

void PassGlobalArray() {
  int i = 0;
  while (i < 5) {
    int j = 0;
    glob_arr_1[i] = i;
    while (j < 4) {
      glob_arr_2[i][j] = (i * 4 + j) * 0.1;
      j = j + 1;
    }
    i = i + 1;
  }
  write(PassArray1(glob_arr_1)); write(" ");
  write(PassArray4(glob_arr_2[1])); write("\n");
}

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
  PassArray(); /* 25 53 37 0.300000 */
  PrintLargeNum(); /* 100000 */
  ManyArguments(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17);
  /* 0 1.000000 2 3.000000 4 5.000000
   * 6 7.000000 8 9.000000 10 11.000000
   * 12 13.000000 14 15.000000 16 17.000000 */
  write(ReadGlobal()); /* 27 */
  write("\n");
  write(WriteGlobal()); /* 6 */
  write("\n");
  PassGlobalArray(); /* 1 0.700000 */
}
