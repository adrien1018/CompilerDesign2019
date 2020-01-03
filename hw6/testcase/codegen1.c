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
  if (a < b, a > b) { c = c + 1; }
  return c;
}

int IfElseStatement() {
  int a = 2, b = 3, c = 0;
  if (a < b) {
    c = c + 8;
  } else {
    c = c + 4;
  }
  if (a < b, a > b) {
    c = c + 2;
  } else {
    c = c + 1;
  }
  return c;
}

void WhileStatement() {
  int c = 1048576, d = 1048576, a = 0;
  while (d) {
    d = d / 2;
    a = a + 1;
  }
  while (write("a"), c) {
    c = c / 2;
    a = a + 1;
  }
  write(" "); write(a); write("\n");
}

void ForStatement(int c) {
  int i, j, k;
  for (i = 0, j = 0, k = 0; j < c, i < c; i = i + j, j = j + 1) {
    k = k + i;
  }
  write(i); write(" "); write(j); write(" "); write(k); write("\n");
  for (i = 0, j = 0, k = 0; i < c, j < c; i = i + j, j = j + 1) {
    k = k + i;
  }
  write(i); write(" "); write(j); write(" "); write(k); write("\n");
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

int LargeArray() {
  int c[1000];
  int a[2], b[2];
  int d[1000];
  a[0] = 1;
  a[1] = a[0] + 3;
  b[0] = a[1] + 2;
  b[1] = b[0] + 1;
  return a[0] + a[1] + b[0] + b[1];
}

int LargeArray2() {
  int a[1000][1000];
  int i = 0;
  while (i < 1000) {
    int j = 0;
    while (j < 1000) {
      a[i][j] = i * 1000 + j;
      j = j + 1;
    }
    i = i + 1;
  }
  return a[395][566];
}

void ManyVariable() {
  int a0 = 0  + 0, a1 = a0 + 1, a2 = a1 + 2, a3 = a2 + 3, a4 = a3 + 4;
  int a5 = a4 + 5, a6 = a5 + 6, a7 = a6 + 7, a8 = a7 + 8, a9 = a8 + 9;
  int b0 = a9 + 0, b1 = b0 + 1, b2 = b1 + 2, b3 = b2 + 3, b4 = b3 + 4;
  int b5 = b4 + 5, b6 = b5 + 6, b7 = b6 + 7, b8 = b7 + 8, b9 = b8 + 9;
  int c0 = b9 + 0, c1 = c0 + 1, c2 = c1 + 2, c3 = c2 + 3, c4 = c3 + 4;
  int c5 = c4 + 5, c6 = c5 + 6, c7 = c6 + 7, c8 = c7 + 8, c9 = c8 + 9;
  int d0 = c9 + 0, d1 = d0 + 1, d2 = d1 + 2, d3 = d2 + 3, d4 = d3 + 4;
  int d5 = d4 + 5, d6 = d5 + 6, d7 = d6 + 7, d8 = d7 + 8, d9 = d8 + 9;
  int e0 = d9 + 0, e1 = e0 + 1, e2 = e1 + 2, e3 = e2 + 3, e4 = e3 + 4;
  int e5 = e4 + 5, e6 = e5 + 6, e7 = e6 + 7, e8 = e7 + 8, e9 = e8 + 9;
  int f0 = e9 + 0, f1 = f0 + 1, f2 = f1 + 2, f3 = f2 + 3, f4 = f3 + 4;
  int f5 = f4 + 5, f6 = f5 + 6, f7 = f6 + 7, f8 = f7 + 8, f9 = f8 + 9;
  int g0 = f9 + 0, g1 = g0 + 1, g2 = g1 + 2, g3 = g2 + 3, g4 = g3 + 4;
  int g5 = g4 + 5, g6 = g5 + 6, g7 = g6 + 7, g8 = g7 + 8, g9 = g8 + 9;
  int h0 = g9 + 0, h1 = h0 + 1, h2 = h1 + 2, h3 = h2 + 3, h4 = h3 + 4;
  int h5 = h4 + 5, h6 = h5 + 6, h7 = h6 + 7, h8 = h7 + 8, h9 = h8 + 9;
  int i0 = h9 + 0, i1 = i0 + 1, i2 = i1 + 2, i3 = i2 + 3, i4 = i3 + 4;
  int i5 = i4 + 5, i6 = i5 + 6, i7 = i6 + 7, i8 = i7 + 8, i9 = i8 + 9;
  int j0 = i9 + 0, j1 = j0 + 1, j2 = j1 + 2, j3 = j2 + 3, j4 = j3 + 4;
  int j5 = j4 + 5, j6 = j5 + 6, j7 = j6 + 7, j8 = j7 + 8, j9 = j8 + 9;
  write(b1); write(" ");
  write(a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 +
        b0 + b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 + b9 +
        c0 + c1 + c2 + c3 + c4 + c5 + c6 + c7 + c8 + c9 +
        d0 + d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 +
        e0 + e1 + e2 + e3 + e4 + e5 + e6 + e7 + e8 + e9 +
        f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7 + f8 + f9 +
        g0 + g1 + g2 + g3 + g4 + g5 + g6 + g7 + g8 + g9 +
        h0 + h1 + h2 + h3 + h4 + h5 + h6 + h7 + h8 + h9 +
        i0 + i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8 + i9 +
        j0 + j1 + j2 + j3 + j4 + j5 + j6 + j7 + j8 + j9); write("\n");
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

void NullNode() {
  ;;;;;
  write("null\n");
  ;;;;;
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
  WhileStatement(); /* aaaaaaaaaaaaaaaaaaaaaa 42 */
  ForStatement(200);
  /* 210 21 1330
   * 19900 200 1313400 */
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
  write(LargeArray()); /* 18 */
  write("\n");
  write(LargeArray2()); /* 395566 */
  write("\n");
  ManyVariable(); /* 46 21900 */
  write(ReadGlobal()); /* 27 */
  write("\n");
  write(WriteGlobal()); /* 6 */
  write("\n");
  PassGlobalArray(); /* 1 0.700000 */
  NullNode(); /* null */
}
