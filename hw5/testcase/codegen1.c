int IntFunction() {
  return 3;
}

float FloatFunction() {
  return 4.5;
}

int Convert(float a, int b) {
  int c = a;
  float d = b;
  int e = a + 5;
  return c + d + e;
}

void FloatCompare(float a, float b) {
  write(a < b); write(" ");
  write(a <= b); write(" ");
  write(a > b); write(" ");
  write(a >= b); write(" ");
  write(a == b); write(" ");
  write(a != b); write("\n");
}

int main() {
  write(IntFunction()); /* 3 */
  write("\n");
  write(FloatFunction()); /* 4.5 */
  write("\n");
  write(Convert(3.6, 6)); /* 17 */
  write("\n");
  FloatCompare(1.0, 2.0); /* 1 1 0 0 0 1 */
  FloatCompare(1.5, 1.5); /* 0 1 0 1 1 0 */
}
