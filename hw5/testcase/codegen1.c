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

int FloatLT(float a, float b) {
  return a < b;
}

int main() {
  write(IntFunction()); /* 3 */
  write("\n");
  write(FloatFunction()); /* 4.5 */
  write("\n");
  write(Convert(3.6, 6)); /* 17 */
  write("\n");
  write(FloatLT(1.0, 2.0)); /* 1 */
  write("\n");
  if (1) {
    float y = 2.5;
    float x = 3.0 + y;
  }
}
