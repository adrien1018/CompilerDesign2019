float c() {
  return 2.66;
}

int main() {
  int a = 5;
  write(a + 4);
  write("meow\n");
  write(c() - 2);
  if (c() < 3) {
    float b = fread();
    int c = read();
    a = b * 3 + c;
  } else {
    a = a + 3;
  }
  write(a);
}
