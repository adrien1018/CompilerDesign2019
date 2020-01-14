int n, m;
int fact() {
  if (n == 1) {
    return n;
  } else {
    m = n;
    n = n - 1;
    return m * fact();
  }
}

int main() {
  n = read();
  write(fact());
}
