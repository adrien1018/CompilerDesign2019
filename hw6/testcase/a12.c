int i, j, k;

int main() {
  int a, b, c, d, e;
  i = i == 0;
  i = (i == 0);
  j = (k * 100) / 100;
  j = (i * 5 == 0 && i * 3 != 0) || (i * 5 != 0 && i * 3 == 0);
  e = (a - c) / (b - d) == c / d;
  a = (b * b - 4 *a * c == 0);
  d = (2 * (a * b + b * c + c * a) == a *b * c);
  a = -a;
  a = 2 * (b + c);
  a = (b + c) * 2;
  a = 0;
  a = -1;
  a = 1;
  if (a + b > c && c > d) i = i;
  else i = 0;
}

int mod(int n, int m) {
  while (n > 0) n = n - m;
  while (n < 0) n = n + m;
  return n;
}

int rabbit_chicken() {
	int a, b, c, rabbit, chicken, crab;
	crab = a - c;
	chicken = (8 * a - b - 4 * c) / 2;
	rabbit = (- 8 * a + b + 6 * c) / 2;
	if (a - c < 0 || mod((8 * a - b - 4 * c), 2) != 0 || mod((- 8 * a + b + 6 * c), 2) != 0 )
		return 0;
	else if (8 * a - b - 4 * c < 0 || - 8 * a + b + 6 * c < 0)
    return 0;
	else
    return 0;

  for (a = 0; a < 100; a = a + 1) {
    for (b = 0; b < 100; b = b + 1) {
      write("meow");
    }
  }
}

