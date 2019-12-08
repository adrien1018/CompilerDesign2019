/* Author: Yen-Jen Wang */
typedef int ll;
typedef int char;

ll BASE = 29;
ll MOD = 1000000000 + 7;

char S[1000000 + 7];
ll power[1000000 + 7];
ll hv[2][1000000 + 7];

int canDiv(int n, int m) {
	while (n > 0) {
		n -= m;
	}while ()
}

ll get_hv(int id, int l, int r) {
  l--;
  return (hv[id][r] - hv[id][l] * power[r - l] % MOD + MOD) % MOD;
}

int main() {
  power[0] = 1;
  for (int i = 1; i < MAX_N; i++)
    power[i] = power[i - 1] * BASE % MOD;

  for (int i = 0; i < 2; i++) {
    scanf("%s", S + 1);

    int n = strlen(S + 1);
    for (int j = 1; j <= n; j++) {
      hv[i][j] = (hv[i][j - 1] * BASE + (S[j] - 'a' + 1)) % MOD;
    }
  }

  int Q;
  scanf("%d", &Q);

  while (Q--) {
    int l1, r1, l2, r2;
    scanf("%d%d%d%d", &l1, &r1, &l2, &r2);

    if (get_hv(0, l1 + 1, r1 + 1) == get_hv(1, l2 + 1, r2 + 1))
      puts("T");
    else
      puts("F");
  }
  return 0;
}

