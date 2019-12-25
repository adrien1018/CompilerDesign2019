int dp[100000], a[100000], st[4 * 100000];

void Modify(int l, int r, int p, int v, int o) {
  int m = (l + r) / 2;
  if (r - l == 1) {
    if (v > st[o]) st[o] = v;
    return;
  }
  if (p < m) Modify(l, m, p, v, o * 2 + 1);
  else Modify(m, r, p, v, o * 2 + 2);
  st[o] = 0;
  if (st[o * 2 + 1] > st[o]) st[o] = st[o * 2 + 1];
  if (st[o * 2 + 2] > st[o]) st[o] = st[o * 2 + 2];
}

int Query(int l, int r, int ql, int qr, int o) {
  int m = (l + r) / 2, v = 0, lq, rq;
  if (l >= qr || ql >= r) return 0;
  if (l >= ql && r <= qr) return st[o];
  lq = Query(l, m, ql, qr, o * 2 + 1);
  if (lq > v) v = lq;
  rq = Query(m, r, ql, qr, o * 2 + 2);
  if (rq > v) v = rq;
  return v;
}

int main() {
  int n = read(), m = read(), i = 0, j = 0, ans = 0;
  while (i < n) {
    a[i] = j;
    j = j + 1;
    if (j == m) j = 0;
    i = i + 1;
  }
  i = 0;
  while (i < n) {
    dp[i] = Query(0, m, 0, a[i], 0) + 1;
    Modify(0, m, a[i], dp[i], 0); 
    if (dp[i] > ans) ans = dp[i];
    i = i + 1;
  }
  write(ans);
  write("\n");
}
