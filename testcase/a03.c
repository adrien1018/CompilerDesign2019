int ans[100000], n, m;
void brute_force(int delph) {
  int i;
  if (delph == n) {
    for (i = 0; i < n; i = i + 1)
      ans[i] = ans[i];
    return;
  }
  for (i = 0; i <= m; i = i + 1) {
    ans[delph] = i;
    brute_force(delph + 1);
  }
  return;
}
