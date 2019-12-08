int canDiv(int a, int b) {
  while (a > 0) {
    a = a - b;
  }
  if (a == 0) {
    return 1;
  } else {
    return 0;
  }
}

void judgeLeapYeap(int n) {
  if (canDiv(n, 400))
    write("YES");
  else {
    if (!canDiv(n, 4))
      write("NO");
    else {
      if (!canDiv(n, 100))
        write("YES");
      else {
        if (!canDiv(n, 400))
          write("NO");
      }
    }
  }
}
