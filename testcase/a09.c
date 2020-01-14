void judgeGrade(int n) {
  if (90 <= n && n <= 100)
    write("A");
  else if (80 <= n && n < 90)
    write("B");
  else if (70 <= n && n < 80)
    write("C");
  else if (60 <= n && n < 70)
    write("D");
  else
    write("E");
}
