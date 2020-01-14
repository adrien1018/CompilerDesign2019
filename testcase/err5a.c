void Ax() {}

void A() {
  int x = Ax();
  int c;
  c = 2 + Ax() + 4;
  for (c = Ax(); Ax(); c = Ax()) {}
  for (c = Ax() + 5; Ax() + 5; c = Ax() + 5) {}
  while (Ax()) {}
  while (Ax() + 5) {}
  if (Ax()) {}
  if (Ax()) {} else {}
}
