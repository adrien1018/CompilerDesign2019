typedef int a, a2;
typedef float b, b2;
int c, c2;
float d, d2;
typedef void i, i2;

int meow (int a) {}

int main ()
{
  a();
  a2();
  b(1);
  c(c, c2);
  meow(d());
  meow(1 * i());
  a(1 * i());
}
