int meow (int a, int b) {}

void meow (float a, float a) {} /* SYMBOL_REDECLARE *2 */

void meow (int a, float a) {} /* SYMBOL_REDECLARE */

void meow2 (float a, float a) {} /* SYMBOL_REDECLARE */

void meow3 (int a, float a) {} /* SYMBOL_REDECLARE */

int i, j, k;

int main ()
{
  int a, b;
  int i, j, k;
  int p, q;
  int q, p; /* SYMBOL_REDECLARE *2 */
}
