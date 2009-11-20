#include <stdio.h>

// calculo alterado do discriminante
int delta(int a, int b, int c)
{
   b = b - 1;
   return b * b - 4 * a * c;
}

int main()
{
   // calcula o discriminante da eq. de segundo grau x^2 - 3x + 2
   printint(delta(1, -3, 2));
   return 0;
}
