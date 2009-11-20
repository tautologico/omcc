#include <stdio.h>

int delta(int a, int b, int c)
{
   int b2; 
   int ac4;
   int d;

   b2  = b * b;
   ac4 = 4 * a * c;
   d   = b2 - ac4;

   return d;
}

int main()
{
   // calcula o discriminante da eq. de segundo grau x^2 - 3x + 2
   printint(delta(1, -5, 3));
   return 0;
}
