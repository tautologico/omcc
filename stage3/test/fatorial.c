#include <stdio.h>

int fatorial(int n)
{
   int result;  

   result = 1;

   while (n > 0)
   {
     result = result * n;
     n = n - 1;
   }

   return result;
}

int main()
{
   printint(fatorial(5));
   return 0;
}
