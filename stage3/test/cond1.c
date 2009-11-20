#include <stdio.h>

int menor(int a, int b)
{
   int result;  

   if (a < b)
     result = 1;
   else
     result = 0;

   return result;
}

int main()
{
   if (menor(5, 10))
     printf("5 e' menor que 10");
   else
     printf("5 e' maior ou igual a 10");

   return 0;
}
