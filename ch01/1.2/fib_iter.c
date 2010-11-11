#include <stdio.h>

int fib_i(int n)
{
   int fib_next = 1;
   int fib_cur = 0;
   int count = n;

   int tmp;
   while ( count > 0) { 
     tmp = fib_next;

     fib_next = fib_cur + fib_next;
     fib_cur = tmp;
     count--;
   }

   return fib_cur;
}

int main(void) 
{
  int n;

  printf("n? ");
  scanf("%d", &n);
  
  printf("Fibonacci n=%d\n", fib_i(n));

  return 0;
}
