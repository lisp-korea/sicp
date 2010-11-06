#include <stdio.h>

int fib_iter(int n)
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
  
  printf("Fibonacci n=%d\n", fib_iter(n));

  return 0;
}
