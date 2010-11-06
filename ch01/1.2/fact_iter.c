#include <stdio.h>

int fact_i(int n)
{
  int product = 1;
  int counter = 1;
  int max_count = n;

  while ( !(counter > max_count) ) {
    product = product * counter;
    counter = counter + 1;
  }

  return product;
}

int main(void)
{
  int n;

  printf("n? ");
  scanf("%d", &n);
  
  printf("factorial n=%d\n", fact_i(n));

  return 0;
}

