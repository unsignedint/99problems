/* derivative.c
   cc -o derivative derivative.c -lm */
#include <float.h>
#include <math.h>
#include <stdio.h>

typedef double fn_t (double);

double derivative(fn_t * f, double x)
{
  const double eps = 1e-6;
  return ((*f)(x + eps) - (*f)(x)) / eps;
}

void main(void)
{
  printf("The derivative of cos(x) in x=1 is %f\n", derivative(cos, 1));
}
