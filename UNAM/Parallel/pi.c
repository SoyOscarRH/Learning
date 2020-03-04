#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>
#include <float.h>
#include <omp.h>

typedef long double f64;

f64 f(f64 x) {
  return sqrt(1 - (x * x));
}

int main(int argc, char **argv) {
  uint64_t steps = atoi(argv[1]);
  const f64 delta = 1.0 / steps * 1.0;

  f64 result = 0.0, current = 0.0;
  #pragma omp parallel for reduction(+ : result) 
  {
    for (uint64_t i = 0; i < steps; ++i) {
      result += f(delta * i);
    }
  }
  

  printf("%.17Lf\n", (result  * 4) / steps);

  return 0;
}