#include <time.h>
#include <math.h>

#include "sum.cu"

int main() {
  const int x = 3, y = 3;
  const int size = x * y;

  const int intput_1[x][y] = {{1, 2, 3}, {1, 2, 3}, {1, 2, 3}};
  const int intput_2[x][y] = {{1, 1, 1}, {1, 1, 1}, {1, 1, 1}};

  int result[x][y] = {0};

  clock_t start = clock();
  sum_matrix_at_cuda((int*)intput_1, (int*)intput_2, (int*)result, size);
  clock_t end = clock();

  double clocks = fabs(end - start);
  double time_taken = clocks / CLOCKS_PER_SEC;
  printf("Time taken: %f\n", time_taken);

  print_matrix((int*)intput_1, x, y);
  printf(" + ");
  print_matrix((int*)intput_2, x, y);
  printf(" = ");
  print_matrix((int*)result, x, y);
  printf("\n");

  return 0;
}
