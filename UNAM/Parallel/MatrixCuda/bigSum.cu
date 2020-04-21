#include <time.h>

#include "sum.cu"

int main() {
  srand(time(0));

  const int x = 300, y = 300;
  const int size = x * y;

  int* intput_1 = (int*)malloc(size * sizeof(int));
  int* intput_2 = (int*)malloc(size * sizeof(int));
  int result[x][y] = {0};

  for (int i = 0; i < size; ++i) intput_1[i] = rand() % 5, intput_2[i] = rand() % 5;

  clock_t start = clock();
  sum_matrix_at_cuda((int*)intput_1, (int*)intput_2, (int*)result, size);
  clock_t end = clock();

  double clocks = fabs(end - start);
  double time_taken = clocks / CLOCKS_PER_SEC;
  printf("Time taken: %f\n", time_taken);

  // print_matrix((int*)intput_1, x, y);
  // printf(" + ");
  // print_matrix((int*)intput_2, x, y);
  // printf(" = ");
  // print_matrix((int*)result, x, y);
  // printf("\n");

  return 0;
}
