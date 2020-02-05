typedef struct real_array {
  int* data;
  int size;
} real_array;

real_array create_randoms(const int num_numbers, const int min, const int max) {
  int* data = malloc(sizeof(int) * num_numbers);

  srand(time(NULL));
  for (int i = 0; i < num_numbers; ++i) data[i] = (rand() % (max - min + 1)) + min;

  const real_array result = {.data = data, .size = num_numbers};
  return result;
}

int compare(const void* _a, const void* _b) {
  int *a = (int*)_a, *b = (int*)_b;

  return (*a - *b);
}
