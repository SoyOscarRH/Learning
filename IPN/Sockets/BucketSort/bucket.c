typedef struct fragment_args {
  real_array numbers;
  int start;
  int end;
  real_array* bucket;
} fragment_args;

fragment_args* create_args(real_array numbers, int start, int end, real_array* bucket) {
  fragment_args* args = malloc(sizeof(fragment_args));

  args->numbers = numbers;
  args->start = start;
  args->end = end;
  args->bucket = bucket;

  return args;
}

int compare(const void* _a, const void* _b) {
  int *a = (int*)_a, *b = (int*)_b;

  return (*a - *b);
}

void* fragment_sort(void* _args) {
  fragment_args* args = _args;

  const real_array numbers = args->numbers;
  const int start = args->start;
  const int end = args->end;
  real_array* bucket = args->bucket;

  int elements = 0;
  for (int i = 0; i < numbers.size; i++) {
    const int value = numbers.data[i];
    if (start <= value && value <= end) ++elements;
  }

  bucket->data = malloc(sizeof(int) * elements);
  bucket->size = elements;

  for (int i = 0, j = 0; i < numbers.size; i++) {
    const int value = numbers.data[i];
    if (start <= value && value <= end) bucket->data[j++] = value;
  }

  qsort(bucket->data, bucket->size, sizeof(int), compare);
  free(_args);

  return NULL;
}
