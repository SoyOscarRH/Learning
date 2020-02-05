#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "utils.c"
#include "bucket.c"

void bucket_paralel_sort(const int num_of_threads, const real_array numbers) {
  const int step = numbers.size / num_of_threads;
  real_array* buckets = malloc(sizeof(real_array) * num_of_threads);
  pthread_t* ids = malloc(sizeof(pthread_t) * num_of_threads);

  int start = 0, end = 0;
  for (int i = 0; i < num_of_threads - 1; ++i) {
    start = i * step;
    end = start + step - 1;

    fragment_args* args = create_args(numbers, start, end, &buckets[i]);
    pthread_create(&ids[i], NULL, fragment_sort, args);
  }

  start = end + 1, end = numbers.size - 1;
  fragment_args* args = create_args(numbers, start, end, &buckets[num_of_threads - 1]);
  pthread_create(&ids[num_of_threads - 1], NULL, fragment_sort, args);

  for (int i = 0; i < num_of_threads; ++i) pthread_join(ids[i], NULL);

  for (int i = 0; i < num_of_threads; ++i) {
    const int* data = buckets[i].data;
    const int size = buckets[i].size;
    for (int j = 0; j < size; j++) printf("%i\n", data[j]);
  }
}

int main(int argc, char** argv) {
  if (argc != 2) {
    printf("Use as ./bucket num_of_threads\n");
    return 0;
  }

  const int num_of_threads = atoi(argv[1]);
  const int num_numbers = 9999;
  const int min = 0, max = 999;

  real_array numbers = create_randoms(num_numbers, min, max);
  bucket_paralel_sort(num_of_threads, numbers);

  return 0;
}
