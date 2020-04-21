#include <stdio.h>

#include "cuda_runtime.h"
#include "device_launch_parameters.h"

__global__ void sum_matrix_kernel(const int* matrix_1_device, const int* matrix_2_device,
                                  int* result_device, int size) {
  const int32_t i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < size) result_device[i] = matrix_1_device[i] + matrix_2_device[i];
}

void sum_matrix_at_cuda(const int* matrix_1_host, const int* matrix_2_host, int* result_host,
                        const int size) {
  int *result_device = NULL, *matrix_1_device = NULL, *matrix_2_device = NULL;
  const int how_many = size * sizeof(int);

  cudaMalloc((void**)&result_device, how_many);
  cudaMalloc((void**)&matrix_1_device, how_many);
  cudaMalloc((void**)&matrix_2_device, how_many);

  cudaMemcpy(matrix_1_device, matrix_1_host, how_many, cudaMemcpyHostToDevice);
  cudaMemcpy(matrix_2_device, matrix_2_host, how_many, cudaMemcpyHostToDevice);

  sum_matrix_kernel<<<2, (how_many + 1) / 2>>>(matrix_1_device, matrix_2_device, result_device,
                                               how_many);

  cudaDeviceSynchronize();

  cudaMemcpy(result_host, result_device, how_many, cudaMemcpyDeviceToHost);

  cudaFree(result_device);
  cudaFree(matrix_1_device);
  cudaFree(matrix_2_device);
  cudaDeviceReset();
}

void print_matrix(const int* matrix, int x, int y) {
  printf("{");
  for (int i = 0; i < x; ++i) {
    printf("{");
    for (int j = 0; j < y; ++j) printf("%i, ", matrix[i * x + j]);
    printf("}");
  }
  printf("}");
}
