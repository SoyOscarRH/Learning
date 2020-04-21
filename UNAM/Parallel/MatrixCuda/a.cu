#include <cuda_device_runtime_api.h>
#include <cuda_runtime_api.h>
#include <device_launch_parameters.h>
#include <stdio.h>
#include <stdlib.h>

#include "cuda_runtime.h"

__global__ void matrix_multiplication(int* m, int* n, int* p, int size) {
  int row = blockIdx.y * blockDim.y + threadIdx.y;
  int col = blockIdx.x * blockDim.x + threadIdx.x;

  int p_sum;

  for (int i = 0; i < size; i++) {
    p_sum += m[row * size + i] * n[col * size + i];
  }

  p[row * size + col] = p_sum;
}

int main() {
  // numero de elementos
  int n = 1024;

  // matrices a manejar
  int* h_m;
  int* h_n;
  int* h_p;

  // matrices a manejar en el device
  int* d_m;
  int* d_n;
  int* d_p;

  size_t bytes = n * n * sizeof(int);

  // alocamos memoria en el host
  h_m = (int*)malloc(bytes);
  h_n = (int*)malloc(bytes);
  h_p = (int*)malloc(bytes);

  // inicializamos las matrices

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      h_m[i * n + j] = rand() % 1024;
      h_n[i * n + j] = rand() % 1024;
    }
  }

  // alocar memoria en el device

  cudaMalloc(&d_m, bytes);
  cudaMalloc(&d_n, bytes);
  cudaMalloc(&d_p, bytes);

  // copiando al device
  cudaMemcpy(d_m, h_m, bytes, cudaMemcpyHostToDevice);
  cudaMemcpy(d_n, h_n, bytes, cudaMemcpyHostToDevice);

  int threads_per_block = 16;
  dim3 block_size(threads_per_block, threads_per_block);  // hay 16 bloques con 16 hilos

  dim3 grid_size(n / block_size.x, n / block_size.y);

  // llamar el kernel
  matrix_multiplication<<<grid_size, block_size>>>(d_m, d_n, d_p, n);
  cudaDeviceSynchronize();

  // copiar al host

  cudaMemcpy(h_p, d_p, bytes, cudaMemcpyDeviceToHost);

  printf("%d", h_p[0]);

  return 0;
}