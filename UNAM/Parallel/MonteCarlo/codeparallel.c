#include <math.h>
#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

typedef long double f64;
const f64 MAX = RAND_MAX;
int id, nproc;

typedef struct integral {
  f64 a, b, c, d, N;
} integral;

typedef struct data {
  f64 mass, xm, ym;
  f64 (*mass_fn)(f64, f64);
  f64 (*xn_fn)(f64, f64);
  f64 (*yn_fn)(f64, f64);
} data;

f64 f1(f64 x, f64 y) { return x + 2 * y; }
f64 xf1(f64 x, f64 y) { return x * f1(x, y); }
f64 yf1(f64 x, f64 y) { return y * f1(x, y); }

f64 f2(f64 x, f64 y) { return sin(sqrt(x * x + y * y)); }
f64 xf2(f64 x, f64 y) { return x * f2(x, y); }
f64 yf2(f64 x, f64 y) { return y * f2(x, y); }

f64 randf(f64 start, f64 end) {
  const f64 difference = end - start;
  const f64 range = difference * ((f64)rand() / MAX);
  return range + start;
}

f64 double_integral(const integral data, f64 f(f64, f64)) {
  const int work = data.N / (nproc - 1);
  f64 total = 0.0;

  if (id != 0) {
    f64 partial = 0.0;
    for (int i = 0; i < work; ++i) {
      partial += f(randf(data.a, data.b), randf(data.c, data.d));
    }
    MPI_Send(&partial, 1, MPI_LONG_DOUBLE, 0, 1, MPI_COMM_WORLD);
    MPI_Barrier(MPI_COMM_WORLD);
    return 3.0;
  } else {
    MPI_Status status;
    f64 partial = 0.0;
    for (int i = 1; i < nproc; ++i) {
      MPI_Recv(&partial, 1, MPI_LONG_DOUBLE, MPI_ANY_SOURCE, 1, MPI_COMM_WORLD, &status);
      total += partial;
    }
    MPI_Barrier(MPI_COMM_WORLD);
  }

  return ((data.b - data.a) * (data.d - data.c)) / data.N * total;
}

void to_calculations(integral i, const data d) {
  f64 const times[4] = {10, 100, 1000, 10000};

  for (int j = 0; j < 4; ++j) {
    i.N = times[j];
    const f64 mass = double_integral(i, d.mass_fn);
    const f64 xm = double_integral(i, d.xn_fn) / mass;
    const f64 ym = double_integral(i, d.yn_fn) / mass;

    if (id == 0) {
      printf("%.0Lf times\n", i.N);
      printf("mass = %Lf\n", mass);
      printf("(xm, yn) = (%Lf, %Lf)\n", xm, ym);

      printf("mass error = %Lf\n", fabsl(d.mass - mass) / fabsl(d.mass));

      f64 base_xm = fabsl(d.xm), base_ym = fabsl(d.ym);
      printf("xm error = %Lf\n", fabsl(d.xm - xm) / (base_xm ? base_xm : 1.0));
      printf("ym error = %Lf\n\n", fabsl(d.ym - ym) / (base_ym ? base_ym : 1.0));
    }
  }
}

int main() {
  srand(time(0));
  clock_t start = clock();
  MPI_Init(NULL, NULL);
  MPI_Comm_rank(MPI_COMM_WORLD, &id);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  // First function
  integral i1 = {.a = 0, .b = 1, .c = 0, .d = 1, .N = 100};
  data d1 = {.mass = 1.5, .xm = 0.5556, .ym = 0.611, .mass_fn = f1, .xn_fn = xf1, .yn_fn = yf1};
  to_calculations(i1, d1);

  // Second function
  integral i2 = {.a = -1, .b = 1, .c = -1, .d = 1, .N = 100};
  data d2 = {.mass = 2.6635, .xm = 0.0, .ym = 0.0, .mass_fn = f2, .xn_fn = xf2, .yn_fn = yf2};

  to_calculations(i2, d2);

  if (id == 0) {
    clock_t end = clock();

    f64 clocks = end - start;
    f64 time_taken = clocks / CLOCKS_PER_SEC;
    printf("Time taken: %Lf", time_taken);
  }

  MPI_Finalize();

  return 0;
}