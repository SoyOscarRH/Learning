#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "solver.c"

double initialCondition(double x) { return x * x * (2 - x); }

const int steps_x = 50;
const int steps_t = 100;

int main() {
  double time_max = 1.0;
  double x_max = 2.0;

  double temperature_at[steps_x + 1][steps_t + 1];
  for (int i = 0; i < steps_x + 1; ++i) {
    for (int j = 0; j < steps_t + 1; ++j) temperature_at[i][j] = 0;
  }

  double points_x[steps_x + 1], nodes_x_ini[steps_x + 1];

#pragma omp parallel
#pragma omp for
  for (int i = 0; i < steps_x + 1; ++i) {
    double point = ((double)i / steps_x) * x_max;
    nodes_x_ini[i] = points_x[i] = point;
    temperature_at[i][0] = initialCondition(point);
  }

  double dx = x_max / steps_x;
  double dt = time_max / steps_t;
  double kappa = 4.0;

  double rho = kappa * dt / (dx * dx);

#pragma omp parallel
#pragma omp for
  for (int k = 0; k < steps_t; ++k) {
    double mat_dig[steps_x + 1][steps_x + 1];
    for (int _i = 0; _i < steps_x + 1; ++_i) {
      for (int _j = 0; _j < steps_x + 1; ++_j) mat_dig[_i][_j] = 0;
    }

    for (int i = 1; i < steps_x; ++i) {
      mat_dig[i][i] = 1.0 + (2.0 * rho);
      mat_dig[i][i - 1] = -rho;
      mat_dig[i][i + 1] = -rho;
    }

    mat_dig[0][0] = mat_dig[steps_x][steps_x] = 1.0;

    double rhs[steps_x + 1], x[steps_x + 1];
    for (int i = 0; i < steps_x + 1; ++i) rhs[i] = temperature_at[i][k];

    gauss_eliminate((double*)mat_dig, rhs, x, steps_x + 1);

    for (int i = 0; i < steps_x + 1; ++i) temperature_at[i][k + 1] = x[i];
  }

  FILE* gnuplot = popen("gnuplot", "w");
  fprintf(gnuplot, "set ylabel \"Temperature\"\n");
  fprintf(gnuplot, "set xlabel \"space\"\n");
  fprintf(gnuplot, "set title \"Heat eq\"\n");
  fprintf(gnuplot, "set datafile separator \",\";");
  fprintf(gnuplot, "plot '-' ");

  double interesting_points[] = {0.0, 0.015, 0.05, 0.8, 0.12, 0.9};
  int n = sizeof(interesting_points) / sizeof(double);

  for (int i = 0; i < n; ++i) {
    if (i != 0) fprintf(gnuplot, "'' ");
    fprintf(gnuplot, "with linespoints title '%g',", interesting_points[i]);
  }
  fprintf(gnuplot, "\n");

  for (int _i = 0; _i < n; ++_i) {
    for (int i = 0; i < steps_x + 1; i++) {
      double y = temperature_at[i][(int)round(interesting_points[_i] / dt)];
      fprintf(gnuplot, "%g,%g\n", points_x[i], y);
    }

    fprintf(gnuplot, "e\n");
  }

  fflush(gnuplot);

  while (1) sleep(100);

  return 0;
}
