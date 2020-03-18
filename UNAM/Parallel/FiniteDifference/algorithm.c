#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

double initialCondition(double x) { return x * x * (2 - x); }

const int steps_x = 50;
const int steps_t = 100;

#define mat_elem(a, y, x, n) (a + ((y) * (n) + (x)))

void swap_row(double* a, double* b, int r1, int r2, int n) {
  double tmp, *p1, *p2;
  int i;

  if (r1 == r2) return;
  for (i = 0; i < n; i++) {
    p1 = mat_elem(a, r1, i, n);
    p2 = mat_elem(a, r2, i, n);
    tmp = *p1, *p1 = *p2, *p2 = tmp;
  }
  tmp = b[r1], b[r1] = b[r2], b[r2] = tmp;
}

void gauss_eliminate(double* a, double* b, double* x, int n) {
#define A(y, x) (*mat_elem(a, y, x, n))
  int i, j, col, row, max_row, dia;
  double max, tmp;

  for (dia = 0; dia < n; dia++) {
    max_row = dia, max = A(dia, dia);

    for (row = dia + 1; row < n; row++)
      if ((tmp = fabs(A(row, dia))) > max) max_row = row, max = tmp;

    swap_row(a, b, dia, max_row, n);

    for (row = dia + 1; row < n; row++) {
      tmp = A(row, dia) / A(dia, dia);
      for (col = dia + 1; col < n; col++) A(row, col) -= tmp * A(dia, col);
      A(row, dia) = 0;
      b[row] -= tmp * b[dia];
    }
  }
  for (row = n - 1; row >= 0; row--) {
    tmp = b[row];
    for (j = n - 1; j > row; j--) tmp -= x[j] * A(row, j);
    x[row] = tmp / A(row, row);
  }
#undef A
}

int main() {
  double time_max = 1.0;
  double x_max = 2.0;

  double temperature_at[steps_x + 1][steps_t + 1];
  for (int i = 0; i < steps_x + 1; ++i) {
    for (int j = 0; j < steps_t + 1; ++j) temperature_at[i][j] = 0;
  }

  double points_x[steps_x + 1], nodes_x_ini[steps_x + 1];
  for (int i = 0; i < steps_x + 1; ++i) {
    double point = ((double)i / steps_x) * x_max;
    nodes_x_ini[i] = points_x[i] = point;
    temperature_at[i][0] = initialCondition(point);
  }

  double dx = x_max / steps_x;
  double dt = time_max / steps_t;
  double kappa = 4.0;

  double rho = kappa * dt / (dx * dx);

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
  fprintf(gnuplot, "with linespoints title 'Ecuacion del calor',");
  fprintf(gnuplot, "'' with linespoints title 'Ecuacion del calor'\n");
  for (int i = 0; i < steps_x + 1; i++) 
    fprintf(gnuplot, "%g,%g\n", points_x[i], temperature_at[i][(int)round(0.00 / dt)]);
  fprintf(gnuplot, "e\n");

  for (int i = 0; i < steps_x + 1; i++) fprintf(gnuplot, "%g,%g\n", points_x[i], temperature_at[i][(int)round(0.05 / dt)]);
  fprintf(gnuplot, "e\n");
  fflush(gnuplot);

  while (1) sleep(100);

  return 0;
}
