#include <math.h>

/*
From https://rosettacode.org/wiki/Gaussian_elimination#C
*/


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
#undef mat_elem
}
