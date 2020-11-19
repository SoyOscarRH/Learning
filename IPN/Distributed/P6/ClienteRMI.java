import java.math.BigInteger;
import java.rmi.*;

class ClienteRMI {

  static final int N = 4;

  static int[][] parte_matriz(int[][] A, int inicio) {
    int[][] M = new int[N / 2][N];

    for (int i = 0; i < N / 2; i++)
      for (int j = 0; j < N; j++)
        M[i][j] = A[i + inicio][j];

    return M;
  }

  static void acomoda_matriz(int[][] C, int[][] A, int renglon, int columna) {
    for (int i = 0; i < N / 2; i++)
      for (int j = 0; j < N / 2; j++)
        C[i + renglon][j + columna] = A[i][j];
  }

  public static int[][][] initMatrices(final int n) {
    final var A = new int[n][n];
    final var B = new int[n][n];

    for (var i = 0; i < n; ++i) {
      for (var j = 0; j < n; ++j) {
        A[i][j] = 2 * i + j;
        B[j][i] = 2 * i - j;
      }
    }

    final var matrices = new int[][][] { A, B };
    return matrices;
  }

  public static void printMatrix(final int[][] matrix) {
    final var n = matrix.length;
    final var m = matrix[0].length;
    for (var i = 0; i < n; ++i) {
      System.out.print("[");
      for (var j = 0; j < m; ++j) {
        System.out.print(matrix[i][j] + " ");
      }
      System.out.println("]");
    }
  }

  public static void transpose(final int[][] matrix) {
    for (int i = 0; i < N; i++) {
      for (int j = 0; j < N; j++) {
        matrix[j][i] = matrix[i][j];
      }
    }
  }

  public static void main(String args[]) throws Exception {

    final var matrices = initMatrices(N);

    var A = matrices[0];
    var transposeB = matrices[1];

    int[][] A1 = parte_matriz(A, 0);
    int[][] A2 = parte_matriz(A, N / 2);
    int[][] B1 = parte_matriz(transposeB, 0);
    int[][] B2 = parte_matriz(transposeB, N / 2);

    int[][] C1;
    {
      // node 1
      InterfaceRMI r = (InterfaceRMI) Naming.lookup("rmi://localhost/prueba");
      C1 = r.multiplica_matrices(A1, B1, N);
    }

    int[][] C2;
    {
      // node 2
      InterfaceRMI r = (InterfaceRMI) Naming.lookup("rmi://40.78.128.89/prueba");
      C2 = r.multiplica_matrices(A1, B2, N);
    }

    int[][] C3;
    {
      // node 3
      InterfaceRMI r = (InterfaceRMI) Naming.lookup("rmi://52.165.22.197/prueba");
      C3 = r.multiplica_matrices(A2, B1, N);
    }

    int[][] C4;
    {
      // node 4
      InterfaceRMI r = (InterfaceRMI) Naming.lookup("rmi://52.176.2.166/prueba");
      C4 = r.multiplica_matrices(A2, B2, N);
    }

    int[][] C = new int[N][N];
    acomoda_matriz(C, C1, 0, 0);
    acomoda_matriz(C, C2, 0, N / 2);
    acomoda_matriz(C, C3, N / 2, 0);
    acomoda_matriz(C, C4, N / 2, N / 2);

    var checksum = BigInteger.ZERO;
    for (var i = 0; i < N; ++i)
      for (var j = 0; j < N; ++j)
        checksum = checksum.add(BigInteger.valueOf(C[i][j]));

    if (N == 4)
      printMatrix(C);
    System.out.println(checksum);
  }
}