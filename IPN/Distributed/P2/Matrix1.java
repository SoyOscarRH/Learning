package Learning.IPN.Distributed.P2;

public class Matrix1 {
  public static void main(String[] args) {
    final var N = 1000;
    var A = new int[N][N];
    var B = new int[N][N];
    var C = new int[N][N];

    final var t1 = System.currentTimeMillis();

    // init A and B
    for (var i = 0; i < N; i++)
      for (var j = 0; j < N; j++) {
        A[i][j] = 2 * i - j;
        B[i][j] = i + 2 * j;
        C[i][j] = 0;
      }

    // multiply A * B storing it in C
    for (var i = 0; i < N; i++)
      for (var j = 0; j < N; j++)
        for (var k = 0; k < N; k++)
          C[i][j] += A[i][k] * B[k][j];

    final var t2 = System.currentTimeMillis();
    System.out.println("Time: " + (t2 - t1) + "ms");
  }
}
