import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.math.BigInteger;

class DistributedMatrix {
  private static int PORT = 5_000;
  private static int NUM_WORKERS = 4;

  public static int[][][] initMatrices(final int n) {
    final var A = new int[n][n];
    final var B = new int[n][n];

    for (var i = 0; i < n; ++i) {
      for (var j = 0; j < n; ++j) {
        A[i][j] = 2 * i + j;
        B[i][j] = 2 * i - j;
      }
    }

    final var matrices = new int[][][] { A, B };
    return matrices;
  }

  public static void printMatrix(final int[][] matrix) {
    final var n = matrix.length;
    for (var i = 0; i < n; ++i) {
      System.out.print("[");
      for (var j = 0; j < n; ++j) {
        System.out.print(matrix[i][j] + " ");
      }
      System.out.println("]");
    }
  }

  public static int[][][] splitVerticalMatrix(final int[][] matrix) {
    final var n = matrix.length;
    var m1 = new int[n][n / 2];
    var m2 = new int[n][n / 2];

    for (var i = 0; i < n; ++i) {
      for (var j = 0; j < n / 2; ++j) {
        m1[i][j] = matrix[i][j];
      }
    }

    for (var i = 0; i < n; ++i) {
      for (var j = n / 2; j < n; ++j) {
        m2[i][j - n / 2] = matrix[i][j];
      }
    }

    return new int[][][] { m1, m2 };
  }

  public static int[][][] splitHorizontalMatrix(final int[][] matrix) {
    final var n = matrix.length;
    var m1 = new int[n / 2][n];
    var m2 = new int[n / 2][n];

    for (var i = 0; i < n / 2; ++i) {
      for (var j = 0; j < n; ++j) {
        m1[i][j] = matrix[i][j];
      }
    }

    for (var i = n / 2; i < n; ++i) {
      for (var j = 0; j < n; ++j) {
        m2[i - n / 2][j] = matrix[i][j];
      }
    }

    return new int[][][] { m1, m2 };
  }

  public static int[][] multiply(final int[][] A, final int[][] B) {
    final var l = A.length;
    final var m = A[0].length;
    final var n = B[0].length;

    var tB = new int[n][m];

    // transpose matrix B
    for (int i = 0; i < m; i++) {
      for (int j = 0; j < n; j++) {
        tB[j][i] = B[i][j];
      }
    }

    final var result = new int[l][n];
    // multiply A * B storing it in C
    for (var i = 0; i < l; i++)
      for (var j = 0; j < n; j++)
        for (var k = 0; k < m; k++)
          result[i][j] += A[i][k] * tB[j][k];

    return result;
  }

  public static void sendMatrix(final DataOutputStream output, final int[][] matrix) throws IOException {
    final var n = matrix.length;
    final var m = matrix[0].length;
    output.writeInt(n);
    output.writeInt(m);
    for (var i = 0; i < n; ++i) {
      for (var j = 0; j < m; ++j) {
        output.writeInt(matrix[i][j]);
      }
    }

    output.flush();
  }

  public static int[][] getMatrix(final DataInputStream input) throws IOException {
    final var n = input.readInt();
    final var m = input.readInt();

    final var matrix = new int[n][m];
    for (var i = 0; i < n; ++i) {
      for (var j = 0; j < m; ++j) {
        matrix[i][j] = input.readInt();
      }
    }

    return matrix;
  }

  public static void clientNode() {
    var hasConnected = false;
    while (!hasConnected) {
      System.out.println("Connecting to server...");
      try (final var socketClient = new Socket("localhost", PORT)) {
        System.out.println("Connected to server");
        hasConnected = true;

        final var input = new DataInputStream(socketClient.getInputStream());
        final var output = new DataOutputStream(socketClient.getOutputStream());

        final var id = input.readInt();
        System.out.println("Worker with id " + id + " starting...");

        final var matrix1 = getMatrix(input);
        final var matrix2 = getMatrix(input);

        final var result = multiply(matrix1, matrix2);
        sendMatrix(output, result);

        if (!hasConnected)
          Thread.sleep(6_000);

      } catch (final Exception e) {
        e.printStackTrace();
      }
    }
    System.out.println("Finishing...");
  }

  private static int[][] sendOperation(final Socket connection, final int id, final int[][][] As, final int[][][] Bs) {
    try {
      final var input = new DataInputStream(connection.getInputStream());
      final var output = new DataOutputStream(connection.getOutputStream());
      output.writeInt(id);

      if (id == 0) {
        sendMatrix(output, As[0]);
        sendMatrix(output, Bs[0]);
      }
      if (id == 1) {
        sendMatrix(output, As[0]);
        sendMatrix(output, Bs[1]);
      }
      if (id == 2) {
        sendMatrix(output, As[1]);
        sendMatrix(output, Bs[0]);
      }
      if (id == 3) {
        sendMatrix(output, As[1]);
        sendMatrix(output, Bs[1]);
      }

      return getMatrix(input);

    } catch (Exception e) {
      e.printStackTrace();
      return new int[1][1];
    }
  }

  public static void serverNode() {
    final var n = 1_000;
    final var matrices = initMatrices(n);

    var A = matrices[0];
    var B = matrices[1];

    final var As = splitHorizontalMatrix(A);
    final var Bs = splitVerticalMatrix(B);

    final var C = new int[n][n];
    final var Cs = new int[n][][];

    System.out.println("Starting Server...");
    try (final var serverSocket = new ServerSocket(PORT)) {
      System.out.println("Server ready");
      final var workers = new Thread[NUM_WORKERS];
      for (var i = 0; i < NUM_WORKERS; ++i) {
        final var connection = serverSocket.accept();
        final var id = i;
        workers[i] = new Thread(() -> {
          Cs[id] = sendOperation(connection, id, As, Bs);
        });

        workers[i].start();
        System.out.println("Starting worker " + i);
      }

      var id = 0;
      for (final var worker : workers) {
        System.out.println("Asking for part" + (++id));
        worker.join();
      }

    } catch (Exception e) {
      e.printStackTrace();
    }

    for (var i = 0; i < n / 2; ++i) {
      for (var j = 0; j < n / 2; ++j) {
        C[i + 0][j + 0] = Cs[0][i][j];
        C[i + 0][j + n / 2] = Cs[1][i][j];
        C[i + n / 2][j + 0] = Cs[2][i][j];
        C[i + n / 2][j + n / 2] = Cs[3][i][j];
      }
    }

    var checksum = BigInteger.ZERO;
    for (var i = 0; i < n; ++i)
      for (var j = 0; j < n; ++j)
      checksum = checksum.add(BigInteger.valueOf(C[i][j]));

    if (n == 4) printMatrix(C);
    System.out.println(checksum);
  }

  public static void main(final String[] args) {
    if (args.length != 1) {
      System.err.println("Use: java DistributedMatrix [0|1|2|3|4]");
      return;
    }

    final var nodeID = Integer.parseInt(args[0]);
    if (nodeID == 0)
      serverNode();
    else
      clientNode();
  }
}