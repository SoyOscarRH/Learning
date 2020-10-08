package Learning.IPN.Distributed.P1;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;

class DistributedPi {
  private static int PORT = 5_000;
  private static int TERMS = 10_000_000;
  private static int NUM_WORKERS = 2;

  private static Object lock = new Object();
  private static double sum = 0;

  public static void main(final String[] args) {
    if (args.length != 1) {
      System.err.println("Use:" + "\n" + "java DistributedPi <server|client>");
      return;
    }

    try {
      final var maybeServer = new Thread(() -> {
        if (args[0].equals("server")) {
          server();
        }
      });
      maybeServer.start();
      client();
      maybeServer.join();

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  static private void client() {
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

        var myTerms = 0.0;
        var ithOdd = id;
        for (var step = 0; step < TERMS; ++step) {
          final var denominator = 2.0 * ithOdd + 1.0;
          final var sign = (ithOdd % 2) == 0;
          final var numerator = sign ? 1.0 : -1.0;
          myTerms += numerator / denominator;
          ithOdd += NUM_WORKERS;
        }

        output.writeDouble(myTerms);
        output.flush();

      } catch (final Exception e) {
        try {
          Thread.sleep(6_000);
        } catch (Exception e2) {
          e2.printStackTrace();
        }
        e.printStackTrace();
      }
    }
    System.out.println("Finishing...");
  }

  private static void server() {
    System.out.println("Starting Server...");
    try (final var serverSocket = new ServerSocket(PORT)) {
      System.out.println("Server ready");
      var id = 0;
      final var workers = new Thread[NUM_WORKERS];
      for (var i = 0; i < NUM_WORKERS; ++i) {
        final var connection = serverSocket.accept();
        final var clientID = i;
        workers[i] = new Thread(() -> addTerms(connection, clientID));
        workers[i].start();
        System.out.println("Starting worker " + i);
      }

      id = 0;
      for (final var worker : workers) {
        System.out.println("Asking for part" + (++id));
        worker.join();
      }

      System.out.println("pi is " + 4 * sum);

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  private static void addTerms(final Socket connection, int id) {
    try {
      final var input = new DataInputStream(connection.getInputStream());
      final var output = new DataOutputStream(connection.getOutputStream());
      output.writeInt(id);
      output.flush();

      final var terms = input.readDouble();
      synchronized (lock) {
        sum += terms;
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
