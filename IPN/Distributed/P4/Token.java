import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;

class Token {
  static DataInputStream entrada;
  static DataOutputStream salida;
  static boolean primera_vez = true;
  static String ip;
  static long token = 0;
  static int node;

  static class Worker extends Thread {
    public void run() {
      try {
        final var server = new ServerSocket(50_000);
        final var socket = server.accept();
        entrada = new DataInputStream(socket.getInputStream());
      } catch (Exception e) {
        e.printStackTrace();
      }
    }
  }

  public static void main(String[] args) throws Exception {
    if (args.length != 2) {
      System.err.println("Use java Token node_number IP");
      System.exit(1);
    }

    node = Integer.valueOf(args[0]);
    ip = args[1];

    final var w = new Worker();
    w.start();

    Socket socket = null;

    while (true) {
      try {
        socket = new Socket(ip, 50_000);
        break;
      } catch (Exception e) {
        Thread.sleep(2_000);
      }
    }

    System.out.println("Socket ready");
    salida = new DataOutputStream(socket.getOutputStream());
    System.out.println("salida ready");

    w.join();

    System.out.println("entrada ready");

    while (true) {
      final var started = node == 0 && primera_vez;
      if (started) {
        System.out.println("Token value is " + token);
        primera_vez = false;
      } else {
        token = entrada.readLong();
        System.out.println("Token value is " + token);
      }

      token++;
      salida.writeLong(token);
      System.out.println("sending token");
      Thread.sleep(4_000);
    }
  }
}

// Node 1: 13.67.191.121
// Node 2: 52.165.154.38
// Node 3: 104.43.251.125
// Node 4: 40.77.66.158
// cMdDBUBS93wuRQ$

//javac Token.java && java Token 0 52.165.154.38
//javac Token.java && java Token 1 104.43.251.125
//javac Token.java && java Token 2 40.77.66.158
//javac Token.java && java Token 3 13.67.191.121