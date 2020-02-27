import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Scanner;

class Worker implements Runnable {
  private final Socket connection;
  private final int counter;

  Worker(Socket connection, int counter) {
    this.connection = connection;
    this.counter = counter;
  }

  public void run() {
    try {
      final var scanner = new Scanner(connection.getInputStream(), "UTF-8");
      final var serverPrintOut = new PrintWriter(new OutputStreamWriter(connection.getOutputStream(), "UTF-8"), true);

      serverPrintOut.println("Hello Client " + counter + ". Enter Peace to exit.");

      while (scanner.hasNextLine()) {
        final var line = scanner.nextLine();
        if (line.equals("close"))
          break;
        serverPrintOut.println("I am server: " + line);
      }

      connection.close();
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}

public class Server {
  public static void main(String[] args) {
    var counter = 0;

    while (true) {
      try (final var serverSocket = new ServerSocket(5555)) {
        final var connection = serverSocket.accept();
        new Thread(new Worker(connection, counter++)).start();
      } catch (IOException e) {
        e.printStackTrace();
      }
    }
  }
}