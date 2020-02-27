import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Scanner;

public class Server {
  public static void main(String[] args) {
    connectToServer();
  }

  public static void connectToServer() {
    try (ServerSocket serverSocket = new ServerSocket(9991)) {
      final var connectionSocket = serverSocket.accept();

      final var inputToServer = connectionSocket.getInputStream();
      final var outputFromServer = connectionSocket.getOutputStream();

      final var scanner = new Scanner(inputToServer, "UTF-8");
      final var serverPrintOut = new PrintWriter(new OutputStreamWriter(outputFromServer, "UTF-8"), true);

      serverPrintOut.println("Hello World! Enter Peace to exit.");

      var done = false;
      while (!done && scanner.hasNextLine()) {
        final var line = scanner.nextLine();
        serverPrintOut.println("Echo <Oscar>: " + line);

        if (line.toLowerCase().trim().equals("peace")) {
          done = true;
        }
      }
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}