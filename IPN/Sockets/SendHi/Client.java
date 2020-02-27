import java.io.*;
import java.net.*;
import java.util.*;

public class Client {
  public static void main(String argv[]) {
    final var localScanner = new Scanner(System.in);
    try {
      Socket socketClient = new Socket("localhost", 5555);
      System.out.println("Connection Established to server");

      final var scanner = new Scanner(socketClient.getInputStream(), "UTF-8");
      final var writeToServer = new PrintWriter(new OutputStreamWriter(socketClient.getOutputStream(), "UTF-8"), true);

      System.out.println("Server say: " + scanner.nextLine());

      while (true) {
        final var user = localScanner.nextLine();
        writeToServer.println(user);
        if ("close".equals(user)) break;

        final var serverMsg = scanner.nextLine();
        System.out.println("Server say: " + serverMsg);
      }

    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}