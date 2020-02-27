import java.io.*;
import java.net.*;
import java.util.*;

public class Client {
  public static void main(String argv[]) {
    try {
      Socket socketClient = new Socket("localhost", 5555);
      System.out.println("Connection Established to server");

      final var inputToServer = socketClient.getInputStream();
      final var outputFromServer = socketClient.getOutputStream();

      final var scanner = new Scanner(inputToServer, "UTF-8");
      final var writeToServer = new PrintWriter(new OutputStreamWriter(outputFromServer, "UTF-8"), true);

      final var localScanner = new Scanner(System.in);
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