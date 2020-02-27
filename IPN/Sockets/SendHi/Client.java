import java.io.*;
import java.net.*;
import java.util.*;

public class Client {
  public static void main(String argv[]) {
    try (final var socketClient = new Socket("127.0.0.1", 9001)) {
      final var reader = new BufferedReader(new InputStreamReader(socketClient.getInputStream()));
      final var writer = new BufferedWriter(new OutputStreamWriter(socketClient.getOutputStream()));
        
      String serverMsg;

      writer.write("hello\r\n");
      writer.flush();

      while ((serverMsg = reader.readLine()) != null) {
        System.out.println("Client: " + serverMsg);
      }

    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}