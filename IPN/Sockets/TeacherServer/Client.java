import java.io.*;
import java.net.*;
import java.util.*;

public class Client {
  public static void main(String argv[]) {
    final Scanner localScanner = new Scanner(System.in);
    try {
      Socket socketClient = new Socket("8.40.1.37", 9000);
      System.out.println("Connection Established to server");

      final var in = new DataInputStream(socketClient.getInputStream());
      final var out = new DataOutputStream(socketClient.getOutputStream());

      out.writeLong(2014090642);
      out.writeUTF("Oscar Andres Rosas Hernandez");
      out.writeInt(21);

      final var size = in.readLong();
      in.read(new byte[(int)size]);

      final var number = in.readDouble();
      out.writeDouble(number);

    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}