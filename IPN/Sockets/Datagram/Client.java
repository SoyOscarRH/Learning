import java.io.*;
import java.net.*;

public class Client {
  static final int serverPort = 2000;
  static final int clientPort = 2001;

  public static void main(String[] args) {
    try (final var socket = new DatagramSocket(clientPort)) {
      final var reader = new BufferedReader(new InputStreamReader(System.in));
      final var data = reader.readLine().getBytes();

      final var ip = InetAddress.getByName("127.0.0.1");
      socket.send(new DatagramPacket(data, data.length, ip, serverPort));

      final var inPacket = new DatagramPacket(new byte[2000], 2000);
      socket.receive(inPacket);
      final var message = new String(inPacket.getData(), 0, inPacket.getLength());
      System.out.println(">" + message);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
