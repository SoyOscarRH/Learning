import java.io.*;
import java.net.*;

public class Client {
  static final String ip = "127.0.0.1";
  static final int port = 2000;

  public static void main(String[] args) {
    try (DatagramSocket socket = new DatagramSocket()) {
      final var reader = new BufferedReader(new InputStreamReader(System.in));
      final var message = reader.readLine();
      byte[] data = message.getBytes();

      final var ip_name = InetAddress.getByName(ip);
      final var packet = new DatagramPacket(data, data.length, ip_name, port);
      socket.send(packet);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
