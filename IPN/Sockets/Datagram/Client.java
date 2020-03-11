import java.io.*;
import java.net.*;

public class Client {
  static final String ip = "127.0.0.1";
  static final int myPort = 2001;
  static final int otherPort = 2000;

  public static void main(String[] args) {
    try (DatagramSocket socket = new DatagramSocket(2001)) {
      final var reader = new BufferedReader(new InputStreamReader(System.in));
      var message = reader.readLine();
      byte[] data = message.getBytes();

      final var ip_name = InetAddress.getByName(ip);
      final var packet = new DatagramPacket(data, data.length, ip_name, otherPort);
      socket.send(packet);

      final var inPacket = new DatagramPacket(new byte[2000], 2000);
      socket.receive(inPacket);
      message = new String(inPacket.getData(), 0, inPacket.getLength());
      System.out.println("With message: " + message);

    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
