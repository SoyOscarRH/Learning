import java.io.*;
import java.net.*;

public class Server {
  static final int serverPort = 2000;
  static final int clientPort = 2001;

  public static void main(String[] args) {
    try (final var socket = new DatagramSocket(serverPort)) {
      while (true) {
        final var inPacket = new DatagramPacket(new byte[2000], 2000);
        socket.receive(inPacket);
        System.out.println(">" + inPacket.getAddress() + ":" + inPacket.getPort());
        final var message = new String(inPacket.getData(), 0, inPacket.getLength());
        System.out.println("With message: " + message);

        final var data = "echo " + message;.getBytes();

        final var ip = InetAddress.getByName("127.0.0.1");
        final var outPacket = new DatagramPacket(data, data.length, ip, clientPort);
        socket.send(outPacket);
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
