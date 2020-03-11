import java.io.*;
import java.net.*;

public class Server {
  static final String ip = "127.0.0.1";
  static final int myPort = 2000;
  static final int otherPort = 2001;

  public static void main(String[] args) {
    try (final var socket = new DatagramSocket(myPort)) {
      while (true) {
        final var inPacket = new DatagramPacket(new byte[2000], 2000);
        socket.receive(inPacket);
        System.out.println("Datagram info " + inPacket.getAddress() + ":" + inPacket.getPort());
        final var message = new String(inPacket.getData(), 0, inPacket.getLength());
        System.out.println("With message: " + message);

        final var result = "echo " + message;
        byte[] data = result.getBytes();

        final var outPacket = new DatagramPacket(data, data.length, ip, otherPort);
        socket.send(outPacket);

      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
