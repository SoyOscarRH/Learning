import java.io.*;
import java.net.*;

public class Server {
  public static void main(String[] args) {
    try (final var socket = new DatagramSocket(2000)) {
      while (true) {
        var packet = new DatagramPacket(new byte[2000], 2000);
        socket.receive(packet);
        System.out.println("Datagram info " + packet.getAddress() + ":" + packet.getPort());
        final var message = new String(packet.getData(), 0, packet.getLength());
        System.out.println("With message: " + message);

        final var result = "echo " + message;
        byte[] data = result.getBytes();

        packet = new DatagramPacket(data, data.length, ip_name, port);
        socket.send(packet);

      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
