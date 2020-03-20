import java.io.*;
import java.net.*;

public class Server {
  static final int serverPort = 2000;
  static final int clientPort = 2001;

  public static void main(String[] args) {
    try (final var socket = new DatagramSocket(serverPort)) {
      while (true) {
        final var inPacket = new DatagramPacket(new byte[20], 20);
        socket.receive(inPacket);

        final var packets_raw = new String(inPacket.getData(), 0, inPacket.getLength());
        final var packets = Integer.parseInt(packets_raw);
        
        var message = "";
        for (int i = 0; i < packets; ++i) {
          socket.receive(inPacket);
          message += new String(inPacket.getData(), 0, inPacket.getLength());
        }

        System.out.println("> Packets " + packets + " from " + inPacket.getAddress() + ":" + inPacket.getPort());
        System.out.println("  " + message);

        final var data = ("echo " + message).getBytes();
        final var ip = InetAddress.getByName("127.0.0.1");
        final var outPacket = new DatagramPacket(data, data.length, ip, clientPort);
        socket.send(outPacket);
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
