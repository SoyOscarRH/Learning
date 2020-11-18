import java.io.IOException;
import java.nio.ByteBuffer;
import java.net.*;
import java.util.*;

public class Client {

  static byte[] recibe_mensaje(final MulticastSocket socket, final int longitud_mensaje) throws IOException {
    byte[] buffer = new byte[longitud_mensaje];
    DatagramPacket paquete = new DatagramPacket(buffer, buffer.length);
    socket.receive(paquete);
    return paquete.getData();
  }

  public static void main(String[] args) {
    try {
      final var port = 50_000;
      final var ip = InetAddress.getByName("230.0.0.0");
      final var group = new InetSocketAddress(ip, port);
      final var netInterface = NetworkInterface.getByName("en0");

      final var socket = new MulticastSocket(port);
      socket.joinGroup(group, netInterface);

      final var hello = recibe_mensaje(socket, 4);
      System.out.println(new String(hello, "utf-8"));

      final var buffer = ByteBuffer.wrap(recibe_mensaje(socket, 5 * 8));

      for (var i = 0; i < 5; ++i) {
        System.out.println(buffer.getDouble());
      }

      socket.leaveGroup(group, netInterface);
      socket.close();

    } catch (final Exception e) {
      e.printStackTrace();
      System.exit(1);
    }
  }

}
