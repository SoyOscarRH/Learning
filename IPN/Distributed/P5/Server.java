import java.io.IOException;
import java.nio.ByteBuffer;
import java.net.*;

class Server {
  static void envia_mensaje(final byte[] buffer, final String ip, final int puerto) throws IOException {
    DatagramSocket socket = new DatagramSocket();
    InetAddress grupo = InetAddress.getByName(ip);
    DatagramPacket paquete = new DatagramPacket(buffer, buffer.length, grupo, puerto);
    socket.send(paquete);
    socket.close();
  }

  public static void main(String[] args) {
    try {
      envia_mensaje("hola".getBytes(), "230.0.0.0", 50000);

      final var buffer = ByteBuffer.allocate(5 * 8);
      buffer.putDouble(1.1);
      buffer.putDouble(1.2);
      buffer.putDouble(1.3);
      buffer.putDouble(1.4);
      buffer.putDouble(1.5);
      envia_mensaje(buffer.array(), "230.0.0.0", 50000);
      
    } catch (final Exception e) {
      e.printStackTrace();
      System.exit(1);
    }
  }
}
