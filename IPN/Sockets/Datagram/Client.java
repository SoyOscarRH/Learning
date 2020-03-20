import java.io.*;
import java.lang.Math;
import java.net.*;
import java.util.Arrays;

public class Client {
  static final int serverPort = 2000;
  static final int clientPort = 2001;

  public static void main(String[] args) {
    final var reader = new BufferedReader(new InputStreamReader(System.in));

    try (final var socket = new DatagramSocket(clientPort)) {
      final var ip = InetAddress.getByName("127.0.0.1");
      final var data = reader.readLine().getBytes();

      final var packets = Math.round(Math.ceil(data.length / 20.0));
      final var split = Long.toString(packets).getBytes();
      socket.send(new DatagramPacket(split, split.length, ip, serverPort));

      for (var offset = 0; offset < data.length; offset += 20) {
        final var local_data = Arrays.copyOfRange(data, offset, offset + 20);
        socket.send(new DatagramPacket(local_data, local_data.length, ip, serverPort));
      }

      final var inPacket = new DatagramPacket(new byte[20000], 20000);
      socket.receive(inPacket);
      final var message = new String(inPacket.getData(), 0, inPacket.getLength());
      System.out.println(">" + message);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
