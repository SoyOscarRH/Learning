import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.util.Hashtable;

class PrivateServer {
  public static void service() {
    try (final var server = new DatagramSocket(9709)) {
      System.out.println("Private server is online. Waiting for messages");
      final var host_to_port = new Hashtable<String, Integer>();

      while (true) {
        final var packet = new DatagramPacket(new byte[1024], 1024);
        server.receive(packet);
        String message = new String(packet.getData(), 0, packet.getLength());

        System.out.printf("private message from: %s:%s\n", packet.getAddress(), packet.getPort());
        System.out.printf("\tdata: %s\n\n", message);

        String[] message_part = message.split(" ");
        final var message_type = message_part[0].toLowerCase();
        message = "";

        if (message_type.equals("<init>")) {
          for (int i = 1; i < message_part.length; i++) message += message_part[i];
          host_to_port.put(message, packet.getPort());
        }

        if (message_type.equals("<msg>")) {
          for (int i = 2; i < message_part.length; i++) message += message_part[i] + " ";
          final var user = message_part[1];
          final var address = InetAddress.getByName("127.0.0.1");
          byte[] raw = Emotion.replaceEmotions(message).getBytes();

          server.send(new DatagramPacket(raw, raw.length, packet.getAddress(), packet.getPort()));
          server.send(new DatagramPacket(raw, raw.length, address, host_to_port.get(user)));
        }
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
