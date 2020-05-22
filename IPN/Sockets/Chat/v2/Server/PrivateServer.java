import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.util.Hashtable;

class PrivateServer {
  static Hashtable<String, Integer> host_to_port;
  private static DatagramSocket server1;

  public static void startServer() {
    host_to_port = new Hashtable<String, Integer>();

    try {
      server1 = new DatagramSocket(9709);
      System.out.println("Private server is online");

      while (true) {
        final var packet = new DatagramPacket(new byte[1024], 1024);
        server1.receive(packet);
        String message = new String(packet.getData(), 0, packet.getLength());

        System.out.printf("private message from: %s:%s\n", packet.getAddress(), packet.getPort());
        System.out.printf("\tdata: %s\n\n", message);

        String[] server = message.split(" ");
        final var message_type = server[0].toLowerCase();

        message = "";

        if (message_type.equals("<init>")) {
          for (int i = 1; i < server.length; i++) message += server[i];
          host_to_port.put(message, packet.getPort());
        }

        if (message_type.equals("<msg>")) {
          for (int i = 2; i < server.length; i++) message += server[i] + " ";
          final var user = server[1];
          message = Emotion.replaceEmotions(message);
          byte[] raw = message.getBytes();
          final var address = InetAddress.getByName("127.0.0.1");

          server1.send(new DatagramPacket(raw, raw.length, packet.getAddress(), packet.getPort()));
          server1.send(new DatagramPacket(raw, raw.length, address, host_to_port.get(user)));
        }
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
