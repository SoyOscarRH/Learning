import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.util.Hashtable;

class PrivateServer {
  static Hashtable<String, Integer> host_to_port;
  private static DatagramSocket server;

  public static void startServer() {
    host_to_port = new Hashtable<String, Integer>();

    try {
      server = new DatagramSocket(9709);
      System.out.println("Private server is online");

      while (true) {
        final var packet = new DatagramPacket(new byte[1024], 1024);
        server.receive(packet);
        String message = new String(packet.getData(), 0, packet.getLength());

        System.out.printf("private message from: %s:%s", packet.getAddress(), packet.getPort());
        System.out.printf("\tdata: %s\n\n", message);

        String[] server = message.split(" ");
        String msg = "";

        if (server[0].equalsIgnoreCase("<init>")) {
          for (int i = 1; i < server.length; i++) msg = msg + server[i];
          host_to_port.put(msg, packet.getPort());
        }

        if (server[0].equalsIgnoreCase("<msg>")) {
          for (int i = 2; i < server.length; i++) msg = msg + server[i] + " ";
          final var user = server[1];
          Message(packet, msg, user);
        }
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  public static void Message(DatagramPacket p, String msg, String user) throws IOException {
    msg = Emotion.replaceEmotions(msg);
    byte[] b = msg.getBytes();
    DatagramPacket p1 = new DatagramPacket(b, b.length, p.getAddress(), p.getPort());
    server.send(p1);
    p = new DatagramPacket(b, b.length, InetAddress.getByName("127.0.0.1"), host_to_port.get(user));
    server.send(p);
  }
}
