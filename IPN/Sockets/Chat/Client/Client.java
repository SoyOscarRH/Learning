import java.net.*;

public class Client {
  public static MulticastSocket server;
  public static int portClient = 9001, portServer = 9000;
  private static InetAddress address;

  public static void main(final String[] args) {
    try {
      address = InetAddress.getByName("230.1.1.1");
      server = new MulticastSocket(portClient);
      server.joinGroup(address);
      server.setTimeToLive(200);

      StartDialog.show();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  public static void send(final String data) {
    try {
      final var raw = data.getBytes();
      address = InetAddress.getByName("230.1.1.1");
      final var packet = new DatagramPacket(raw, raw.length, address, Client.portServer);
      Client.server.send(packet);
    } catch (Exception e) {
      System.out.println("Error sending message");
    }
  }

  public static String receive() {
    var result = "";
    try {
      final var packet = new DatagramPacket(new byte[1024], 1024);
      Client.server.receive(packet);
      result = new String(packet.getData(), 0, packet.getLength());
    } catch (Exception e) {
      System.out.println("Error getting message");
    }

    return result;
  }
}
