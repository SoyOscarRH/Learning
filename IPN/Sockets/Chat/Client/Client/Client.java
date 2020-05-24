package Client;
import GUI.StartDialog;
import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;

public class Client {
  public static MulticastSocket server;
  public static int port_client = 9001, port_server = 9000;

  public static void send(final String data) {
    try {
      final var raw = data.getBytes();
      final var address = InetAddress.getByName("230.1.1.1");
      final var packet = new DatagramPacket(raw, raw.length, address, Client.port_server);
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

  public static void main(final String[] args) {
    try {
      server = new MulticastSocket(port_client);
      server.joinGroup(InetAddress.getByName("230.1.1.1"));
      server.setTimeToLive(200);

      StartDialog.show();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
