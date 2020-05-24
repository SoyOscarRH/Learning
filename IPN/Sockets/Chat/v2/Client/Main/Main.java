package Main;
import GUI.StartDialog;
import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;

public class Main {
  public static MulticastSocket cl;
  public static int portc = 9001, ports = 9000;
  public static String address = "230.1.1.1";
  public static InetAddress group;

  public static void main(final String[] args) {
    try {
      cl = new MulticastSocket(portc);
      group = InetAddress.getByName(address);
      cl.joinGroup(group);
      cl.setTimeToLive(200);

      StartDialog.show();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  public static void send(final String data) {
    try {
      final var raw = data.getBytes();
      final var packet = new DatagramPacket(raw, raw.length, Main.group, Main.ports);
      Main.cl.send(packet);
    } catch (Exception e) {
      System.out.println("Error sending message");
    }
  }

  public static String receive() {
    var result = "";
    try {
      final var packet = new DatagramPacket(new byte[1024], 1024);
      Main.cl.receive(packet);
      result = new String(packet.getData(), 0, packet.getLength());
    } catch (Exception e) {
      System.out.println("Error getting message");
    }

    return result;
  }
}
