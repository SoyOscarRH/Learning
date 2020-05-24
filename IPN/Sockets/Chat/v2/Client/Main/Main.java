package Main;
import GUI.StartDialog;
import java.net.InetAddress;
import java.net.MulticastSocket;
import java.net.DatagramPacket;

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

  public static void send(final byte[] data) {
    try {
      final var packet = new DatagramPacket(data, data.length, Main.group, Main.ports);
      Main.cl.send(packet);
    } catch (Exception e) {
      System.out.println("Error sending message");
    }
  }
}
