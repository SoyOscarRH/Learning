import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;
import java.net.UnknownHostException;
import java.util.ArrayList;

public class Server {
  public static int port_server = 9000, port_client = 9001;
  public static String address = "230.1.1.1";

  protected static ArrayList<String> onlineUserNames;
  private static MulticastSocket server;
  public static InetAddress group;

  public static void main(String[] args) {
    new Thread(() -> Private.PrivateServerSocket()).start();

    onlineUserNames = new ArrayList<String>();

    try {
      group = InetAddress.getByName(address);
      server = new MulticastSocket(port_server);
      server.joinGroup(group);
      server.setTimeToLive(200);
      System.out.println("Server is online\n");

      while (true) {
        final var packet = new DatagramPacket(new byte[1024], 1024);
        server.receive(packet);
        var message = new String(packet.getData(), 0, packet.getLength());

        System.out.printf("message from: %s:%s", packet.getAddress(), packet.getPort());
        System.out.printf("\tusers: [%s]\n", String.join("|", onlineUserNames));
        System.out.printf("\tdata: %s\n\n", message);

        handle_message(message);
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /* The type of the message received from the client
   * can be <msg>, <init> or <private>. If it'server <msg>, the
   * server will send the message to the common chat window,
   * <init> if a new user wants to join to the conversation,
   * and <private> to open a personal chat with another user.
   */

  public static void handle_message(String message) throws IOException {
    final var message_part = message.split(" ");
    final var message_type = message_part[0].toLowerCase();

    if (message_type.equals("<init>")) {
      final var info = "<init>".getBytes();
      server.send(new DatagramPacket(info, info.length, group, port_client));

      final var userName = message.substring(message_part[0].length() + 1) + " ";
      onlineUserNames.add(userName);
      try (final var byteStream = new ByteArrayOutputStream()) {
        try (final var stream = new DataOutputStream(byteStream)) {
          stream.writeInt(onlineUserNames.size());
          stream.flush();

          final var data = byteStream.toByteArray();
          server.send(new DatagramPacket(data, data.length, group, port_client));
        }
      }

      for (final var user : onlineUserNames) {
        final var raw_data = user.getBytes();
        server.send(new DatagramPacket(raw_data, raw_data.length, group, port_client));
      }
    }

    if (message_type.equals("<msg>")) {
      message = "";
      String type = "<msg>";
      String aux = "";
      byte[] b1 = type.getBytes();
      DatagramPacket p1 = new DatagramPacket(b1, b1.length, group, port_client);
      server.send(p1);
      for (int i = 1; i < message_part.length; i++) {
        message = message + message_part[i] + " ";
      } // End for.
      aux = Emotion.replaceEmotions(message);
      message = aux;
      byte[] b = message.getBytes();
      DatagramPacket p = new DatagramPacket(b, b.length, group, port_client);
      server.send(p);
    } // End if.

    if (message_type.equals("<private>")) {
      String msgFrom = message_part[message_part.length - 1];
      String msgFor = "";
      for (int i = 1; i < message_part.length - 2; i++) {
        msgFor = msgFor + message_part[i];
      } // End for.
      System.out.println("\n\tPrivate Message for: " + msgFor + ". From: " + msgFrom + ".");
      String type = "<private>";
      byte[] b = type.getBytes();
      DatagramPacket p = new DatagramPacket(b, b.length, group, port_client);
      server.send(p);
      b = msgFrom.getBytes();
      DatagramPacket p1 = new DatagramPacket(b, b.length, group, port_client);
      server.send(p1);
      b = msgFor.getBytes();
      DatagramPacket p2 = new DatagramPacket(b, b.length, group, port_client);
      server.send(p2);
    } // End if.

  } // End Type.
}
