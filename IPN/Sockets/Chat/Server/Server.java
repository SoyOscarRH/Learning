import java.io.IOException;
import java.net.*;
import java.util.ArrayList;

class Server {
  private static int port_server = 9000, port_client = 9001;
  private static String addressName = "230.1.1.1";

  private static ArrayList<String> onlineUserNames;
  private static MulticastSocket server;
  private static InetAddress address;

  public static void main(String[] args) {
    try {
      new Thread(() -> PrivateServer.service()).start();
      onlineUserNames = new ArrayList<String>();

      address = InetAddress.getByName(addressName);
      server = new MulticastSocket(port_server);
      server.joinGroup(address);
      server.setTimeToLive(32);
      System.out.println("Server is online. Waiting for messages");

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

  private static void send(final String data) throws IOException {
    final var raw = data.getBytes();
    server.send(new DatagramPacket(raw, raw.length, address, port_client));
  }

  public static void handle_message(final String message) throws IOException {
    final var message_part = message.split(" ");
    final var message_type = message_part[0].toLowerCase();

    send(message_type);
    if (message_type.equals("<init>")) {
      final var userName = message.substring(message_part[0].length() + 1) + " ";
      onlineUserNames.add(userName);
      send(String.format("%d", onlineUserNames.size()));
      for (final var user : onlineUserNames) send(user);
    }

    if (message_type.equals("<msg>")) {
      final var actual_message = message.substring(message_part[0].length() + 1);
      final var message_with_emojis = Emotion.replaceEmotions(actual_message);
      send(message_with_emojis);
    }

    if (message_type.equals("<private>")) {
      final var userFrom = message_part[message_part.length - 1];
      var userFor = "";
      for (int i = 1; i < message_part.length - 2; i++) userFor += message_part[i];
      System.out.printf("\tprivate message for: |%s| -> |%s|\n", userFor, userFrom);

      send(userFrom);
      send(userFor);
    }
  }
}
