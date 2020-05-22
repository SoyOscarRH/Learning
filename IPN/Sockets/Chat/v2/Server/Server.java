
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
    Runnable t = () -> Private.PrivateServerSocket();
    new Thread(t).start();

    try {
      onlineUserNames = new ArrayList<String>();
      System.setProperty("java.net.preferIPv4Stack", "true");
      server = new MulticastSocket(port_server);
      System.out.println("\n\tMulticast service initialized...");
      group = null;
      try {
        group = InetAddress.getByName(address);
      } catch (UnknownHostException e) {
        System.err.println("\n\tInvalid address.");
        System.exit(0);
      } // End try - catch.
      server.joinGroup(group);
      server.setTimeToLive(200);
      while (true) {
        DatagramPacket p = new DatagramPacket(new byte[1500], 1500);
        server.receive(p);
        String msg = new String(p.getData(), 0, p.getLength());
        System.out.println("\n\tMessage received from: " + p.getAddress() + " : " + p.getPort()
            + "\n\tMessage: " + msg);
        Type(msg);
        try {
          Thread.sleep(5000);
        } catch (InterruptedException ie) {
        } // End try - catch.
      } // End for.
    } catch (Exception e) {
      e.printStackTrace();
    } // End try - catch.

  } // End main.

  /* The type of the message received from the client
   * can be <msg>, <init> or <private>. If it'server <msg>, the
   * server will send the message to the common chat window,
   * <init> if a new user wants to join to the conversation,
   * and <private> to open a personal chat with another user.
   */

  public static void Type(String msg) throws IOException {
    String[] sp = msg.split(" ");
    msg = "";

    if (sp[0].equalsIgnoreCase("<init>")) {
      String type = "<init>";
      String userName = "";
      byte[] b = type.getBytes();
      byte[] b1;
      String user;
      DatagramPacket p = new DatagramPacket(b, b.length, group, port_client);
      server.send(p);
      for (int i = 1; i < sp.length; i++) {
        userName = userName + sp[i] + " ";
      } // End for.
      onlineUserNames.add(userName);
      sendNumberOfOnlineUsersToClient();
      for (int i = 0; i < onlineUserNames.size(); i++) {
        user = onlineUserNames.get(i);
        b1 = user.getBytes();
        DatagramPacket p1 = new DatagramPacket(b1, b1.length, group, port_client);
        server.send(p1);
      } // End for.
    } // End if.

    if (sp[0].equalsIgnoreCase("<msg>")) {
      String type = "<msg>";
      String aux = "";
      byte[] b1 = type.getBytes();
      DatagramPacket p1 = new DatagramPacket(b1, b1.length, group, port_client);
      server.send(p1);
      for (int i = 1; i < sp.length; i++) {
        msg = msg + sp[i] + " ";
      } // End for.
      aux = Emotion.replaceEmotions(msg);
      msg = aux;
      byte[] b = msg.getBytes();
      DatagramPacket p = new DatagramPacket(b, b.length, group, port_client);
      server.send(p);
    } // End if.

    if (sp[0].equalsIgnoreCase("<private>")) {
      String msgFrom = sp[sp.length - 1];
      String msgFor = "";
      for (int i = 1; i < sp.length - 2; i++) {
        msgFor = msgFor + sp[i];
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

  public static void sendNumberOfOnlineUsersToClient() throws IOException {
    try (final var byteStream = new ByteArrayOutputStream()) {
      try (final var stream = new DataOutputStream(byteStream)) {
        stream.writeInt(onlineUserNames.size());
        stream.flush();

        final var data = byteStream.toByteArray();
        server.send(new DatagramPacket(data, data.length, group, port_client));
      }
    }
  }
}
