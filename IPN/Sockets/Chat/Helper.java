import java.io.IOException;
import java.net.*;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.nio.channels.*;
import java.util.Collections;

class Helper {
  static int port = 2000;
  static String host = "230.0.0.1";
  static String netInterface = "en0";

  static void displayInterfaceInfo(NetworkInterface netInt) {
    System.out.printf("Interface: %s\n", netInt.getDisplayName());
    System.out.printf("Name:      %s\n", netInt.getName());

    final var addresses = Collections.list(netInt.getInetAddresses());
    for (final var inetAddress : addresses) System.out.printf("InetAddress: %s\n", inetAddress);

    System.out.printf("\n");
  }

  static DatagramChannel getDatagramChannel() throws IOException {
    final var channel = DatagramChannel.open(StandardProtocolFamily.INET);
    channel.setOption(StandardSocketOptions.SO_REUSEADDR, true);

    final var netInterface = NetworkInterface.getByName(Helper.netInterface);
    channel.setOption(StandardSocketOptions.IP_MULTICAST_IF, netInterface);
    channel.configureBlocking(false);

    final var group = InetAddress.getByName(Helper.host);
    channel.join(group, netInterface);

    return channel;
  }
}
