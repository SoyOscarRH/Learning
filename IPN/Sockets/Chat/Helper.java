import java.net.NetworkInterface;
import java.net.SocketException;
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
}
