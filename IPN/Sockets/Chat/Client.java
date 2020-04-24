import java.io.IOException;
import java.net.*;
import java.nio.*;
import java.nio.channels.*;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;

public class Client {
  public static void main(String[] args) throws IOException {
    final var nets = Collections.list(NetworkInterface.getNetworkInterfaces());
    for (final var netInterface : nets) Helper.displayInterfaceInfo(netInterface);

    final var channel = DatagramChannel.open(StandardProtocolFamily.INET);
    channel.setOption(StandardSocketOptions.SO_REUSEADDR, true);
    final var netInterface = NetworkInterface.getByName(Helper.netInterface);
    channel.setOption(StandardSocketOptions.IP_MULTICAST_IF, netInterface);
    channel.configureBlocking(false);

    final var selector = Selector.open();
    channel.register(selector, SelectionKey.OP_READ | SelectionKey.OP_WRITE);
    final var group = InetAddress.getByName(Helper.host);
    channel.join(group, netInterface);

    final var remote = new InetSocketAddress(Helper.host, Helper.port);
    for (var n = 0; n < 100; ++n) {
      selector.select();
      final var it = selector.selectedKeys().iterator();
      while (it.hasNext()) {
        final var key = it.next();
        it.remove();
        if (!key.isWritable())
          continue;

        final var ch = (DatagramChannel) key.channel();
        final var bytes = ByteBuffer.allocate(1024);
        bytes.put(String.format("Hello %d", n).getBytes());
        bytes.flip();
        ch.send(bytes, remote);
      }
    }

    channel.close();
    System.out.println("Termina envio de datagramas");
  }
}
