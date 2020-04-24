import java.io.IOException;
import java.net.*;
import java.nio.*;
import java.nio.channels.*;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;

public class Server {
  public static void main(String[] args) throws SocketException, IOException {
    final var channel = Helper.getDatagramChannel();
    channel.socket().bind(new InetSocketAddress(Helper.port));

    final var selector = Selector.open();
    channel.register(selector, SelectionKey.OP_READ);

    System.out.println("Server ready... waiting for datagrams");
    while (true) {
      selector.select();
      final var it = selector.selectedKeys().iterator();

      while (it.hasNext()) {
        final var key = it.next();
        it.remove();

        if (!key.isReadable())
          continue;

        final var ch = (DatagramChannel) key.channel();
        final var bytes = ByteBuffer.allocate(1024);

        final var d = (InetSocketAddress) ch.receive(bytes);
        bytes.flip();

        System.out.println("Datagram send from " + d.getAddress() + ":" + d.getPort());
        final var data = new String(bytes.array(), "UTF-8");
        System.out.println("Data: " + data);
      }
    }
  }
}
