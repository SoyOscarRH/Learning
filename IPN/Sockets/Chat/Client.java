import java.io.IOException;
import java.net.*;
import java.nio.*;
import java.nio.channels.*;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Scanner;

public class Client {
  public static void main(String[] args) throws IOException {
    final var localScanner = new Scanner(System.in);

    final var channel = Helper.getDatagramChannel();

    final var selector = Selector.open();
    channel.register(selector, SelectionKey.OP_READ | SelectionKey.OP_WRITE);

    final var remote = new InetSocketAddress(Helper.host, Helper.port);
    while (true) {
      selector.select();
      final var it = selector.selectedKeys().iterator();
      while (it.hasNext()) {
        final var key = it.next();
        it.remove();
        if (!key.isWritable())
          continue;

        final var ch = (DatagramChannel) key.channel();
        final var bytes = ByteBuffer.allocate(1024);

        final var message = localScanner.nextLine();
        bytes.put(message.getBytes());
        bytes.flip();
        ch.send(bytes, remote);
      }
    }
  }
}
