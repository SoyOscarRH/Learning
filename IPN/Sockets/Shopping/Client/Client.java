import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.util.Iterator;
import java.io.IOException;

class ClientServer extends Thread {
  final SocketChannel client;

  ClientServer(final SocketChannel client) {
    this.client = client;
  }

  public void run() {
    try (final var selector = Selector.open()) {
      this.client.configureBlocking(false);
      this.client.register(selector, this.client.validOps(), null);

      System.out.println("Server started :D");
      while (true) {
        selector.select();
        final var i = selector.selectedKeys().iterator();
        while (i.hasNext()) {
          final SelectionKey key = i.next();
          if (key.isReadable()) {
            System.out.println("Reading...");
            final var buffer = ByteBuffer.allocate(1024);
            this.client.read(buffer);
            final var data = new String(buffer.array());

            System.out.println("Received message: " + data);
          }
          i.remove();
        }
      }

    } catch (IOException e) {

    }
  }
}

public class Client {
  public static void main(final String[] args) throws Exception {
    final var client = SocketChannel.open(new InetSocketAddress("localhost", 9090));

    new ClientServer(client).start();
    var i = Integer.valueOf(0);
    while (true) {
      final var message = "Hello" + i.toString();
      i++;
      final var data = ByteBuffer.wrap(message.getBytes());
      client.write(data);
      Thread.sleep(2000);
    }
  }
}