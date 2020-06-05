import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.io.IOException;

public class Client {
  public static void main(final String[] args) throws Exception {
    final var channel = SocketChannel.open(new InetSocketAddress("localhost", 9090));
    channel.configureBlocking(false);

    // keep
    new Thread(() -> {
      System.out.println("Client started :D");

      try (final var selector = Selector.open()) {
        channel.register(selector, channel.validOps(), null);

        while (true) {
          Thread.sleep(100);
          if (selector.select() == 0)
            continue;

          final var buffer = ByteBuffer.allocate(1024);
          final var bytes = channel.read(buffer);
          if (bytes == 0) continue;
          final var data = new String(buffer.array());

          System.out.println("Reading...");
          System.out.println("Received message: " + data);
        }
      } catch (Exception e) {
        e.printStackTrace();
      }

    }).start();

    //
    var i = Integer.valueOf(0);
    while (true) {
      final var message = "Hello" + i.toString();
      i++;
      final var data = ByteBuffer.wrap(message.getBytes());
      channel.write(data);
      Thread.sleep(2000);
    }
  }
}