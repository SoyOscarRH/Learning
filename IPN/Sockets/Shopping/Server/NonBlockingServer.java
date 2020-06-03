import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.util.Iterator;
import java.io.IOException;

class NonBlockingServer {
  void runServer() throws IOException {
    try (final var selector = Selector.open()) {
      final var socket = ServerSocketChannel.open();
      final var serverSocket = socket.socket();
      serverSocket.bind(new InetSocketAddress("localhost", 9090));
      socket.configureBlocking(false);

      socket.register(selector, socket.validOps(), null);
      System.out.println("Server started :D");
      while (true) {
        selector.select();
        final var i = selector.selectedKeys().iterator();
        while (i.hasNext()) {
          final SelectionKey key = i.next();

          if (key.isAcceptable())
            handleAccept(socket, selector, key);
          else if (key.isReadable())
            handleRead(key);

          i.remove();
        }
      }
    }
  }

  private void handleAccept(final ServerSocketChannel mySocket, final Selector selector, final SelectionKey key)
      throws IOException {
    System.out.println("\nConnection Accepted...");

    final SocketChannel client = mySocket.accept();
    client.configureBlocking(false);
    client.register(selector, SelectionKey.OP_READ);
  }

  private static void handleRead(final SelectionKey key) throws IOException {
    System.out.println("Reading...");
    final var client = (SocketChannel) key.channel();
    final var buffer = ByteBuffer.allocate(1024);
    final var endOfStream = client.read(buffer);
    final var data = new String(buffer.array());

    if (data.length() == 0 || data.equalsIgnoreCase("exit") || endOfStream == -1) {
      client.close();
      System.out.println("Connection closed...\n");
    } else
      System.out.println("Received message: " + data + "\n");
  }
}
