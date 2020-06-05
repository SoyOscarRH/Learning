package Shopping.Server;

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.util.Iterator;
import java.io.IOException;
import java.nio.ByteBuffer;
import Shopping.Products;

class NonBlockingServer {
  void runServer() {
    try (final var selector = Selector.open()) {
      final var channel = ServerSocketChannel.open();
      channel.socket().bind(new InetSocketAddress("localhost", 9090));
      channel.configureBlocking(false);

      channel.register(selector, channel.validOps(), null);
      System.out.println("Server started :D");
      while (true) {
        selector.select();
        final var selected = selector.selectedKeys();
        for (final var key : selected) {
          if (key.isAcceptable()) handleAccept(channel, selector);
          if (key.isReadable()) handleRead((SocketChannel) key.channel());
        }
        selected.clear();
      }
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }

  void handleAccept(final ServerSocketChannel listener, final Selector selector) throws IOException {
    final var newClient = listener.accept();
    final var id = newClient.socket().getRemoteSocketAddress();
    
    System.out.println("\nConnection Accepted..." + id);

    newClient.configureBlocking(false);
    newClient.register(selector, SelectionKey.OP_READ);

    newClient.write(Products.getBytes());
  }

  void handleRead(final SocketChannel client) throws IOException {
    final var id = client.socket().getRemoteSocketAddress();
    System.out.println("Reading from " + id);
    final var buffer = ByteBuffer.allocate(20480);
    final var endOfStream = client.read(buffer);
    final var data = new String(buffer.array());

    if (data.length() == 0 || data.equalsIgnoreCase("exit") || endOfStream == -1) {
      client.close();
      System.out.println("Connection closed...\n");
    } else
      System.out.println("Received message: " + data + "\n");
  }
}
