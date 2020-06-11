package Shopping.Server;

import java.net.*;
import java.nio.*;
import java.nio.channels.*;
import java.util.*;
import java.io.*;

import Shopping.Products;

class NonBlockingServer {
  static void runServer() throws Exception {
    final var listener = ServerSocketChannel.open();
    listener.socket().bind(new InetSocketAddress("localhost", 9090));
    listener.configureBlocking(false);

    final var selector = Selector.open();
    listener.register(selector, listener.validOps(), null);
    System.out.println("Server started :D\n");
    while (true) {
      selector.select();
      final var selected = selector.selectedKeys();
      for (final var key : selected) {
        if (key.isAcceptable())
          handleAccept(listener, selector);
        else if (key.isReadable())
          handleRead((SocketChannel) key.channel());
      }
      selected.clear();
    }
  }

  static void handleAccept(final ServerSocketChannel listener, final Selector selector) throws IOException {
    final var newClient = listener.accept();
    newClient.configureBlocking(false);
    newClient.register(selector, SelectionKey.OP_READ);

    System.out.println("Connection Accepted from" + newClient.socket().getRemoteSocketAddress() + "\n");
    Products.sendUpdateTo(newClient);
  }

  static void handleRead(final SocketChannel client) throws Exception {
    System.out.println("message from " + client.socket().getRemoteSocketAddress());
    final var isOk = Products.updateFrom(client);
    if (isOk) {
      System.out.println("Client gave me this update:");
      Products.print();
      Products.save();
    }
  }
}
