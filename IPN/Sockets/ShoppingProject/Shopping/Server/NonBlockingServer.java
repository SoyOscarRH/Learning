package Shopping.Server;

import java.net.*;
import java.net.http.WebSocket.Listener;
import java.nio.*;
import java.nio.channels.*;
import java.util.*;
import java.io.*;

import Shopping.Products;

class NonBlockingServer {
  final Selector selector;
  final ServerSocketChannel listener;

  NonBlockingServer() throws Exception {
    selector = Selector.open();
    listener = ServerSocketChannel.open();
    listener.socket().bind(new InetSocketAddress("localhost", 9090));
    listener.configureBlocking(false);
    listener.register(selector, listener.validOps(), null);
  }

  void runServer() throws Exception {
    System.out.println("Server started :D\n");
    while (true) {
      selector.select();
      final var selected = selector.selectedKeys();
      for (final var key : selected) {
        if (key.isAcceptable()) handleAccept();
        else if (key.isReadable()) handleRead((SocketChannel) key.channel());
      }
      selected.clear();
    }
  }

  void handleAccept() throws IOException {
    final var newClient = listener.accept();
    newClient.configureBlocking(false);
    newClient.register(selector, SelectionKey.OP_READ);

    System.out.println("Connection Accepted from" + newClient.socket().getRemoteSocketAddress() + "\n");
    Products.sendUpdateTo(newClient);
  }

  void handleRead(final SocketChannel client) throws Exception {
    System.out.println("message from " + client.socket().getRemoteSocketAddress());
    final var isOk = Products.updateFrom(client);
    if (isOk) {
      System.out.println("Client gave me this update:");
      Products.print();
      Products.save();

      System.out.println("Sending to others:");
      for (final var key : selector.keys()) {
        final var channel = key.channel();
        if (channel == listener) continue;
        if (channel == client) continue;
        Products.sendUpdateTo((SocketChannel) channel);
      }
    }
  }
}
