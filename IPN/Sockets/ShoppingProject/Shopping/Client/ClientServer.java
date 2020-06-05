package Shopping.Client;

import java.io.*;
import java.nio.*;

import java.net.InetSocketAddress;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;
import java.io.IOException;

import Shopping.Products;
import Shopping.Product;

public class ClientServer {
  static ArrayList<Product> products;

  @SuppressWarnings(value = "unchecked")
  static void syncProducts(byte[] data) throws Exception {
    try (final var byteStream = new ByteArrayInputStream(data)) {
      try (final var objectStream = new ObjectInputStream(byteStream)) {
        products = (ArrayList<Product>) objectStream.readObject();
      }
    }

    System.out.println("Update on products:");
    products.forEach(System.out::println);
    System.out.println("");
  }

  static void startServer() {
    try {
      final var channel = SocketChannel.open(new InetSocketAddress("localhost", 9090));
      channel.configureBlocking(false);
      final var selector = Selector.open();
      channel.register(selector, channel.validOps(), null);

      System.out.println("Client started :D");
      while (true) {
        if (selector.select() == 0) {
          Thread.sleep(100);
          continue;
        }

        final var buffer = ByteBuffer.allocate(20480);
        if (channel.read(buffer) != 0) syncProducts(buffer.array());
      }
    } catch (final Exception e) {
      e.printStackTrace();
    }
  }
}