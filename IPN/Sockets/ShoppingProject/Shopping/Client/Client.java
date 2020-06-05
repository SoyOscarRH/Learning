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

public class Client {
  static ArrayList<Product> products;

  @SuppressWarnings(value = "unchecked")
  static void setup(byte[] data) throws Exception {
    try (final var byteStream = new ByteArrayInputStream(data)) {
      try (final var objectStream = new ObjectInputStream(byteStream)) {
        products = (ArrayList<Product>) objectStream.readObject();
      }
    }

    products.forEach(System.out::println);
  }

  public static void main(final String[] args) throws Exception {
    final var channel = SocketChannel.open(new InetSocketAddress("localhost", 9090));
    channel.configureBlocking(false);

    new Thread(() -> {
      var products = true;
      System.out.println("Client started :D");

      try (final var selector = Selector.open()) {
        channel.register(selector, channel.validOps(), null);
        while (true) {
          Thread.sleep(100);
          if (selector.select() == 0)
            continue;

          final var buffer = ByteBuffer.allocate(1024);
          final var bytes = channel.read(buffer);

          if (bytes == 0)
            continue;

          if (products) {
            setup(buffer.array());
            products = false;
          } else {
            final var data = new String(buffer.array());
            System.out.println("Reading...\nReceived message: " + data);
          }
        }
      } catch (final Exception e) {
        e.printStackTrace();
      }
    }).start();

  }
}