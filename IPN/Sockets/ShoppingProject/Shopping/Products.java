package Shopping;

import java.io.*;
import java.nio.*;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;

public class Products {
  static final String pathToSave = "./data.ser";
  static public ArrayList<Product> products;

  public static void print() {
    System.out.println("Products:");
    products.forEach(System.out::println);
    System.out.printf("");
  }

  public static void save() throws IOException {
    try (final var file = new FileOutputStream(pathToSave)) {
      try (final var out = new ObjectOutputStream(file)) {
        out.writeObject(products);
        System.out.printf("Serialized data is saved\n");
      }
    }
  }

  @SuppressWarnings(value = "unchecked")
  public static void load() throws Exception {
    try (final var fileIn = new FileInputStream(pathToSave)) {
      try (final var in = new ObjectInputStream(fileIn)) {
        products = (ArrayList<Product>) in.readObject();
        System.out.printf("Serialized data retrieved\n");
      }
    }
  }

  @SuppressWarnings(value = "unchecked")
  public static void updateFrom(final SocketChannel channel) throws Exception {
    final var buffer = ByteBuffer.allocate(20480);
    final var bytes = channel.read(buffer);
    if (bytes < 1) {
      if (bytes == 0) return;
      channel.close();
      System.out.println("Connection closed...\n");
      return;
    }

    try (final var byteStream = new ByteArrayInputStream(buffer.array())) {
      try (final var objectStream = new ObjectInputStream(byteStream)) {
        products = (ArrayList<Product>) objectStream.readObject();
      }
    }
  }

  public static void sendUpdateTo(final SocketChannel channel) throws IOException {
    try (final var byteStream = new ByteArrayOutputStream()) {
      try (final var objectStream = new ObjectOutputStream(byteStream)) {
        objectStream.writeObject(products);
        objectStream.flush();
        channel.write(ByteBuffer.wrap(byteStream.toByteArray()));
      }
    }
  }
}