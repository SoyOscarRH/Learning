package Shopping;

import java.io.*;
import java.nio.*;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;
import java.io.FileNotFoundException;

public class Products {
  static final String pathToSave = "./data.ser";
  static public ArrayList<Product> products;

  public static void print() {
    products.forEach(System.out::println);
    System.out.println("");
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
    catch (FileNotFoundException e) {
      products = new ArrayList<Product>();
    }
  }

  @SuppressWarnings(value = "unchecked")
  public static boolean updateFrom(final SocketChannel channel) throws Exception {
    final var buffer = ByteBuffer.allocate(20480);
    final var bytes = channel.read(buffer);
    if (bytes < 1) {
      if (bytes < 0) {
        channel.close();
        System.out.println("Connection closed...\n");
      }
      return false;
    }

    try (final var byteStream = new ByteArrayInputStream(buffer.array())) {
      try (final var objectStream = new ObjectInputStream(byteStream)) {
        products = (ArrayList<Product>) objectStream.readObject();
      }
    }

    return true;
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