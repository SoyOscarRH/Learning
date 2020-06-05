package Shopping;

import java.io.*;
import java.nio.*;
import java.util.ArrayList;

public class Products {
  static ArrayList<Product> products;
  static String pathToSave = "./data.ser";

  @SuppressWarnings(value = "unchecked")
  public static void load() throws Exception {
    if (new File(pathToSave).exists() == false) {
      products = new ArrayList<Product>();
      products.add(new Product("te", 5, "te.png"));
      products.add(new Product("jabon", 2, "jabon.png"));
      Products.save(); 
    }

    try (final var fileIn = new FileInputStream(pathToSave)) {
      try (final var in = new ObjectInputStream(fileIn)) {
        products = (ArrayList<Product>) in.readObject();
      }
    }

    products.forEach(System.out::println);
  }

  public static ByteBuffer getBytes() throws IOException {
    final var out = new ByteArrayOutputStream();
    final var writer = new ObjectOutputStream(out);
    writer.writeObject(products);
    writer.flush();

    return ByteBuffer.wrap(out.toByteArray());
  }

  public static void save() throws IOException {
    try (final var fileOut = new FileOutputStream("./data.ser")) {
      try (final var out = new ObjectOutputStream(fileOut)) {
        out.writeObject(products);
        System.out.printf("Serialized data is saved\n");
      }
    }
  }
}