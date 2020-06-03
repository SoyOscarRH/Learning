import java.io.*;
import java.util.ArrayList;

class Products {
  @SuppressWarnings(value = "unchecked")
  static ArrayList<Product> get() {
    try (final var fileIn = new FileInputStream("./data.ser")) {
      try (final var in = new ObjectInputStream(fileIn)) {
        return (ArrayList<Product>) in.readObject();
      } catch (final Exception e) {
      }
    } catch (final Exception e) {
    }

    return null;
  }

  static void save(final ArrayList<Product> data) throws IOException {
    try (final var fileOut = new FileOutputStream("./data.ser")) {
      try (final var out = new ObjectOutputStream(fileOut)) {
        out.writeObject(data);
        System.out.printf("Serialized data is saved\n");
      }
    }
  }

  static void createInitialProducts() throws IOException {
    final var data = new ArrayList<Product>();
    data.add(new Product("te", 5, "te.png"));

    Products.save(data);
  }
}