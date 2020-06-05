import java.io.*;
import java.util.ArrayList;

class Products {
  static ArrayList<Product> products;
  static String pathToSave = "./data.ser";

  @SuppressWarnings(value = "unchecked")
  static void load() throws Exception {
    if (new File(pathToSave).exists() == false) {
      products = new ArrayList<Product>();
      products.add(new Product("te", 5, "te.png"));
      Products.save();
    }

    try (final var fileIn = new FileInputStream(pathToSave)) {
      try (final var in = new ObjectInputStream(fileIn)) {
        products = (ArrayList<Product>) in.readObject();
      }
    }

    products.forEach(System.out::println);
  }

  static void save() throws IOException {
    try (final var fileOut = new FileOutputStream("./data.ser")) {
      try (final var out = new ObjectOutputStream(fileOut)) {
        out.writeObject(products);
        System.out.printf("Serialized data is saved\n");
      }
    }
  }
}