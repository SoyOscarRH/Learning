package Shopping;

public class Product implements java.io.Serializable {
  String name;
  int quantity;
  String imageUrl;

  Product(final String name, final int quantity, final String imageUrl) {
    this.name = name;
    this.quantity = quantity;
    this.imageUrl = imageUrl;
  }

  public String toString() {
    return String.format("%s [%d] [%s]", name, quantity, imageUrl);
  }
}