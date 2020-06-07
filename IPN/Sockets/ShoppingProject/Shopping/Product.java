package Shopping;

public class Product implements java.io.Serializable {
  public final String name;
  public final String imageUrl;
  public int quantity;

  Product(final String name, final int quantity, final String imageUrl) {
    this.name = name;
    this.quantity = quantity;
    this.imageUrl = imageUrl;
  }

  public String toString() {
    return String.format("%d %s: \t[%s]", quantity, name, imageUrl);
  }
}