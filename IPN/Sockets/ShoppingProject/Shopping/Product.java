package Shopping;

@SuppressWarnings("serial")
public class Product implements java.io.Serializable {
  public String name, imageUrl;
  public int quantity;
  public double price;

  public Product(final String name, final int quantity, final double price, final String imageUrl) {
    this.name = name;
    this.quantity = quantity;
    this.imageUrl = imageUrl;
    this.price = price;
  }

  public String toString() {
    return String.format("%d %s: \t[$%.2f][%s]", quantity, name, price, imageUrl);
  }
}