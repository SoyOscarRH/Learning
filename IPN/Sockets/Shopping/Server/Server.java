import java.io.IOException;

class Server {
  public static void main(final String[] args) throws IOException {
    Products.createInitialProducts();
    final var data = Products.get();
    data.forEach(System.out::println);
    new NonBlockingServer().runServer();
  }
}