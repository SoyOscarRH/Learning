package Shopping.Server;

import java.io.IOException;
import Shopping.Products;

class Server {
  public static void main(final String[] args) {
    try {
      Products.load();
      new NonBlockingServer().runServer();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}