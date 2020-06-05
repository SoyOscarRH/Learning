import java.io.IOException;

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