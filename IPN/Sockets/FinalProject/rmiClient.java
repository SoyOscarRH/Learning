import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

class rmiClient {
  static void callServer(final int serverPort, final int portStarter) {
    try {
      final var registry = LocateRegistry.getRegistry(null, serverPort + 100);
      final var stub = (finder) registry.lookup("finder");

      final var results = stub.getList("3.pdf", portStarter);
      for (final var result : results) {
        System.out.printf("%s at %d", result.md5, result.port);
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
