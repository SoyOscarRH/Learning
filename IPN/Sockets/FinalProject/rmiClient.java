import java.lang.reflect.Array;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.ArrayList;


class rmiClient {
  static ArrayList<result> callServer(final int serverPort, final int portStarter) {
    try {
      final var registry = LocateRegistry.getRegistry(serverPort);
      final var stub = (finder) registry.lookup("finder");

      final var results = stub.getList("3.pdf", portStarter);
      for (final var result : results) {
        System.out.printf("%s at %d", result.md5, result.port);
      }

      return results;
    } catch (Exception e) {
      e.printStackTrace();
      return new ArrayList<>();
    }
  }
}
