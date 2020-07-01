import java.lang.reflect.Array;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.ArrayList;

class rmiClient {
  static ArrayList<result> findFile(final String name, final int where, final int origin, int port) {
    var results = new ArrayList<result>();
    try {
      System.out.printf("calling node %d to see if it has file\n", where);
      final var registry = LocateRegistry.getRegistry(where);
      final var stub = (finder) registry.lookup("finder");

      results = stub.getList(name, origin);
    } catch (Exception e) {
      e.printStackTrace();
    }
    return results;
  }
}
