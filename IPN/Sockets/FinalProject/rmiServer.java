import java.rmi.registry.Registry;
import java.rmi.registry.LocateRegistry;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.ArrayList;
import java.rmi.registry.Registry;
import java.rmi.registry.LocateRegistry;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

class rmiServer implements finder {
  public ArrayList<result> getList(final String filename, final int portStarter) {
    final var results = new ArrayList<result>();
    results.add(new result("23223", 10));
    return results;
  }

  public static void startingServer(final int port) {
    try {
      java.rmi.registry.LocateRegistry.createRegistry(port + 100);
      final var me = new rmiServer();
      final var stub = (finder) UnicastRemoteObject.exportObject(me, 0);
      Registry registry = LocateRegistry.getRegistry();
      registry.bind("finder", stub);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}