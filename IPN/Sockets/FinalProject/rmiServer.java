import java.rmi.registry.Registry;
import java.rmi.registry.LocateRegistry;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.ArrayList;
import java.rmi.registry.Registry;
import java.rmi.registry.LocateRegistry;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.io.File;

class rmiServer implements finder {
  final private int port;

  rmiServer(int port) {
    this.port = port;
  }

  public ArrayList<result> getList(final String filename, final int portStarter) {
    final var results = new ArrayList<result>();
    if (port == portStarter)
      return results;

    final var others = rmiClient.callServer(multiCastManager.portNext + 100, port);
    final var searching = String.format("./%d/%s", port, filename);
    if ((new File(searching)).exists()) {
      try {
        final var md5 = MD5Checksum.getMD5Checksum(searching);
        results.add(new result(md5, port));
      } catch (Exception e) {
        e.printStackTrace();
      }
    }
    return others;
  }

  public static void startingServer(final int port) {
    try {
      final var me = new rmiServer(port);
      final var stub = (finder) UnicastRemoteObject.exportObject(me, 0);
      final var registry = java.rmi.registry.LocateRegistry.createRegistry(port);
      registry.rebind("finder", stub);
      System.out.printf("RMI server is online.\n", port);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}