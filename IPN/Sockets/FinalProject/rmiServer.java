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
  final private int rmiServer;

  rmiServer(int port) {
    this.port = port;
    this.rmiServer = port + 100;
  }

  public ArrayList<result> getList(final String filename, final int origin) {
    System.out.printf("[ID=%d] is searching %s, origin is %d\n", port, filename, origin);
    if (port == origin)
      return new ArrayList<result>();

    final var others = rmiClient.findFile(filename, multiCastManager.portNext + 100, origin, port);
    final var searching = String.format("./%d/%s", port, filename);
    if ((new File(searching)).exists()) {
      try {
        final var md5 = MD5Checksum.getMD5Checksum(searching);
        others.add(new result(md5, port));
      } catch (Exception e) {
        e.printStackTrace();
      }
    }

    return others;
  }

  public static void startServer(final int port) {
    try {
      final var me = new rmiServer(port);
      final var stub = (finder) UnicastRemoteObject.exportObject(me, 0);
      final var registry = java.rmi.registry.LocateRegistry.createRegistry(me.rmiServer);
      registry.rebind("finder", stub);
      System.out.printf("RMI server is online.\n");
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}