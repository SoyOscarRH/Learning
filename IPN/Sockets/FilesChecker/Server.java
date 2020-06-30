import java.rmi.registry.Registry;
import java.rmi.registry.LocateRegistry;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

import java.rmi.registry.Registry;
import java.rmi.registry.LocateRegistry;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

public class Server implements FileChecker {
  public float divide(int a, int b) { return (float)a / (float)b; }
  public boolean isTheSameFile(final String name, final String md5) {
    try {
      return md5.equals(MD5Checksum.getMD5Checksum("./server/" + name));
    }
    catch (Exception e) {
      return false;
    }
  }

  public static void main(String args[]) {
    try {
      java.rmi.registry.LocateRegistry.createRegistry(1099);
      System.out.println("RMI ready");
      final var obj = new Server();
      final var stub = (FileChecker) UnicastRemoteObject.exportObject(obj, 0);
      LocateRegistry.getRegistry().bind("FileChecker", stub);

      System.err.println("Server ready n.n");
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}