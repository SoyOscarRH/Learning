import java.rmi.registry.Registry;
import java.rmi.registry.LocateRegistry;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

import java.rmi.registry.Registry;
import java.rmi.registry.LocateRegistry;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

public class Server implements Calculator {
  public int sum(int a, int b) { return a + b; }
  public int substract(int a, int b) { return a - b; }
  public int multiply(int a, int b) { return a * b; }
  public float divide(int a, int b) { return (float)a / (float)b; }

  public static void main(String args[]) {
    try {
      java.rmi.registry.LocateRegistry.createRegistry(1099);
      System.out.println("RMI ready");
      System.setProperty("java.rmi.server.codebase", "file:/../server_files");
      final var obj = new Server();
      final var stub = (Calculator) UnicastRemoteObject.exportObject(obj, 0);
      Registry registry = LocateRegistry.getRegistry();
      registry.bind("Calculator", stub);

      System.err.println("Server ready n.n");
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}