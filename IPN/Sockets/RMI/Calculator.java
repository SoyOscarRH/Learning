import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Calculator extends Remote {
  int sum(int a, int b) throws RemoteException;
  int substract(int a, int b) throws RemoteException;
  int multiply(int a, int b) throws RemoteException;
  float divide(int a, int b) throws RemoteException;
}
