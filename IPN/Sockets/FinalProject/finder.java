import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.ArrayList;

public interface finder extends Remote {
  ArrayList<result> getList(final String filename, final int portStarter) throws RemoteException;
}
