import java.rmi.*;
import java.rmi.registry.*;

class ServidorRMI {
  public static void main(String[] args) throws Exception {
    Naming.rebind("rmi://localhost/prueba", new ClaseRMI());
    System.out.println("Server Ready");
  }
}

