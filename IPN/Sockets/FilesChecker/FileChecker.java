import java.rmi.Remote;

public interface FileChecker extends Remote {
  public boolean isTheSameFile(final String name, final String md5) throws Exception;
}
