package Main;
import ClientSocket.Client;
import GUI.StartDialog;
import GUI.Private;

public class Main {
  public static void main(final String[] args) {
    final var cl = new Client();
    cl.Launch();
    Private.Launch();
    StartDialog.show();
  }
}
