import java.io.*;
import java.net.*;
import javax.swing.JFileChooser;

public class Client {
  static final String host = "10.100.76.1";
  static final int port = 7000;

  public static void main(String[] args) {
    final var fileChooser = new JFileChooser();
    if (fileChooser.showOpenDialog(null) != JFileChooser.APPROVE_OPTION)
      return;

    try (final var socket = new Socket(host, port)) {
      final File file = fileChooser.getSelectedFile(); 
      final String filename = file.getName();         
      final long size = file.length();              

      final var out = new DataOutputStream(socket.getOutputStream());
      final var in = new DataInputStream(new FileInputStream(file.getAbsolutePath()));

      out.writeUTF(filename);
      out.flush();
      out.writeLong(size);
      out.flush();

      var buffer = new byte[1024];
      long bytesSend = 0;
      int percentage, send;

      while (bytesSend < size) {
        send = in.read(buffer);
        out.write(buffer, 0, send);
        out.flush();

        bytesSend = bytesSend + send;
        percentage = (int) (bytesSend * 100 / size);
        System.out.println("Progress: " + percentage + "%");
      }

      System.out.println("File " + filename + " send!");

      out.close();
      in.close();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}