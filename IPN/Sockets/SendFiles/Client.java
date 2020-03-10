import java.awt.*;
import java.io.*;
import java.net.*;
import javax.swing.*;

public class Client {
  static final String host = "10.100.76.1";
  static final int port = 7000;
  static final int bufferSize = 2048;
  static final boolean Nagle = false;

  public static void main(String[] args) {
    final var fileChooser = new JFileChooser();
    fileChooser.setMultiSelectionEnabled(true);
    if (fileChooser.showOpenDialog(null) != JFileChooser.APPROVE_OPTION)
      return;

    try (final var socket = new Socket(host, port)) {
      socket.setTcpNoDelay(Nagle);
      final var out = new DataOutputStream(socket.getOutputStream());
      final var files = fileChooser.getSelectedFiles();

      out.writeInt(bufferSize);
      out.flush();
      out.writeInt(files.length);
      out.flush();

      for (final var file : files) {
        final var filename = file.getName();
        System.out.println(filename);

        final var size = file.length();
        final var path = file.getAbsolutePath();
        out.writeUTF(filename);
        out.flush();
        out.writeLong(size);
        out.flush();

        final var frame = new JFrame("Sending file: " + filename);
        frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        frame.setSize(500, 300);
        frame.setVisible(true);

        final var pane = frame.getContentPane();
        pane.setLayout(new BoxLayout(pane, BoxLayout.Y_AXIS));

        final var info = new JLabel("");
        info.setAlignmentX(Component.CENTER_ALIGNMENT);
        pane.add(info);

        final var progress = new JLabel("");
        progress.setAlignmentX(Component.CENTER_ALIGNMENT);
        pane.add(progress);

        final var in = new DataInputStream(new FileInputStream(path));
        var buffer = new byte[bufferSize];
        long bytesSend = 0;
        int percentage, send;

        while (bytesSend < size) {
          send = in.read(buffer);
          out.write(buffer, 0, send);
          out.flush();

          bytesSend = bytesSend + send;
          percentage = (int)(bytesSend * 100 / size);
          progress.setText("Progress: " + percentage + "%");
        }

        info.setText("File " + filename + " send!");
        in.close();
      }

      out.close();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}