import java.awt.*;
import java.io.*;
import java.net.*;
import javax.swing.*;

public class Client {
  static final String host = "10.100.76.1";
  static final int port = 7000;
  static JLabel progress;
  static JLabel info;

  public static void main(String[] args) {
    final var fileChooser = new JFileChooser();
    fileChooser.setMultiSelectionEnabled(true);
    if (fileChooser.showOpenDialog(null) != JFileChooser.APPROVE_OPTION)
      return;
    paint();

    try (final var socket = new Socket(host, port)) {
      final var out = new DataOutputStream(socket.getOutputStream());
      final var files = fileChooser.getSelectedFiles();

      out.writeInt(1024);
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

        final var in = new DataInputStream(new FileInputStream(path));
        var buffer = new byte[1024];
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

  static void paint() {
    final var frame = new JFrame("Sending files");
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.setSize(500, 300);
    frame.setVisible(true);

    final var pane = frame.getContentPane();
    pane.setLayout(new BoxLayout(pane, BoxLayout.Y_AXIS));

    info = new JLabel("");
    info.setAlignmentX(Component.CENTER_ALIGNMENT);
    pane.add(info);

    progress = new JLabel("");
    progress.setAlignmentX(Component.CENTER_ALIGNMENT);
    pane.add(progress);
  }
}