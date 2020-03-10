import java.io.*;
import java.net.*;

public class Server {
  public static void main(String[] args) {
    try {
      final var serverSocket = new ServerSocket(7000);
      while (true) {
        final var socket = serverSocket.accept();
        System.out.println(socket.getInetAddress() + ":" + socket.getPort());

        final var in = new DataInputStream(socket.getInputStream());
        final var bufferSize = in.readInt();
        final var files = in.readInt();

        for (var i = 0; i < files; ++i) {
          final var filename = in.readUTF();
          System.out.println("Receiving file: " + filename);
          final var size = in.readLong();
          System.out.println("size " + size);

          final var out = new DataOutputStream(new FileOutputStream(filename));
          var buffer = new byte[1024];
          long bytesSend = 0;
          int percentage, send;

          while (bytesSend < size) {
            if (bytesSend + bufferSize > size)
              buffer = new byte[(int)(size - bytesSend)];
              
            send = in.read(buffer);
            out.write(buffer, 0, send);
            out.flush();

            bytesSend = bytesSend + send;
            percentage = (int)(bytesSend * 100 / size);
            System.out.println("Progress: " + percentage + "%");
          }

          System.out.println("File " + filename + " received!");
          System.out.println("bytesSend " + bytesSend);
          out.close();
        }

        in.close();
        socket.close();
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
