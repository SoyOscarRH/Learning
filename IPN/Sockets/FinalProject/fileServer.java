import java.io.*;
import java.net.*;

public class fileServer {
  ServerSocket serverSocket;
  final int port;

  fileServer(final int port) {
    this.port = port;
    try {
      this.serverSocket = new ServerSocket(port + 200);
    } catch (final Exception e) {
      e.printStackTrace();
    }
  }

  void startServer() {
    System.out.printf("Socket TCP server is online.\n");
    while (true) {
      try {
        final var socket = serverSocket.accept();
        System.out.printf("connected from %s:%d\n", socket.getInetAddress(), socket.getPort());

        final var in = new DataInputStream(socket.getInputStream());
        final var out = new DataOutputStream(socket.getOutputStream());
        final var filename = in.readUTF();
        final var part = in.readInt();
        final var parts = in.readInt();

        final var path = String.format("./%d/%s", port, filename);
        final var toSend = (new File(path)).length();
        out.writeLong(toSend);
        out.flush();

        System.out.printf("sending file: %s, part %d/%d: \t%d bytes", filename, part, parts, toSend);
        in.close();
        socket.close();

      } catch (final Exception e) {
        e.printStackTrace();
      }
    }
  }

}
