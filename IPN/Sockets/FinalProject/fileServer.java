import java.io.*;
import java.net.*;
import java.nio.file.Files;
import java.nio.file.Paths;

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
        final var size = (int) (new File(path)).length();
        final var numOfElements = size / parts;
        final var beforeMe = part * numOfElements;
        final var sending = (part + 1 == parts) ? size - beforeMe : numOfElements;
        System.out.printf("sending file: %s, part %d/%d: %d bytes\n", filename, part, parts, sending);
        out.writeLong(sending);
        out.flush();

        final byte[] raw = Files.readAllBytes(Paths.get(path));
        out.write(raw, beforeMe, sending);
        out.flush();

        out.close();
        in.close();
        socket.close();
      } catch (final Exception e) {
        e.printStackTrace();
      }
    }
  }

}
