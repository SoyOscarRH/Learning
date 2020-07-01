import java.awt.*;
import java.io.*;
import java.net.*;
import javax.swing.*;

class fileClient {
  static byte[] send(final String filename, final int port, final int part, final int parts) {
    try (final var socket = new Socket("localhost", port + 200)) {
      System.out.printf("calling to %d to get %s\n", port, filename);
      final var out = new DataOutputStream(socket.getOutputStream());
      final var in = new DataInputStream(socket.getInputStream());

      out.writeUTF(filename);
      out.writeInt(part);
      out.writeInt(parts);
      out.flush();

      final var toReceive = in.readLong();
      System.out.printf("going to receive %d bytes\n", toReceive);
      final var raw = new byte[(int)toReceive];


      final var reality = in.read(raw);
      System.out.printf("readed %d bytes\n", reality);

      in.close();
      out.close();
      return raw;
    } catch (final Exception e) {
      e.printStackTrace();
    }

    return new byte[0];
  }
}