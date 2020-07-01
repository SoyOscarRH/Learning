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
      System.out.printf("goinf to receive \t%d bytes", toReceive);

      out.close();


    } catch (final Exception e) {
      e.printStackTrace();
    }

    return new byte[0];
  }
}