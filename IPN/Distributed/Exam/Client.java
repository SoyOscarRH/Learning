import java.net.Socket;
import java.io.DataOutputStream;
import java.io.DataInputStream;
import java.nio.ByteBuffer;

class Client {
  public static void main(String[] args) throws Exception {
    final var socket = new Socket("sisdis.sytes.net", 10_000);

    final var out = new DataOutputStream(socket.getOutputStream());
    final var in = new DataInputStream(socket.getInputStream());

    out.writeInt(2);
    out.writeDouble(200.0);

    final var x = in.readDouble();
    System.out.println(x);

    in.close();
    out.close();
    socket.close();
  }
}