import java.io.*;
import java.net.*;
import java.nio.ByteBuffer;

class Chat {
  static String name;

  static class Worker extends Thread {
    public void run() {
      // En un ciclo infinito se recibirán los mensajes enviados al grupo
      // 230.0.0.0 a través del puerto 50000 y se desplegarán en la pantalla.

      try {
        final var port = 50_000;
        final var ip = InetAddress.getByName("230.0.0.0");
        final var group = new InetSocketAddress(ip, port);
        final var netInterface = NetworkInterface.getByName("en0");

        final var socket = new MulticastSocket(port);
        socket.joinGroup(group, netInterface);

        while (true) {
          recibe_mensaje(socket, 2048);
          final var message = new String(data, "utf-8");
          final var otherName = message.split(":")[0];
          final var otherLine = message.split(":")[1];
          if (!otherName.equals(name)) {
            System.out.println(String.format("[%s] escribe: [%s]", otherName, otherLine));
          }
        }
      } catch (final Exception e) {
        e.printStackTrace();
        System.exit(1);
      }
    }
  }

  static void envia_mensaje(final byte[] buffer, final String ip, final int puerto) throws IOException {
    DatagramSocket socket = new DatagramSocket();
    InetAddress grupo = InetAddress.getByName(ip);
    DatagramPacket paquete = new DatagramPacket(buffer, buffer.length, grupo, puerto);
    socket.send(paquete);
    socket.close();
  }

  static byte[] recibe_mensaje(final MulticastSocket socket, final int longitud_mensaje) throws IOException {
    byte[] buffer = new byte[longitud_mensaje];
    DatagramPacket paquete = new DatagramPacket(buffer, buffer.length);
    socket.receive(paquete);
    return paquete.getData();
  }

  public static void main(String[] args) throws Exception {
    Worker w = new Worker();
    w.start();
    String nombre = args[0];
    BufferedReader b = new BufferedReader(new InputStreamReader(System.in));
    // En un ciclo infinito se leerá los mensajes del teclado y se enviarán
    // al grupo 230.0.0.0 a través del puerto 50000.

    Chat.name = nombre;
    while (true) {
      final var line = b.readLine();
      final var message = nombre + ":" + line;
      if (line != "") {
        envia_mensaje(message.getBytes(), "230.0.0.0", 50_000);
      }
    }
  }
}