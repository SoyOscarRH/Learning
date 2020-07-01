import java.io.*;

class fileManager {
  final int port;

  fileManager(final int port) {
    this.port = port;
    new File("./" + port).mkdirs();
  }
}