import java.net.*;
import java.util.*;
import java.util.concurrent.locks.ReentrantLock;

public class multiCastManager {
  final private int port;
  final private TreeMap<Integer, Integer> timeSinceAliveMessageFrom;
  private MulticastSocket socket;
  private InetAddress group;
  static int portNext;
  static int portPrevious;
  ReentrantLock lock;

  multiCastManager(final int port) {
    this.port = port;
    this.timeSinceAliveMessageFrom = new TreeMap<Integer, Integer>();
    this.lock = new ReentrantLock();
    try {
      this.socket = new MulticastSocket(8000);
      this.group = InetAddress.getByName("228.1.1.1");
      socket.joinGroup(group);
      socket.setTimeToLive(16);
    } catch (Exception e) {
      e.printStackTrace();
    }

    startMulticastServer();
  }

  void sendingHellosEach(final int seconds) {
    final var message = String.valueOf(port);
    while (true) {
      try {
        socket.send(new DatagramPacket(message.getBytes(), message.length(), group, 8000));
        Thread.sleep(1000 * seconds);
      } catch (Exception e) {
        e.printStackTrace();
      }
    }
  }

  void updatingConnectionTable() {
    while (true) {
      lock.lock();
      var ports = new ArrayList<Integer>();
      final var old = new TreeMap<>(timeSinceAliveMessageFrom);
      for (final var entry : old.entrySet()) {
        if (entry.getValue() > 15)
          timeSinceAliveMessageFrom.remove(entry.getKey());
        else {
          ports.add(entry.getKey());
          timeSinceAliveMessageFrom.put(entry.getKey(), entry.getValue() + 1);
        }
      }
      lock.unlock();

      ports.add(this.port);
      Collections.sort(ports);
      final var index = ports.indexOf(this.port);
      portNext = ports.get((index + 1) % ports.size());
      portPrevious = ports.get((ports.size() + index - 1) % ports.size());
      try {
        Thread.sleep(1000);
      } catch (Exception e) {
        e.printStackTrace();
      }
    }
  }

  public String getListOfConnections() {
    lock.lock();
    var listOfConnections = "";
    for (final var entry : timeSinceAliveMessageFrom.entrySet()) {
      listOfConnections += String.format("%s:%s  ", socket.getLocalAddress(), entry.getKey());
      listOfConnections += String.format("message %d sec. ago\n", entry.getValue());
    }

    listOfConnections += String.format("\n Next: %d \n", this.portNext);
    listOfConnections += String.format("\n Previous: %d \n", this.portPrevious);
    lock.unlock();

    return listOfConnections;
  }

  void waintingNewConnectionMessages() {
    while (true) {
      try {
        final var packet = new DatagramPacket(new byte[512], 512);
        socket.receive(packet);
        lock.lock();
        final var message = new String(packet.getData(), 0, packet.getLength());
        final var receivedPort = Integer.parseInt(message.trim());
        if (receivedPort != this.port)
          timeSinceAliveMessageFrom.put(receivedPort, 0);
      } catch (Exception e) {
        e.printStackTrace();
      }
      finally {
        lock.unlock();
      }
    }
  }

  void startMulticastServer() {
    new Thread(() -> {
      sendingHellosEach(5);
    }).start();

    new Thread(() -> {
      updatingConnectionTable();
    }).start();

    new Thread(() -> {
      waintingNewConnectionMessages();
    }).start();

    System.out.println("Multicast server is online. waiting for messages");
  }
}