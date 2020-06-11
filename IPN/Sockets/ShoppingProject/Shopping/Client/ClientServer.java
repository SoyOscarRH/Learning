package Shopping.Client;

import java.io.*;
import java.nio.*;

import java.net.InetSocketAddress;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;
import javax.swing.table.*;

import Shopping.Products;

class ClientServer {
  static SocketChannel channel;
  static AbstractTableModel t1, t2;

  static void startServer() {
    try {
      channel = SocketChannel.open(new InetSocketAddress("localhost", 9090));
      channel.configureBlocking(false);

      final var selector = Selector.open();
      channel.register(selector, channel.validOps(), null);

      System.out.println("Client started :D");
      while (true) {
        selector.select();
        final var selected = selector.selectedKeys();
        for (final var key : selected) {
          if (!key.isReadable()) continue;
          Products.updateFrom(channel);
          if (t1 != null) t1.fireTableDataChanged();
          if (t2 != null) t2.fireTableDataChanged();
        }
        selected.clear();
      }
    } catch (Exception e) {
      e.printStackTrace();
    }

  }
}