package Shopping.Client;

import java.io.*;
import java.nio.*;

import java.net.InetSocketAddress;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;
import java.io.IOException;

import Shopping.Products;
import Shopping.Product;

public class Client {
  public static void main(final String[] args) {
    try {
      new Thread(() -> ClientServer.startServer()).start();
      Products.print();

      Products.sendUpdateTo(ClientServer.channel);
      new ClientWindow();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}