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
      Thread.sleep(100);
      Products.print();
      Products.products.clear();
      Products.products.add(new Product("azucar", 3, 2, "azucar.jpg"));
      Products.products.add(new Product("frijol", 10, 5.5, "frijol.jpg"));
      Products.products.add(new Product("galletas", 3, 10.2, "galletas.jpeg"));
      Products.products.add(new Product("jugo", 5, 19, "jugo.jpeg"));
      Products.products.add(new Product("kiwi", 18, 22.5, "kiwi.jpeg"));
      Products.products.add(new Product("platano", 8, 34.6, "platano.jpeg"));
      Products.products.add(new Product("sabritas", 8, 7.5, "sabritas.jpeg"));
      Products.sendUpdateTo(ClientServer.channel);
      new ClientWindow();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}