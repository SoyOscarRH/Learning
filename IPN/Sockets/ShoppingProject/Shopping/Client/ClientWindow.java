package Shopping.Client;

import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;

import javax.swing.*;
import javax.swing.table.*;

import Shopping.Products;
import Shopping.Product;

public class ClientWindow {
  final ArrayList<Product> buying;
  static AbstractTableModel t1, t2;
  final JButton totalLabel;

  static String getFullPath(final String data) {
    final var projects = "/Users/soyoscarrh/Documents/Projects/Learning/";
    final var project = "IPN/Sockets/ShoppingProject/Shopping/Client/Images/";
    return projects + project + data;
  }

  JScrollPane createTable(final boolean isShowing) {
    @SuppressWarnings("serial")
    final var model = new AbstractTableModel() {
      public Class<?> getColumnClass(final int column) {
        if (column == 3) return ImageIcon.class;
        return String.class;
      }

      public int getColumnCount() {
        return 4;
      }

      public String getColumnName(final int col) {
        if (col == 0) return "Cantidad";
        if (col == 1) return "Nombre";
        if (col == 2) return "Precio";
        if (col == 3) return "Foto";

        return "";
      }

      public int getRowCount() {
        return isShowing ? Products.products.size() : buying.size();
      }

      public Object getValueAt(final int row, final int col) {
        final var product = isShowing ? Products.products.get(row) : buying.get(row);

        if (col == 0) return product.quantity;
        if (col == 1) return product.name;
        if (col == 2) return product.price;
        if (col == 3) return new ImageIcon(getFullPath(product.imageUrl));
        return "";
      }
    };

    final var table = new JTable(model);
    table.setRowHeight(80);
    table.setFont(new Font("helvetica", Font.PLAIN, 20));

    table.addMouseListener(new MouseAdapter() {
      public void mouseClicked(final MouseEvent e) {
        try {
          if (e.getClickCount() != 2) return;
          final var elementIndex = table.getSelectedRow();

          final var productsOrigin = isShowing ? Products.products : buying;
          final var productsDestination = !isShowing ? Products.products : buying;

          final var origin = productsOrigin.get(elementIndex);
          origin.quantity -= 1;
          if (origin.quantity == 0) productsOrigin.remove(elementIndex);

          final var destination = productsDestination.stream()
                                  .filter(product -> origin.name.equals(product.name))
                                  .findAny()
                                  .orElse(null);

          if (destination ==  null)  productsDestination.add(new Product(origin.name, 1, origin.price, origin.imageUrl));
          else destination.quantity += 1;

          t1.fireTableDataChanged();
          t2.fireTableDataChanged();

          final var total = buying.stream()
                            .map(product -> product.price * product.quantity)
                            .mapToDouble(x -> x)
                            .sum();

          totalLabel.setText(String.format("Comprar por %.2f", total));

          Products.sendUpdateTo(ClientServer.channel);
        } catch (Exception ex) {
          ex.printStackTrace();
        }
      }
    });

    if (isShowing) t1 = model;
    else t2 = model;

    if (isShowing) ClientServer.t1 = model;
    else ClientServer.t2 = model;

    return new JScrollPane(table);
  }

  public ClientWindow() throws Exception {
    buying = new ArrayList<Product>();

    final var frame = new JFrame("Shopping");
    frame.setLayout(new BoxLayout(frame.getContentPane(), BoxLayout.Y_AXIS));

    final var title1 = new JLabel("Productos disponibles");
    title1.setFont(new Font("helvetica", Font.PLAIN, 26));
    title1.setAlignmentX(Component.CENTER_ALIGNMENT);
    frame.add(title1);
    frame.add(createTable(true));

    final var title2 = new JLabel("Tu carrito");
    title2.setFont(new Font("helvetica", Font.PLAIN, 26));
    title2.setAlignmentX(Component.CENTER_ALIGNMENT);
    frame.add(title2);
    frame.add(createTable(false));

    totalLabel = new JButton("");
    totalLabel.addActionListener(e -> {
      t2.fireTableDataChanged();
      buying.clear();
    });
    totalLabel.setFont(new Font("helvetica", Font.PLAIN, 24));
    totalLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
    frame.add(totalLabel);

    frame.setSize(600, 750);
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.setLocationRelativeTo(null);
    frame.setVisible(true);
  }
}
