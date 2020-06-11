package Shopping.Server;

import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;

import javax.swing.*;
import javax.swing.table.*;

import Shopping.Products;
import Shopping.Product;

class UpdaterWindow {

  public static void main(final String[] args) throws Exception {
    new UpdaterWindow();
  }


  UpdaterWindow() throws Exception {
    Products.load();
    final var frame = new JFrame("Shopping");
    frame.setLayout(new BoxLayout(frame.getContentPane(), BoxLayout.Y_AXIS));

    final var title1 = new JLabel("Productos disponibles");
    title1.setFont(new Font("helvetica", Font.PLAIN, 22));
    title1.setAlignmentX(Component.CENTER_ALIGNMENT);
    frame.add(title1);

    @SuppressWarnings("serial")
    final var model = new AbstractTableModel() {
      public int getColumnCount() {
        return 5;
      }

      public String getColumnName(final int col) {
        if (col == 0) return "Cantidad";
        if (col == 1) return "Nombre";
        if (col == 2) return "Precio";
        if (col == 3) return "Foto";
        if (col == 4) return "Eliminar";

        return "";
      }

      public int getRowCount() {
        return Products.products.size();
      }

      public boolean isCellEditable(final int row, final int col) {
        return true; 
      }

      public void setValueAt(final Object value, final int row, final int col) {
        if (col == 4) {
          Products.products.remove(row);
          Products.print();
          return;
        }
        final var product = Products.products.get(row);


        try {
          if (col == 0) product.quantity = Integer.parseInt(value.toString());
          if (col == 1) product.name = value.toString();
          if (col == 2) product.price = Double.parseDouble(value.toString());
          if (col == 3) product.imageUrl = value.toString();
        } catch (Exception e) {
          e.printStackTrace();
        }

        Products.print();
        try {
          Products.save();
        } catch (Exception ex) {
          ex.printStackTrace();
        }

        this.fireTableDataChanged();
      }

      public Object getValueAt(final int row, final int col) {
        if (col == 4) return "Eliminar";
        final var product = Products.products.get(row);

        if (col == 0) return product.quantity;
        if (col == 1) return product.name;
        if (col == 2) return product.price;
        if (col == 3) return product.imageUrl;
        return "";

      }
    };

    final var table = new JTable(model);
    table.setRowHeight(40);
    table.setFont(new Font("helvetica", Font.PLAIN, 18));

    frame.add(new JScrollPane(table));

    final var title2 = new JLabel("Producto nuevo");
    title2.setFont(new Font("helvetica", Font.PLAIN, 20));
    title2.setAlignmentX(Component.CENTER_ALIGNMENT);
    frame.add(title2);

    final var name = new JTextField("Nombre del producto");
    frame.add(name);

    final var price = new JTextField("Precio");
    frame.add(price);

    final var quantity = new JTextField("Cantidad actual");
    frame.add(quantity);

    final var imageUrl = new JTextField("path a la imagen");
    frame.add(imageUrl);

    final var add = new JButton("Añadir producto");
    frame.add(add);

    add.addActionListener(e -> {
      final var realName = name.getText();
      final var realImageUrl = imageUrl.getText();
      final var realPrice = Double.parseDouble(price.getText());
      final var realQuantity = Integer.parseInt(quantity.getText());

      Products.products.add(new Product(realName, realQuantity, realPrice, realImageUrl));
      model.fireTableDataChanged();
      try {
        Products.save();
      } catch (Exception ex) {
        ex.printStackTrace();
      }
    });

    frame.setSize(600, 650);
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.setLocationRelativeTo(null);
    frame.setVisible(true);
  }
}
