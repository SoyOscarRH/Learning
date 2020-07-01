import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;
import java.util.TreeMap;
import java.util.Arrays;

import javax.swing.*;

class window {
  ArrayList<JButton> buttons = new ArrayList<JButton>();

  window(final int port) {
    final var multicast = new multiCastManager(port);
    final var files = new fileManager(port);

    final var frame = new JFrame("File checker [Port " + port + " ]");
    frame.setLayout(new BoxLayout(frame.getContentPane(), BoxLayout.Y_AXIS));

    final var titleLogs = new JLabel("Logs from " + port);
    titleLogs.setFont(new Font("helvetica", Font.PLAIN, 24));
    titleLogs.setAlignmentX(Component.CENTER_ALIGNMENT);
    frame.add(titleLogs);

    final var logs = new JTextArea("\n");
    logs.setEditable(false);
    logs.setFont(new Font("helvetica", Font.PLAIN, 14));
    logs.setAlignmentX(Component.CENTER_ALIGNMENT);
    frame.add(logs);

    new Thread(() -> {
      while (true) {
        logs.setText(multicast.getListOfConnections());
        try {
          Thread.sleep(1000);
        } catch (Exception e) {
          e.printStackTrace();
        }
      }
    }).start();

    final var findIt = new JButton("find file");
    findIt.setFont(new Font("helvetica", Font.PLAIN, 24));
    findIt.setAlignmentX(Component.CENTER_ALIGNMENT);
    frame.add(findIt);

    final var name = new JTextField("filename");
    name.setFont(new Font("helvetica", Font.PLAIN, 20));
    name.setAlignmentX(Component.CENTER_ALIGNMENT);
    frame.add(name);

    final var result = new JTextArea("\n");
    result.setEditable(false);
    result.setFont(new Font("helvetica", Font.PLAIN, 10));
    result.setAlignmentX(Component.CENTER_ALIGNMENT);
    frame.add(result);

    frame.setSize(400, 450);
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.setLocationRelativeTo(null);
    frame.setVisible(true);

    new Thread(() -> {
      rmiServer.startServer(port);
    }).start();

    new Thread(() -> {
      (new fileServer(port)).startServer();
    }).start();

    findIt.addActionListener(e -> {
      final var next = multicast.portNext + 100;
      final var filename = name.getText();
      final var results = rmiClient.findFile(filename, next, port, port);
      var total = "";

      for (final var button : buttons) {
        frame.remove(button);
        frame.repaint();
      }
      buttons.clear();

      final var where = new TreeMap<String, ArrayList<Integer>>();
      for (final var r : results) {
        if (where.get(r.md5) == null) {
          final var options = new ArrayList<Integer>();
          options.add(r.port);
          where.put(r.md5, options);
        } else
          where.get(r.md5).add(r.port);
      }

      for (final var r : where.entrySet()) {
        final var destinations = r.getValue().toArray();
        final var ids = Arrays.toString(destinations);
        total += String.format("%s in %s", r.getKey(), ids);

        final var getThis = new JButton("Get " + r.getKey());
        getThis.setFont(new Font("helvetica", Font.PLAIN, 14));
        getThis.setAlignmentX(Component.CENTER_ALIGNMENT);
        frame.add(getThis);
        buttons.add(getThis);
        getThis.addActionListener(e2 -> {
          var i = 0;
          for (final var destination : destinations) {
            fileClient.send(filename, (Integer) destination, i++, destinations.length);
          }
        });
      }

      if (total.equals("")) {
        total = "There we no results :c";
      }

      result.setText(total);
    });
  }
}
