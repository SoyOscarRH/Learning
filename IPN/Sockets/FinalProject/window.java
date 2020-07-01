import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;
import java.util.TreeMap;
import java.util.Arrays;
import java.util.concurrent.locks.ReentrantLock;
import java.io.FileOutputStream;

import javax.swing.*;

class window {
  ArrayList<JButton> buttons = new ArrayList<JButton>();
  int missing;

  window(final int port) {
    System.out.println("========= " + port + " ============");
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
        } catch (Exception e3) {
          e3.printStackTrace();
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
        total += String.format("%s in %s\n", r.getKey(), ids);

        final var getThis = new JButton("Get " + r.getKey());
        getThis.setFont(new Font("helvetica", Font.PLAIN, 14));
        getThis.setAlignmentX(Component.CENTER_ALIGNMENT);
        frame.add(getThis);
        buttons.add(getThis);
        getThis.addActionListener(e2 -> {
          var i = 0;
          final var numDestionations = destinations.length;
          missing = numDestionations;
          final var lock = new ReentrantLock();
          final var raws = new byte[numDestionations][];
          for (final var destination : destinations) {
            final var part = i++;
            new Thread(() -> {
              final var to = (Integer) destination;
              raws[part] = fileClient.send(filename, to, part, numDestionations);
              lock.lock();
              missing--;
              lock.unlock();
              if (missing == 0) {
                try (final var stream = new FileOutputStream("./" + port + "/" + filename)) {

                  var size = 0;
                  for (final var raw : raws)
                    size += raw.length;

                  final var soClose = new byte[size];
                  var j = 0;
                  for (final var raw : raws) {
                    for (var k = 0; k < raw.length; ++k) {
                      soClose[j++] = raw[k];
                    }
                  }

                  stream.write(soClose);
                } catch (Exception e4) {
                  e4.printStackTrace();
                }

              }
            }).start();
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
