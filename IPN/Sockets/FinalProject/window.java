import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;
import javax.swing.*;

class window {
  window(final int port) {
    final var multicast = new multiCastManager(port);

    final var frame = new JFrame("File checker [Port " + port + " ]");
    frame.setLayout(new BoxLayout(frame.getContentPane(), BoxLayout.Y_AXIS));

    final var titleLogs = new JLabel("Logs\n");
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

    final var findIt = new JButton("");
    findIt.setFont(new Font("helvetica", Font.PLAIN, 24));
    findIt.setText("find file");
    findIt.setAlignmentX(Component.CENTER_ALIGNMENT);
    frame.add(findIt);

    final var name = new JTextField("");
    name.setFont(new Font("helvetica", Font.PLAIN, 10));
    name.setText("find file");
    name.setAlignmentX(Component.CENTER_ALIGNMENT);
    frame.add(name);

    new Thread(() -> {
      rmiServer.startingServer(port);
    }).start();

    findIt.addActionListener(e -> {
      rmiClient.callServer(multicast.portNext, port);
    });

    frame.setSize(400, 450);
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.setLocationRelativeTo(null);
    frame.setVisible(true);
  }
}
