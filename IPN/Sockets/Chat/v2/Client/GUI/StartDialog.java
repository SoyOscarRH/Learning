package GUI;

import ClientSocket.Client;
import java.io.IOException;
import java.net.DatagramPacket;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JTextField;

public class StartDialog {
  static public void show() {
    final var dialog = new JDialog();
    dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
    dialog.setLocationRelativeTo(null);
    dialog.setTitle("Login to chat");
    dialog.setSize(400, 150);
    dialog.setLayout(null);

    final var confirm = new JButton("Join chat");
    final var cancel = new JButton("Close");
    final var userName = new JTextField("Username in chat");

    userName.setBounds(20, 20, 360, 20);
    confirm.setBounds(80, 60, 95, 20);
    cancel.setBounds(220, 60, 95, 20);

    dialog.add(userName);
    dialog.add(confirm);
    dialog.add(cancel);

    confirm.addActionListener(e -> {
      try {
        final var username = userName.getText();
        final var raw = ("<init> " + username).getBytes();
        Client.cl.send(new DatagramPacket(raw, raw.length, Client.group, Client.ports));
        new ChatRoom(username).Components();
        dialog.dispose();
      } catch (final IOException expection) {
        expection.printStackTrace();
      }
    });

    cancel.addActionListener(e -> { dialog.dispose(); });

    dialog.setVisible(true);
  }
}