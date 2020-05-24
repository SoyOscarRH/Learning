package GUI;

import Main.Main;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.InetAddress;
import java.util.ArrayList;
import javax.swing.*;

public class ChatRoom {
  String messages = "";

  public ChatRoom(final String username) {
    final var onlineUsers = new JList<String>();
    final var messageSection = new JEditorPane();
    final var label = new JLabel("Online Users:");
    final var newMessageField = new JTextField();

    final var verticalYes = JScrollPane.VERTICAL_SCROLLBAR_ALWAYS;
    final var horizontalNo = JScrollPane.HORIZONTAL_SCROLLBAR_NEVER;
    final var scroller1 = new JScrollPane(onlineUsers, verticalYes, horizontalNo);
    final var scroller = new JScrollPane(messageSection, verticalYes, horizontalNo);

    onlineUsers.setBounds(300, 125, 70, 160);
    scroller1.setBounds(300, 125, 80, 170);
    scroller.setBounds(20, 20, 260, 280);
    label.setBounds(300, 80, 120, 50);
    newMessageField.setBounds(15, 330, 270, 20);

    onlineUsers.setForeground(Color.LIGHT_GRAY);
    newMessageField.setForeground(Color.blue);
    messageSection.setForeground(Color.blue);

    final var font = new Font("Helvetica", Font.BOLD, 15);
    onlineUsers.setFont(font);
    newMessageField.setFont(font);
    messageSection.setFont(font);
    label.setFont(font);

    messageSection.setContentType("text/html");
    messageSection.setEditable(false);

    final var frame = new JFrame("Char: " + username);
    frame.getContentPane().setBackground(Color.white);
    frame.setLayout(null);
    frame.setSize(450, 450);

    frame.add(scroller1);
    frame.add(scroller);
    frame.add(label);
    frame.add(newMessageField);

    frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
    frame.setLocationRelativeTo(null);
    frame.setResizable(false);
    frame.setVisible(true);

    newMessageField.addActionListener(e -> {
      Main.send("<msg> " + username + ": " + newMessageField.getText());
      newMessageField.setText("");
    });

    onlineUsers.addMouseListener(new MouseAdapter() {
      public void mouseClicked(final MouseEvent event) {
        if (event.getClickCount() != 2)
          return;

        try {
          final var list = (JList) event.getSource();
          final int index = list.locationToIndex(event.getPoint());

          var msgFor = (String) list.getModel().getElementAt(index);
          final String[] s1 = msgFor.split(" ");
          msgFor = s1[0];
          System.out.printf("|%s| -> |%s|", username, msgFor);

          if (msgFor.equals(username))
            return;
          Main.send("<private> " + msgFor + " from " + username);

          new Private(username, msgFor);

          final var b = String.format("<init> <%s>", username).getBytes();
          final var address = InetAddress.getByName(Private.host);
          Private.cl.send(new DatagramPacket(b, b.length, address, Private.ports));

          onlineUsers.clearSelection();

        } catch (final IOException e) {
          e.printStackTrace();
        }
      }
    });

    new Thread(() -> {
      while (true) {
        try {
          final var type = Main.receive().toLowerCase();

          if (type.equals("<init>")) {
            final var users = new DefaultListModel<String>();
            final int numUsers = Integer.parseInt(Main.receive());

            for (var i = 0; i < numUsers; ++i) users.addElement(Main.receive());

            onlineUsers.setModel(users);
            onlineUsers.ensureIndexIsVisible(onlineUsers.getModel().getSize());
          }

          if (type.equals("<msg>")) {
            messages += Main.receive() + "<br />";
            messageSection.setText(messages);
          }

          if (type.equals("<private>")) {
            final var msgFrom = Main.receive();
            final var msgFor = Main.receive();

            if (!msgFor.equals(username))
              return;

            new Private(username, msgFrom);
            final var raw = ("<init> <" + username + ">").getBytes();
            final var address = InetAddress.getByName(Private.host);
            Private.cl.send(new DatagramPacket(raw, raw.length, address, Private.ports));
          }
        } catch (final IOException e) {
          e.printStackTrace();
        }
      }
    }).start();
  }
}
