package GUI;

import Main.Main;
import java.awt.Color;
import java.awt.Font;
import java.awt.Image;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.InetAddress;
import java.util.ArrayList;
import javax.swing.DefaultListModel;
import javax.swing.ImageIcon;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

public class ChatRoom {
  String messages = "";

  public ChatRoom(final String username) {
    final var onlineUsers = new JList<String>();
    final var messageSection = new JEditorPane();

    final var label = new JLabel("Online Users:");

    final var newMessageField = new JTextField();

    final var logo = new JLabel();

    final var verticalYes = JScrollPane.VERTICAL_SCROLLBAR_ALWAYS;
    final var horizontalNo = JScrollPane.HORIZONTAL_SCROLLBAR_NEVER;
    final var scroller1 = new JScrollPane(onlineUsers, verticalYes, horizontalNo);
    final var scroller = new JScrollPane(messageSection, verticalYes, horizontalNo);

    final var image = new ImageIcon("Logo.png").getImage();
    logo.setIcon(new ImageIcon(image.getScaledInstance(90, 70, Image.SCALE_SMOOTH)));

    onlineUsers.setBounds(300, 125, 70, 160);
    scroller1.setBounds(300, 125, 80, 170);
    scroller.setBounds(20, 20, 260, 280);
    label.setBounds(300, 80, 120, 50);
    logo.setBounds(300, 10, 90, 70);
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
    frame.setSize(400, 400);

    frame.add(scroller1);
    frame.add(scroller);
    frame.add(label);
    frame.add(logo);
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
          final var privateMsg = "<private> " + msgFor + "from " + username;

          final String[] s1 = msgFor.split(" ");
          msgFor = s1[0];
          Main.send(privateMsg);
          final var privateSession = new Private();
          final var b = ("<init> <" + username + ">").getBytes();
          Private.cl.send(
              new DatagramPacket(b, b.length, InetAddress.getByName(Private.host), Private.ports));

          privateSession.Components(username, msgFor);

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

            for (var i = 0; i < numUsers; ++i) {
              final var user = Main.receive();
              System.out.printf("-----%s\n", user);
              users.addElement(user);
            }

            onlineUsers.setModel(users);
            onlineUsers.ensureIndexIsVisible(onlineUsers.getModel().getSize());
            System.out.printf("-----%d\n", onlineUsers.getModel().getSize());
          }

          if (type.equals("<msg>")) {
            final var message = Main.receive();
            System.out.printf("\tmessage: %s\n", message);

            messages += message + "<br />";
            messageSection.setText(messages);
          }

          if (type.equals("<private>")) {
            final var msgFrom = Main.receive();
            final var msgFor = Main.receive();

            System.out.printf("private for: %s from %s", msgFor, msgFrom);

            if (msgFor.equals(username)) {
              final var address = InetAddress.getByName(Private.host);

              final var privateSession = new Private();
              final var raw = ("<init> <" + username + ">").getBytes();
              Private.cl.send(new DatagramPacket(raw, raw.length, address, Private.ports));
              privateSession.Components(username, msgFrom);
            }
          }
        } catch (final IOException e) {
          e.printStackTrace();
        }
      }
    }).start();
  }
}
