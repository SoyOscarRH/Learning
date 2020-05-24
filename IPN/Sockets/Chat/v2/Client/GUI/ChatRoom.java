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
  String username;
  String privateMsg;
  String messages = "";

  public ChatRoom(final String username) {
    this.username = username;

    final var onlineUsers = new JList<String>();
    final var messageSection = new JEditorPane();

    final var label = new JLabel("Online Users:");

    final var newMessageField = new JTextField();

    newMessageField.addActionListener(e -> {
      Main.send(("<msg> " + username + ": " + newMessageField.getText()).getBytes());
      newMessageField.setText("");
    });

    final var logo = new JLabel();

    final var verticalYes = JScrollPane.VERTICAL_SCROLLBAR_ALWAYS;
    final var horizontalNo = JScrollPane.HORIZONTAL_SCROLLBAR_NEVER;
    final var scroller1 = new JScrollPane(onlineUsers, verticalYes, horizontalNo);
    final var scroller = new JScrollPane(messageSection, verticalYes, horizontalNo);

    logo.setIcon(new ImageIcon(
        new ImageIcon("Logo.png").getImage().getScaledInstance(90, 70, Image.SCALE_SMOOTH)));

    onlineUsers.setBounds(300, 125, 70, 160);
    scroller1.setBounds(300, 125, 80, 170);
    scroller.setBounds(20, 20, 260, 280);
    label.setBounds(300, 80, 120, 50);
    logo.setBounds(300, 10, 90, 70);
    newMessageField.setBounds(15, 330, 270, 20);

    onlineUsers.setForeground(Color.LIGHT_GRAY);
    newMessageField.setForeground(Color.blue);
    messageSection.setForeground(Color.blue);

    onlineUsers.setFont(new Font("Times New Roman", Font.BOLD, 14));
    newMessageField.setFont(new Font("Times New Roman", Font.BOLD, 14));
    messageSection.setFont(new Font("Times New Roman", Font.BOLD, 15));
    label.setFont(new Font("Helvetica", Font.BOLD, 15));

    onlineUsers.addMouseListener(new MouseAdapter() {
      public void mouseClicked(final MouseEvent event) {
        if (event.getClickCount() != 2)
          return;

        try {
          final var list = (JList) event.getSource();
          final int index = list.locationToIndex(event.getPoint());

          final var msgFor = (String) list.getModel().getElementAt(index);
          privateMsg = "<private> " + list.getModel().getElementAt(index) + "from " + username;

          PrivateMessage(msgFor);
        } catch (final IOException e) {
          e.printStackTrace();
        }
      }
    });

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

    new Thread(() -> {
      while (true) {
        try {
          final var packet = new DatagramPacket(new byte[1024], 1024);
          Main.cl.receive(packet);
          final var type = new String(packet.getData(), 0, packet.getLength()).toLowerCase();

          if (type.equals("<init>")) {
            final var users = new DefaultListModel<String>();

            final DatagramPacket p = new DatagramPacket(new byte[1024], 1024);
            Main.cl.receive(p);

            final var bytes = new ByteArrayInputStream(p.getData());
            final var numUsers = new DataInputStream(bytes).readInt();

            for (var i = 0; i < numUsers; ++i) {
              final var packetName = new DatagramPacket(new byte[1024], 1024);
              Main.cl.receive(packetName);
              final var user = new String(packetName.getData(), 0, packetName.getLength());
              System.out.printf("-----%s\n", user);
              users.addElement(user);
            }

            onlineUsers.setModel(users);
            onlineUsers.updateUI();
            System.out.printf("-----%d\n", onlineUsers.getModel().getSize());
          }

          if (type.equals("<msg>")) {
            final var packetMessage = new DatagramPacket(new byte[1024], 1024);
            Main.cl.receive(packetMessage);
            final var message = new String(packetMessage.getData(), 0, packetMessage.getLength());

            System.out.printf(
                "message from: %s:%s\n", packetMessage.getAddress(), packetMessage.getPort());
            System.out.printf("\tmessage: %s\n", message);

            messages += message + "<br />";
            messageSection.setText(messages);
          }

          if (type.equals("<private>")) {
            final var packetFrom = new DatagramPacket(new byte[1024], 1024);
            Main.cl.receive(packetFrom);
            final var msgFrom = new String(packetFrom.getData(), 0, packetFrom.getLength());

            final var packetFor = new DatagramPacket(new byte[1024], 1024);
            Main.cl.receive(packetFor);
            final var msgFor = new String(packetFor.getData(), 0, packetFor.getLength());

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

  /* Method called from a nested method if the program detect a mouse event,
   * send a string to the socket with the label "<private>", opens a new
   * JDialog for private texting with another user. At the same time sends a
   * message to the UDP socket server with the label <init> to store the port,
   * and the username of the requester user.
   */

  void PrivateMessage(String msgFor) throws IOException {
    final String[] s1 = msgFor.split(" ");
    msgFor = s1[0];
    Main.send(privateMsg.getBytes());
    final var privateSession = new Private();
    final var b = ("<init> <" + username + ">").getBytes();
    Private.cl.send(
        new DatagramPacket(b, b.length, InetAddress.getByName(Private.host), Private.ports));
    privateSession.Components(username, msgFor);
  }
}
