package GUI;

import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

public class Private {
  String aux = "";

  public Private(final String username, final String user) throws Exception {
    final var host = InetAddress.getByName("127.0.0.1");
    final var server_port = 9709;

    final var socket = new DatagramSocket();

    final var raw = String.format("<init> <%s>", username).getBytes();
    socket.send(new DatagramPacket(raw, raw.length, host, server_port));

    final var frame = new JFrame(String.format("Private Chat from %s -> %s", username, user));
    frame.getContentPane().setBackground(Color.WHITE);
    final var messageSection = new JEditorPane();
    final var newMessage = new JTextField();

    final var scroller = new JScrollPane(messageSection, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
        JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

    frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
    frame.setLocationRelativeTo(null);
    frame.setResizable(false);
    frame.setSize(300, 340);
    frame.setVisible(true);
    frame.setLayout(null);

    scroller.setBounds(20, 20, 260, 240);

    messageSection.setContentType("text/html");
    messageSection.setForeground(Color.RED);
    messageSection.setFont(new Font("CHelvetica", Font.BOLD, 14));
    messageSection.setEditable(false);

    newMessage.setBounds(20, 280, 260, 20);
    newMessage.setForeground(Color.RED);
    newMessage.setFont(new Font("Helvetica", Font.BOLD, 14));

    frame.add(scroller);
    frame.add(newMessage);
    frame.setVisible(true);

    newMessage.addActionListener(e -> {
      try {
        final var bytes =
            String.format("<msg> <%s> %s: %s", user, username, newMessage.getText()).getBytes();
        socket.send(new DatagramPacket(bytes, bytes.length, host, server_port));
      } catch (final Exception e1) {
        e1.printStackTrace();
      }
      newMessage.setText("");
    });

    new Thread(() -> {
      while (true) {
        try {
          final var p = new DatagramPacket(new byte[1024], 1024);
          socket.receive(p);
          final var message = new String(p.getData(), 0, p.getLength());
          aux = aux + message + "<br />";
          messageSection.setText(aux);
        } catch (final IOException e) {
          e.printStackTrace();
        }
      }
    }).start();
  }
}
