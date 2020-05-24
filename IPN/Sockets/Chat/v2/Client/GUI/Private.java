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
  String messages = "";

  public Private(final String me, final String other) throws Exception {
    final var host = InetAddress.getByName("127.0.0.1");
    final var server_port = 9709;
    final var socket = new DatagramSocket();
    final var raw = String.format("<init> <%s>", me).getBytes();
    socket.send(new DatagramPacket(raw, raw.length, host, server_port));

    final var frame = new JFrame(String.format("Private Chat from %s to %s", me, other));
    frame.getContentPane().setBackground(Color.WHITE);
    final var messageSection = new JEditorPane();
    final var newMessage = new JTextField();

    final var scroller = new JScrollPane(messageSection, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
        JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

    frame.setLayout(null);
    frame.setSize(300, 340);

    scroller.setBounds(20, 20, 260, 240);

    messageSection.setContentType("text/html");
    messageSection.setForeground(Color.BLUE);
    messageSection.setFont(new Font("Helvetica", Font.BOLD, 14));
    messageSection.setEditable(false);

    newMessage.setBounds(20, 280, 260, 20);
    newMessage.setForeground(Color.BLUE);
    newMessage.setFont(new Font("Helvetica", Font.BOLD, 14));

    frame.add(scroller);
    frame.add(newMessage);
    frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
    frame.setLocationRelativeTo(null);
    frame.setResizable(false);
    frame.setVisible(true);

    newMessage.addActionListener(e -> {
      try {
        final var message = String.format("<msg> <%s> %s: %s", other, me, newMessage.getText());
        final var bytes = message.getBytes();
        socket.send(new DatagramPacket(bytes, bytes.length, host, server_port));

      } catch (final Exception expection) {
        expection.printStackTrace();
      }
      newMessage.setText("");
    });

    new Thread(() -> {
      while (true) {
        try {
          final var packet = new DatagramPacket(new byte[1024], 1024);
          socket.receive(packet);
          final var message = new String(packet.getData(), 0, packet.getLength());
          messages += message + "<br />";
          messageSection.setText(messages);
        } catch (final Exception e) {
          e.printStackTrace();
        }
      }
    }).start();
  }
}
