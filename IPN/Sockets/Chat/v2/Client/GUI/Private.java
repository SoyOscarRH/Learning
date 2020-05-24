package GUI;

import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.SocketException;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

public class Private {
  public static String host = "127.0.0.1";
  public static String aux;
  public static int ports = 9709;
  public static DatagramSocket cl;


  public Private(final String username, final String user) throws SocketException {
    final var frame = new JFrame("Private Chat");
    cl = new DatagramSocket();

    frame.getContentPane().setBackground(Color.WHITE);
    final var ep = new JEditorPane();
    final var tf = new JTextField();

    aux = "";
    final var scroller = new JScrollPane(
        ep, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

    frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
    frame.setLocationRelativeTo(null);
    frame.setResizable(false);
    frame.setSize(300, 340);
    frame.setVisible(true);
    frame.setLayout(null);

    scroller.setBounds(20, 20, 260, 240);
    tf.setBounds(20, 280, 260, 20);

    ep.setContentType("text/html");
    tf.setForeground(Color.RED);
    ep.setForeground(Color.RED);

    tf.setFont(new Font("Helvetica", Font.BOLD, 14));
    ep.setFont(new Font("CHelvetica", Font.BOLD, 14));
    ep.setEditable(false);

    frame.add(scroller);
    frame.add(tf);

    tf.addActionListener(e -> {
      try {
        final var message = tf.getText();
        final var raw = ("<msg> <" + user + "> " + username + ": " + message).getBytes();
        cl.send(new DatagramPacket(raw, raw.length, InetAddress.getByName(host), ports));
      } catch (final Exception e1) {
        e1.printStackTrace();
      }
      tf.setText("");
    });

    new Thread(() -> {
      while (true) {
        try {
          final var p = new DatagramPacket(new byte[1500], 1500);
          cl.receive(p);
          final var msg = new String(p.getData(), 0, p.getLength());
          aux = aux + msg + "<BR>";
          ep.setText(aux);
        } catch (final IOException e) {
          e.printStackTrace();
        }
      }
    }).start();
  }
}
