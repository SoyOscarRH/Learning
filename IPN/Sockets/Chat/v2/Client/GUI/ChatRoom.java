package GUI;

import Main.Main;
import java.awt.Color;
import java.awt.Font;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
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

public class ChatRoom extends JFrame {
  private static final long serialVersionUID = 1L;
  public static DefaultListModel<String> dlm;
  public static JList<String> onlineUsers;
  protected static JScrollPane scroller1;
  public static ArrayList<String> aList;
  protected static JScrollPane scroller;
  protected static String username;
  public static String privateMsg;
  protected static JEditorPane ep;
  protected static JTextField tf;
  protected static JLabel label;
  protected static JLabel logo;
  public static int counter;
  public static String aux;
  public static String un;
  public static Thread t;

  public ChatRoom(final String s) {
    super("TeamWork-Chat: " + s);
    this.getContentPane().setBackground(Color.white);
    dlm = new DefaultListModel<String>();
    onlineUsers = new JList<String>(dlm);
    aList = new ArrayList<String>();
    label = new JLabel("Online Users:");
    ep = new JEditorPane();
    t = new Thread(() -> {
      for (;;) {
        /* Receive the type of the datagram packet; <msg>, <init> or <private>,
         * depending the message there is an if sentence for any case.
         */
        String type = "";
        try {
          final DatagramPacket p = new DatagramPacket(new byte[1024], 1024);
          Main.cl.receive(p);
          type = new String(p.getData(), 0, p.getLength());
          System.out.printf("== %s ===\n", type);

        } catch (final IOException e) {
          e.printStackTrace();
        } // End try - catch.

        if (type.equalsIgnoreCase("<init>")) {
          dlm.clear();
          aList.clear();
          try {
            int numUsers = 0;
            final DatagramPacket p = new DatagramPacket(new byte[1500], 1500);
            Main.cl.receive(p);
            final ByteArrayInputStream bais = new ByteArrayInputStream(p.getData());
            final DataInputStream dis = new DataInputStream(bais);
            numUsers = (int) dis.readInt();

            System.out.printf("== %d ===\n", numUsers);
            String user = "";
            for (int i = 0; i < numUsers; i++) {
              final DatagramPacket p2 = new DatagramPacket(new byte[1024], 1024);
              Main.cl.receive(p2);
              user = new String(p2.getData(), 0, p2.getLength());
              System.out.printf("== %s ===\n", user);
              aList.add(user);
              dlm.addElement(aList.get(i));
            } // End for.

            onlineUsers.setModel(dlm);
            onlineUsers.updateUI();

          } catch (final IOException e) {
            e.printStackTrace();
          } // End try - catch.

        } // End if.

        if (type.equalsIgnoreCase("<msg>")) {
          String msg = "";
          try {
            final DatagramPacket p = new DatagramPacket(new byte[1500], 1500);
            Main.cl.receive(p);
            msg = new String(p.getData(), 0, p.getLength());
            System.out.println("\n\tMessage received from: " + p.getAddress() + " : " + p.getPort()
                + "\n\tMessage: " + msg);
          } catch (final IOException e) {
            e.printStackTrace();
          } // End try - catch.
          aux = aux + msg + "<BR>";
          ep.setText(aux);
        } // End if.

        if (type.equalsIgnoreCase("<private>")) {
          DatagramPacket p = new DatagramPacket(new byte[1500], 1500);
          String msgFrom = "";
          String msgFor = "";
          try {
            Main.cl.receive(p);
            msgFrom = new String(p.getData(), 0, p.getLength());
            p = new DatagramPacket(new byte[1500], 1500);
            Main.cl.receive(p);
            msgFor = new String(p.getData(), 0, p.getLength());
          } catch (final IOException e) {
            e.printStackTrace();
          } // End try - catch.
          System.out.println("\n\tPrivate Message for: " + msgFor + ". From: " + msgFrom + ".");
          if (msgFor.equalsIgnoreCase(username)) {
            try {
              final Private pmsg = new Private();
              final String s2 = "<init> <" + username + ">";
              final byte[] b = s2.getBytes();
              p = new DatagramPacket(
                  b, b.length, InetAddress.getByName(Private.host), Private.ports);
              Private.cl.send(p);
              pmsg.Components(username, msgFrom);

            } catch (final Exception e) {
              e.printStackTrace();
            } // End try - catch.
          } // End if.
        } // End if.

      } // End forever.
    });
    tf = new JTextField();
    logo = new JLabel();
    scroller1 = new JScrollPane(
        onlineUsers, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
    scroller = new JScrollPane(
        ep, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
    un = s + ":";
    username = s;
    counter = -1;
    t.start();
    aux = "";

    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    setLocationRelativeTo(null);
    setResizable(false);
    setSize(400, 400);
    setVisible(true);
    setLayout(null);

    final ImageIcon ic = new ImageIcon("Logo.png");
    final Image im = ic.getImage();
    final ImageIcon ic1 = new ImageIcon(im.getScaledInstance(90, 70, Image.SCALE_SMOOTH));
    logo.setIcon(ic1);

    onlineUsers.setBounds(300, 125, 70, 160);
    scroller1.setBounds(300, 125, 80, 170);
    scroller.setBounds(20, 20, 260, 280);
    label.setBounds(300, 80, 120, 50);
    logo.setBounds(300, 10, 90, 70);
    tf.setBounds(15, 330, 270, 20);

    onlineUsers.setForeground(Color.LIGHT_GRAY);
    tf.setForeground(Color.blue);
    ep.setForeground(Color.blue);

    onlineUsers.setFont(new Font("Times New Roman", Font.BOLD, 12));
    tf.setFont(new Font("Times New Roman", Font.BOLD, 12));
    ep.setFont(new Font("Times New Roman", Font.BOLD, 12));
    label.setFont(new Font("Helvetica", Font.BOLD, 14));

    /* Mouse event listener. */
    onlineUsers.addMouseListener(new MouseAdapter() {
      public void mouseClicked(final MouseEvent event) {
        final JList list = (JList) event.getSource();
        if (event.getClickCount() == 2) {
          final int index = list.locationToIndex(event.getPoint());
          final String msgFor = (String) list.getModel().getElementAt(index);
          privateMsg = "<private> " + list.getModel().getElementAt(index) + "from " + username;
          counter++;
          try {
            PrivateMessage(msgFor);
          } catch (final IOException e) {
            e.printStackTrace();
          } // End try - catch.
        } // End if.
      } // End method.
    });

    ep.setContentType("text/html");
    onlineUsers.setModel(dlm);
    ep.setEditable(false);

    add(scroller1);
    add(scroller);
    add(label);
    add(logo);
    add(tf);

    tf.addActionListener(e -> {
      String s2 = tf.getText();
      s2 = "<msg> " + un + " " + s2;
      final byte[] b = s2.getBytes();
      try {
        final DatagramPacket p = new DatagramPacket(b, b.length, Main.group, Main.ports);
        Main.cl.send(p);
      } catch (final Exception e1) {
        e1.printStackTrace();
      } // End try - catch.
      tf.setText("");
    });

  } // End Attributes.

  /* Method called from a nested method if the program detect a mouse event,
   * send a string to the socket with the label "<private>", opens a new
   * JDialog for private texting with another user. At the same time sends a
   * message to the UDP socket server with the label <init> to store the port,
   * and the username of the requester user.
   */

  public static void PrivateMessage(String msgFor) throws IOException {
    final String[] s1 = msgFor.split(" ");
    msgFor = s1[0];
    byte[] b = privateMsg.getBytes();
    DatagramPacket p = new DatagramPacket(b, b.length, Main.group, Main.ports);
    Main.cl.send(p);
    final Private pmsg = new Private();
    final String s = "<init> <" + username + ">";
    b = s.getBytes();
    p = new DatagramPacket(b, b.length, InetAddress.getByName(Private.host), Private.ports);
    Private.cl.send(p);
    pmsg.Components(username, msgFor);

  } // End PrivateMessage.

} // End class.
