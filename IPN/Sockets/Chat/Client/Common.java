import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.InetAddress;
import java.util.ArrayList;
import javax.swing.*;

class Common {
  String messages = "";

  Common(final String username) {
    final var onlineUsers = new JList<String>();
    final var messageSection = new JEditorPane();
    final var label = new JLabel("Online Users:");
    final var newMessageField = new JTextField();

    final var verticalYes = JScrollPane.VERTICAL_SCROLLBAR_ALWAYS;
    final var horizontalNo = JScrollPane.HORIZONTAL_SCROLLBAR_NEVER;
    final var scrollerOnlineUsers = new JScrollPane(onlineUsers, verticalYes, horizontalNo);
    final var scrollerMessages = new JScrollPane(messageSection, verticalYes, horizontalNo);

    final var frame = new JFrame("Chat: " + username);
    frame.getContentPane().setBackground(Color.white);
    frame.setLayout(null);
    frame.setSize(420, 400);
    scrollerMessages.setBounds(20, 20, 260, 280);
    label.setBounds(300, 0, 120, 50);
    onlineUsers.setBounds(300, 40, 70, 160);
    scrollerOnlineUsers.setBounds(300, 40, 80, 170);

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



    frame.add(scrollerOnlineUsers);
    frame.add(scrollerMessages);
    frame.add(label);
    frame.add(newMessageField);

    frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
    frame.setLocationRelativeTo(null);
    frame.setResizable(false);
    frame.setVisible(true);

    newMessageField.addActionListener(e -> {
      Client.send("<msg> " + username + ": " + newMessageField.getText());
      newMessageField.setText("");
    });

    onlineUsers.addMouseListener(new MouseAdapter() {
      public void mouseClicked(final MouseEvent event) {
        if (event.getClickCount() != 2) return;
        try {
          final var msgFor = onlineUsers.getSelectedValue().trim();
          if (msgFor.equals(username)) return;
          Client.send("<private> " + msgFor + " from " + username);
          new PrivateChat(username, msgFor);
          onlineUsers.clearSelection();

        } catch (final Exception e) {
          e.printStackTrace();
        }
      }
    });

    new Thread(() -> {
      while (true) {
        try {
          final var type = Client.receive().toLowerCase();

          if (type.equals("<init>")) {
            final var users = new DefaultListModel<String>();
            final int numUsers = Integer.parseInt(Client.receive());

            for (var i = 0; i < numUsers; ++i) users.addElement(Client.receive());

            onlineUsers.setModel(users);
            onlineUsers.ensureIndexIsVisible(onlineUsers.getModel().getSize());
          }

          if (type.equals("<msg>")) {
            messages += Client.receive() + "<br />";
            messageSection.setText(messages);
          }

          if (type.equals("<private>")) {
            final var msgFrom = Client.receive();
            final var msgFor = Client.receive();

            if (msgFor.equals(username)) new PrivateChat(username, msgFrom);
          }
        } catch (final Exception e) {
          e.printStackTrace();
        }
      }
    }).start();
  }
}
