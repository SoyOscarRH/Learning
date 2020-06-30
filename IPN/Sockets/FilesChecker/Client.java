import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;
import javax.swing.*;

class Client {
  public static void main(String[] args) {
    final var frame = new JFrame("Files Checker");
    frame.setLayout(new BoxLayout(frame.getContentPane(), BoxLayout.Y_AXIS));

    final var title = new JLabel("File to check if already exist");
    title.setFont(new Font("helvetica", Font.PLAIN, 24));
    title.setAlignmentX(Component.CENTER_ALIGNMENT);
    frame.add(title);

    final var toCheck = new JLabel("");
    toCheck.setFont(new Font("helvetica", Font.PLAIN, 14));
    toCheck.setAlignmentX(Component.CENTER_ALIGNMENT);
    frame.add(toCheck);

    final var root = "./" + args[0] + "/";
    final var chooser = new JFileChooser(root);

    final var check = new JButton("");
    check.addActionListener(e -> {
      if (chooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
        final var name = chooser.getSelectedFile().getName();

        try {
          final var registry = LocateRegistry.getRegistry(null);
          final var stub = (FileChecker) registry.lookup("FileChecker");
          final var md5 = MD5Checksum.getMD5Checksum(root + name);
          final var isSame = stub.isTheSameFile(name, md5);

          toCheck.setText(args[0] + "/" + name + (isSame ? " is" : " is not") + " on server");
        } catch (Exception ex) {
          ex.printStackTrace();
        }

      }
    });

    check.setFont(new Font("helvetica", Font.PLAIN, 24));
    check.setText("check file");
    check.setAlignmentX(Component.CENTER_ALIGNMENT);
    frame.add(check);

    frame.setSize(400, 450);
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.setLocationRelativeTo(null);
    frame.setVisible(true);

  }
}
