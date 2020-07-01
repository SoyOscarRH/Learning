import javax.swing.*;

class portDialog {
  static public void show() {
    final var dialog = new JDialog();
    dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
    dialog.setLocationRelativeTo(null);
    dialog.setTitle("Files Ring");
    dialog.setSize(400, 150);
    dialog.setLayout(null);

    final var text = new JTextField("Port to use:");
    final var confirm = new JButton("Join chat");
    final var cancel = new JButton("Close");

    text.setBounds(20, 20, 360, 20);
    confirm.setBounds(80, 60, 95, 20);
    cancel.setBounds(220, 60, 95, 20);

    dialog.add(text);
    dialog.add(confirm);
    dialog.add(cancel);

    confirm.addActionListener(e -> {
      new Window(Integer.parseInt(text.getText()));
      dialog.dispose();
    });

    cancel.addActionListener(e -> {
      dialog.dispose();
    });
    text.requestFocus();
    dialog.setVisible(true);
  }
}