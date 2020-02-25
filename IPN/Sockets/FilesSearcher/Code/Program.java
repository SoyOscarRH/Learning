import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.concurrent.locks.ReentrantLock;

import java.io.File;

public class Program {
  public static void main(String args[]) {
    final var messageID = JOptionPane.showInputDialog(null, "How threads to run?");
    final var threads = Math.min(Integer.valueOf(messageID), 10);
    final var program = new ContainerApp(threads);
  }
}

class ContainerApp {
  final int numThreads;

  public ContainerApp(int numThreads) {
    this.numThreads = numThreads;

    final var workers = new Worker[numThreads];
    for (var i = 0; i < numThreads; ++i)
      workers[i] = new Worker(i);

    final var mod = numThreads;
    for (var i = 0; i < numThreads; ++i) {
      workers[i].next = workers[(i + 1 + mod) % mod];
      workers[i].prev = workers[(i - 1 + mod) % mod];
    }

    for (final var w : workers)
      w.paint();
  }

  class Worker {
    final int workerID;
    Worker prev, next;

    JTextArea history;

    public Worker(int workerID) {
      this.workerID = workerID;
    }

    void append(String s) {
      final var data = history.getText();
      history.setText(data + "\n " + s);
    }

    String findFile(final String fileName, int sender, int jumps) {
      final var who = jumps == 0 ? "" : " Me mando a buscarlo el hilo: " + sender;
      append("Soy el hilo #" + workerID + " buscando: " + fileName + who);

      if (jumps == numThreads) {
        append("-> Ya regrese al mismo lugar, paro, no lo encontre");
        return null;
      }

      var path = "../" + workerID + "/" + fileName;

      if (new File(path).exists()) {
        append("-> Encontrado bajo mi guardia");
        if (jumps != 0)
          append("-> Regresando informacion al hilo " + sender);
        return path;
      } else {
        append("-> No lo encontre, buscando en el siguiente hilo: " + next.workerID);
        path = next.findFile(fileName, workerID, jumps + 1);
      }

      if (path != null)
        append("-> Lo encontre, gracias al hilo " + next.workerID + ", la ruta es: " + path);

      if (jumps != 0)
        append("-> Regresando informacion a " + sender);

      return path;
    }

    void paint() {
      JFrame frame = new JFrame("Searching Files at " + this.workerID);
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
      final var pane = frame.getContentPane();

      pane.setLayout(new BoxLayout(pane, BoxLayout.Y_AXIS));

      final var j1 = new JLabel("Hilo actual: " + workerID);
      j1.setAlignmentX(Component.CENTER_ALIGNMENT);
      pane.add(j1);

      final var j2 = new JLabel("Siguiente Hilo: " + next.workerID);
      j2.setAlignmentX(Component.CENTER_ALIGNMENT);
      pane.add(j2);

      final var j3 = new JLabel("Anterior Hilo: " + prev.workerID);
      j3.setAlignmentX(Component.CENTER_ALIGNMENT);
      pane.add(j3);

      final var l1 = new JLabel("Nombre del archivo a buscar: ");
      l1.setAlignmentX(Component.CENTER_ALIGNMENT);
      pane.add(l1);

      final var t1 = new JTextField("Juan.txt       ");
      t1.setMaximumSize(t1.getPreferredSize());

      t1.setAlignmentX(Component.CENTER_ALIGNMENT);
      pane.add(t1);

      final var bs = new JButton("Buscar archivo");
      bs.setAlignmentX(Component.CENTER_ALIGNMENT);
      pane.add(bs);

      bs.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
          findFile(t1.getText(), workerID, 0);
        }
      });

      final var bc = new JButton("Limpiar log");
      bc.setAlignmentX(Component.CENTER_ALIGNMENT);
      pane.add(bc);
      bc.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
          history.setText("");
        }
      });

      history = new JTextArea("Historial: ");
      history.setEditable(false);
      history.setAlignmentX(Component.CENTER_ALIGNMENT);
      pane.add(history);

      frame.pack();
      frame.setSize(400, 400);
      frame.setVisible(true);
    }

  }

}
