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
    final var program = new ProgramPanel(threads);
  }
}

class ProgramPanel extends JPanel {
  private ReentrantLock searchHere = new ReentrantLock();
  final int numThreads;
  int currentThread = 0;
  int searched = 0;
  boolean searching = false;

  JLabel history;
  JTextField fileName;

  Worker[] workers;

  class Worker extends Thread {
    Worker prev, next;
    int ID;

    void doWork() {
      if (!searching)
        return;
      searchHere.lock();
      try {
        if (searched >= numThreads) {
          searching = false;
          searched = 0;
          System.out.println("Stopped at " + this.ID);
        } else {
          System.out.println("I will do work " + this.ID);
          searched++;

          final var path = "./" + this.ID + "/" + fileName.getText();
          if (new File(path).exists()) {
            System.out.println("Found it: " + this.ID);
            searching = false;
            searched = 0;
          }
          System.out.println("Work done " + this.ID + "\n");
        }
      } finally {
        searchHere.unlock();
        if (searching) {
          prev.interrupt();
          next.interrupt();
        }
      }
    }

    @Override
    public void run() {
      while (true) {
        try {
          Thread.sleep(100);
        } catch (Exception e) {
          doWork();
        }
      }
    }
  }

  public ProgramPanel(int numThreads) {
    this.numThreads = numThreads;
    this.workers = new Worker[numThreads];
    for (var i = 0; i < this.workers.length; ++i)
      this.workers[i] = new Worker();

    for (var i = 0; i < this.workers.length; ++i) {
      this.workers[i].ID = i;
      this.workers[i].prev = this.workers[id(numThreads, i, -1)];
      this.workers[i].next = this.workers[id(numThreads, i, +1)];
    }

    for (var w : workers)
      w.start();

    JFrame frame = new JFrame("Searching Files");
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    addComponentsToPane(frame.getContentPane());

    frame.pack();
    frame.setVisible(true);
  }

  public void addComponentsToPane(Container pane) {
    pane.setLayout(new BoxLayout(pane, BoxLayout.Y_AXIS));

    final var l1 = new JLabel("Hilos activos: " + this.numThreads);
    l1.setAlignmentX(Component.CENTER_ALIGNMENT);
    pane.add(l1);

    final var j1 = new JLabel("Hilo actual: " + this.currentThread);
    j1.setAlignmentX(Component.CENTER_ALIGNMENT);
    pane.add(j1);

    final var b1 = new JButton("Siguiente Hilo");
    b1.setAlignmentX(Component.CENTER_ALIGNMENT);
    pane.add(b1);

    b1.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        currentThread += (numThreads + 1);
        currentThread = currentThread % numThreads;
        j1.setText("Hilo actual: " + currentThread);
      }
    });

    final var b2 = new JButton("Anterior Hilo");
    b2.setAlignmentX(Component.CENTER_ALIGNMENT);
    pane.add(b2);

    b2.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        currentThread += (numThreads - 1);
        currentThread = currentThread % numThreads;
        j1.setText("Hilo actual: " + currentThread);
      }
    });

    final var l2 = new JLabel("Nombre del archivo a buscar");
    l2.setAlignmentX(Component.CENTER_ALIGNMENT);
    pane.add(l2);

    fileName = new JTextField("Juan.txt");
    fileName.setAlignmentX(Component.CENTER_ALIGNMENT);
    pane.add(fileName);

    final var bs = new JButton("Buscar archivo");
    bs.setAlignmentX(Component.CENTER_ALIGNMENT);
    pane.add(bs);

    bs.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        searching = true;
        workers[currentThread].interrupt();
      }
    });

    history = new JLabel("Historial");
    history.setAlignmentX(Component.CENTER_ALIGNMENT);
    pane.add(history);
  }

  private int id(int size, int current, int operation) {
    var idea = current + operation;
    if (idea < 0)
      idea = size - 1;
    return idea % size;
  }

}
