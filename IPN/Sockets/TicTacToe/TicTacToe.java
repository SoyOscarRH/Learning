import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.concurrent.locks.ReentrantLock;

public class TicTacToe {
  static final char BLANK = ' ', O = 'O', X = 'X';

  public TicTacToe() {
    new Board(O);
    new Board(X);
  }

  private class Board extends JFrame implements ChangeListener, ActionListener {
    private BoardPanel boardPanel;

    public void stateChanged(ChangeEvent e) {
      boardPanel.repaint();
    }

    public void actionPerformed(ActionEvent e) {
      boardPanel.repaint();
    }

    public Board(char gamer) {
      super("Tic Tac Toe");

      boardPanel = new BoardPanel(this);

      add(boardPanel, BorderLayout.CENTER);
      setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
      setSize(400, 400);
      setVisible(true);
    }

  }

  private class BoardPanel extends JPanel implements MouseListener {
    private char current = O;
    // Board position (BLANK, O, or X)
    private char position[] = { BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK };
    private int rows[][] = { { 0, 2 }, { 3, 5 }, { 6, 8 }, { 0, 6 }, { 1, 7 }, { 2, 8 }, { 0, 8 }, { 2, 6 } };
    private ReentrantLock changingGamestate = new ReentrantLock();
    private boolean gameOver = false;
    private Board window;

    int x = -1, y = -1;

    // Endpoints of the 8 rows in position[] (across, down, diagonally)

    public BoardPanel(Board board) {
      window = board;
      addMouseListener(this);

      new Thread(() -> {
        while (!gameOver) {
          changingGamestate.lock();
          try {
            if (current == O) {
              changeState();
            }
          } finally {
            changingGamestate.unlock();
          }
        }
      }).start();

      new Thread(() -> {
        while (!gameOver) {
          changingGamestate.lock();
          try {
            if (current == X) {
              changeState();
            }
          } finally {
            changingGamestate.unlock();
          }
        }
      }).start();
    }

    public void paintComponent(Graphics g) {
      super.paintComponent(g);

      final var w = getWidth();
      final var h = getHeight();
      final var g2d = (Graphics2D) g;

      g2d.setPaint(Color.WHITE);
      g2d.fill(new Rectangle2D.Double(0, 0, w, h));
      g2d.setPaint(Color.BLACK);
      g2d.setStroke(new BasicStroke(5));
      g2d.draw(new Line2D.Double(0, h / 3, w, h / 3));
      g2d.draw(new Line2D.Double(0, h * 2 / 3, w, h * 2 / 3));
      g2d.draw(new Line2D.Double(w / 3, 0, w / 3, h));
      g2d.draw(new Line2D.Double(w * 2 / 3, 0, w * 2 / 3, h));

      // Draw the Os and Xs
      final var oColor = Color.BLUE;
      final var xColor = Color.RED;
      for (int i = 0; i < 9; ++i) {
        final var xpos = (i % 3 + 0.5) * w / 3.0;
        final var ypos = (i / 3 + 0.5) * h / 3.0;
        final var xr = w / 8.0;
        final var yr = h / 8.0;

        if (position[i] == O) {
          g2d.setPaint(oColor);
          g2d.draw(new Ellipse2D.Double(xpos - xr, ypos - yr, xr * 2, yr * 2));
        } else if (position[i] == X) {
          g2d.setPaint(xColor);
          g2d.draw(new Line2D.Double(xpos - xr, ypos - yr, xpos + xr, ypos + yr));
          g2d.draw(new Line2D.Double(xpos - xr, ypos + yr, xpos + xr, ypos - yr));
        }
      }
    }

    boolean isDraw() {
      for (var pos : position)
        if (pos == BLANK)
          return false;

      return true;
    }

    // Return true if player has won
    boolean won(char player) {
      for (var i = 0; i < 8; ++i)
        if (testRow(player, rows[i][0], rows[i][1]))
          return true;
      return false;
    }

    // Has player won in the row from position[a] to position[b]?
    boolean testRow(char player, int a, int b) {
      return position[a] == player && position[b] == player && position[(a + b) / 2] == player;
    }

    public void mousePressed(MouseEvent e) {
      final var xpos = e.getX() * 3 / getWidth();
      final var ypos = e.getY() * 3 / getHeight();

      this.x = xpos;
      this.y = ypos;
    }

    private void changeState() {
      final var pos = this.x + 3 * this.y;
      if (pos >= 0 && pos < 9 && position[pos] == BLANK) {
        position[pos] = current;
        current = current == O ? X : O;
        repaint();

        final var close = JOptionPane.CLOSED_OPTION;

        if (won(X)) {
          JOptionPane.showConfirmDialog(null, "You win X", "Result", close);
          gameOver = true;
          window.dispose();
        } else if (won(O)) {
          JOptionPane.showConfirmDialog(null, "You win O", "Result", close);
          gameOver = true;
          window.dispose();
        } else if (isDraw()) {
          JOptionPane.showConfirmDialog(null, "Draw", "Result", close);
          gameOver = true;
          window.dispose();
        }
      }
    }

    public void mouseClicked(MouseEvent e) {
    }

    public void mouseReleased(MouseEvent e) {
    }

    public void mouseEntered(MouseEvent e) {
    }

    public void mouseExited(MouseEvent e) {
    }
  }
}