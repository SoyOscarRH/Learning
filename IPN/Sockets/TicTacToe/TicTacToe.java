import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.concurrent.locks.ReentrantLock;

public class TicTacToe {
  // Endpoints of the 8 rows in position[] (across, down, diagonally)
  final private int rows[][] = { { 0, 2 }, { 3, 5 }, { 6, 8 }, { 0, 6 }, { 1, 7 }, { 2, 8 }, { 0, 8 }, { 2, 6 } };

  private char position[] = { BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK };
  private ReentrantLock changingGamestate = new ReentrantLock();
  private final int gameID;
  boolean gameOver = false;

  static final public char BLANK = ' ', O = 'O', X = 'X';
  private char current = O;
  final private Board board1, board2;

  public TicTacToe(int i) {
    board1 = new Board(O);
    board2 = new Board(X);
    this.gameID = i;

    updateBoth();
  }

  private void updateBoth() {
    board1.repaint(100);
    board2.repaint(100);
    board2.changeText();
    board1.changeText();

    if (won(X) || won(O) || isDraw()) {
      gameOver = true;
    }

    if (won(X)) {
      new Thread(() -> {
        board1.showBeforeClose("X won");
      }).start();
      new Thread(() -> {
        board2.showBeforeClose("X won");
      }).start();

    } else if (won(O)) {
      new Thread(() -> {
        board1.showBeforeClose("O won");
      }).start();
      new Thread(() -> {
        board2.showBeforeClose("O won");
      }).start();

    } else if (isDraw()) {
      new Thread(() -> {
        board1.showBeforeClose("Draw");
      }).start();
      new Thread(() -> {
        board2.showBeforeClose("Draw");
      }).start();

    }
  }

  boolean isDraw() {
    for (var pos : position)
      if (pos == BLANK)
        return false;

    return true;
  }

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

  private class Board extends JFrame {
    private BoardPanel boardPanel;
    private char player;

    public Board(char player) {
      this.player = player;
      this.changeText();
      boardPanel = new BoardPanel();

      add(boardPanel, BorderLayout.CENTER);
      setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
      setSize(400, 400);
      setVisible(true);
    }

    public void changeText() {
      var title = "Tic Tac Toe [" + gameID + "] [Player " + player + "] ";
      title += player == current ? "[Your turn]" : " [Not your turn]";
      this.setTitle(title);
    }

    public void showBeforeClose(String message) {
      var optionPane = new JOptionPane(message);
      var dialog = optionPane.createDialog(null, "Result");
      dialog.setModal(false);
      dialog.show();

      final var wea = this;

      dialog.addComponentListener(new ComponentListener() {
        public void componentHidden(ComponentEvent e) {
          wea.dispose();
        }

        public void componentMoved(ComponentEvent e) {
        }

        public void componentResized(ComponentEvent e) {
        }

        public void componentShown(ComponentEvent e) {
        }

      });
    }

    private class BoardPanel extends JPanel implements MouseListener {

      public BoardPanel() {
        addMouseListener(this);
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

      public void mousePressed(MouseEvent e) {
        changingGamestate.lock();
        try {
          if (current != player || gameOver)
            return;

          final var xpos = e.getX() * 3 / getWidth();
          final var ypos = e.getY() * 3 / getHeight();

          final var pos = xpos + 3 * ypos;
          if (pos >= 0 && pos < 9 && position[pos] == BLANK) {
            position[pos] = player;
            current = player == O ? X : O;

            updateBoth();
          }
        } finally {
          changingGamestate.unlock();
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

}