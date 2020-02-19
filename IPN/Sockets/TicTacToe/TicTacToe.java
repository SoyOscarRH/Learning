import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.Random;

public class TicTacToe extends JFrame implements ChangeListener, ActionListener {
  private Board board;
  static final char BLANK = ' ', O = 'O', X = 'X';

  // Board position (BLANK, O, or X)
  private char position[] = { BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK, BLANK };

  public static void main(String args[]) {
    new TicTacToe();
  }

  public TicTacToe() {
    super("Tic Tac Toe #n");
    var board = new Board();
    add(board, BorderLayout.CENTER);
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    setSize(400, 400);
    setVisible(true);
  }

  public void stateChanged(ChangeEvent e) {
    board.repaint();
  }

  public void actionPerformed(ActionEvent e) {
    board.repaint();
  }

  private class Board extends JPanel implements MouseListener {
    private Random random = new Random();

    private char current = O;
    private int rows[][] = { { 0, 2 }, { 3, 5 }, { 6, 8 }, { 0, 6 }, { 1, 7 }, { 2, 8 }, { 0, 8 }, { 2, 6 } };
    // Endpoints of the 8 rows in position[] (across, down, diagonally)

    public Board() {
      addMouseListener(this);
    }

    // Redraw the board
    public void paintComponent(Graphics g) {
      super.paintComponent(g);

      // Draw the grid
      var w = getWidth();
      var h = getHeight();
      var g2d = (Graphics2D) g;

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
        var xpos = (i % 3 + 0.5) * w / 3.0;
        var ypos = (i / 3 + 0.5) * h / 3.0;
        var xr = w / 8.0;
        var yr = h / 8.0;

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

    public void mouseClicked(MouseEvent e) {
      var xpos = e.getX() * 3 / getWidth();
      var ypos = e.getY() * 3 / getHeight();

      var pos = xpos + 3 * ypos;
      if (pos >= 0 && pos < 9 && position[pos] == BLANK) {
        position[pos] = current;
        current = current == O ? X : O;
        repaint();

        final var close = JOptionPane.CLOSED_OPTION;

        if (won(X)) {
          JOptionPane.showConfirmDialog(null, "You win X", "Result", close);
          System.exit(0);
        } else if (won(O)) {
          JOptionPane.showConfirmDialog(null, "You win O", "Result", close);
          System.exit(0);
        } else if (isDraw()) {
          JOptionPane.showConfirmDialog(null, "Draw", "Result", close);
          System.exit(0);
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
      for (int i = 0; i < 8; ++i)
        if (testRow(player, rows[i][0], rows[i][1]))
          return true;
      return false;
    }

    // Has player won in the row from position[a] to position[b]?
    boolean testRow(char player, int a, int b) {
      return position[a] == player && position[b] == player && position[(a + b) / 2] == player;
    }

    public void mousePressed(MouseEvent e) {
    }

    public void mouseReleased(MouseEvent e) {
    }

    public void mouseEntered(MouseEvent e) {
    }

    public void mouseExited(MouseEvent e) {
    }
  }
}