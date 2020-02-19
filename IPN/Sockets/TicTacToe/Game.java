import javax.swing.*;

public class Game {
  public static void main(String args[]) {
    final var plays = JOptionPane.showInputDialog(null, "How many games?");
    for (var i = 0; i < Integer.valueOf(plays); ++i) {
      new TicTacToe();
    }
  }
}
