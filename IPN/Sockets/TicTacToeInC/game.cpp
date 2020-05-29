struct game {
  static constexpr char PLAYER1 = '#', PLAYER2 = 'X';
  static constexpr int index_of[9] = {1, 5, 9, 13, 17, 21, 25, 29, 33};
  id players[2];
  id is_turn_of = -1;
  int name;

  char board_display[38] = " 1 | 2 | 3 \n 4 | 5 | 6 \n 7 | 8 | 9 \n\n";
  auto& element_at(int i) { return board_display[index_of[i - 1]]; }

  auto is_viable(const int index) {
    if (index > 9 or index < 1) return false;
    const auto current = element_at(index);
    return current != PLAYER1 and current != PLAYER2;
  }

  auto set(const id player, const int index) {
    if (index > 9 or index < 1) return;
    element_at(index) = player == players[0] ? PLAYER1 : PLAYER2;
  }

  static constexpr int winner_moves[8][3] = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}, {1, 4, 7}, {2, 5, 8},
                                         {3, 6, 9}, {1, 5, 9}, {3, 5, 7}};
  auto winner() {
    for (const auto& move : winner_moves) {
      if ((element_at(move[0]) == PLAYER1 or element_at(move[0]) == PLAYER2) and
          element_at(move[0]) == element_at(move[1]) and
          element_at(move[1]) == element_at(move[2])) {
        return players[element_at(move[0]) == PLAYER1 ? 0 : 1];
      }
    }

    return -1;
  }
};