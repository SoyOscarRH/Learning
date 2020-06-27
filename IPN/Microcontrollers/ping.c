#include <delay.h>
#include <mega8535.h>

typedef unsigned char u8;
typedef unsigned int u16;
typedef unsigned long int u32;

const u32 delay_of_column = 10;
u8 player1_x = 0, player2_x = 0;
u8 board[7][5];
u8 ball_x = 2, ball_y = 2, up = 0, left = 1;
u8 score_player1 = 0;
u8 score_player2 = 0;
u8 num_to_7segments[10] = {0x3F, 0x06, 0x5B, 0x4F, 0x66,
                           0x6D, 0x7D, 0x07, 0x7F, 0x6F};

u8 one_hot_encoding(const u8 index) {
  u8 value = 0;
  value |= 1UL << index;

  return value;
}
void update_players_table() {
  if (PINB.0 == 1) player1_x++;
  if (PINB.1 == 1) player1_x--;
  if (PINB.2 == 1) player2_x++;
  if (PINB.3 == 1) player2_x--;

  if (player1_x == 4) player1_x = 3;
  if (player2_x == 4) player2_x = 3;
  if (player1_x == 255) player1_x = 0;
  if (player2_x == 255) player2_x = 0;

  board[0][player1_x] = 1;
  board[0][player1_x + 1] = 1;

  board[6][player2_x] = 1;
  board[6][player2_x + 1] = 1;
}

void clear() {
  u8 col = 0, row = 0;
  for (row = 0; row < 7; ++row) {
    for (col = 0; col < 5; ++col) {
      board[row][col] = 0;
    }
  }
}

void paint_screen() {
  u8 col = 0, row = 0, current = 0;
  for (col = 0; col < 5; ++col) {
    PORTC = one_hot_encoding(col);
    PORTC .6 = 1;
    current = 0;
    for (row = 0; row < 7; ++row) {
      if (board[row][col]) current |= 1 << row;
    }

    PORTD = ~current;
    delay_ms(delay_of_column);
  }
}

void move_ball() {
  (left == 1) ? ball_x++ : ball_x--;

  (up == 1) ? ball_y-- : ball_y++;

  if (ball_x == 255) {
    ball_x += 2;
    left = 1;
  }

  if (ball_x == 5) {
    ball_x -= 2;
    left = 0;
  }

  if (ball_y == 0) {
    if (ball_x == player1_x || ball_x == player1_x + 1) {
      ball_y += 2;
      up = 0;
    } else {
      score_player2++;
      ball_x = 3;
      ball_y = 3;
      delay_ms(1000);
      up = 0;
    }
  }

  if (ball_y == 6) {
    if (ball_x == player2_x || ball_x == player2_x + 1) {
      ball_y -= 2;
      up = 1;
    } else {
      score_player1++;
      ball_x = 3;
      ball_y = 3;
      delay_ms(1000);
      up = 0;
    }
  }
}

void main(void) {
  u32 milis = 0;

  while (1) {
    clear();
    milis += 5 * delay_of_column;
    if (milis >= 500) {
      move_ball();
      milis = 0;
    }

    board[ball_y][ball_x] = 1;
    update_players_table();
    PORTA = num_to_7segments[score_player1];
    PORTC.6 = 1;
    PORTC.7 = 0;

    paint_screen();
    PORTA = num_to_7segments[score_player2];
    PORTC.7 = 1;
    PORTC.6 = 0;
  }
}