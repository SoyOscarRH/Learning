#include <delay.h>
#include <mega8535.h>

// Global
typedef unsigned char u8;
typedef unsigned int u16;
typedef unsigned long int u32;

u8 one_hot_encoding(const u8 index) {
  u8 value = 0;
  value |= 1UL << index;

  return value;
}

void main(void) {
  // Local
  const u32 delay_of_column = 20;
  u8 col = 0, val_col, row;
  const u8 board[7][5] = {
    {1, 0, 0, 0, 0},
    {0, 1, 0, 0, 0},
    {0, 0, 1, 0, 0},
    {0, 0, 0, 1, 0},
    {0, 0, 0, 0, 1},
    {0, 0, 0, 1, 0},
    {0, 0, 1, 0, 0},
  };

  while (1) {
    for (col = 0; col < 5; ++col) {
      PORTC = one_hot_encoding(col);

      val_col = 0;
      for (row = 0; row < 7; ++row) val_col |= 1 << board[row][col];
      PORTD = val_col;

      delay_ms(delay_of_column);
    }
  }
}
}
