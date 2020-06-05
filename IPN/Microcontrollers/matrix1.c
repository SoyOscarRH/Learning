#include <delay.h>
#include <mega8535.h>

// Global
typedef unsigned char u8;
typedef unsigned int u16;
typedef unsigned long int u32;

u8 one_hot_encoding(const u8 index) {
  const u8 value = 0;
  value |= 1UL << index;

  return value;
}

void main(void) {
  // Local
  const u32 delay_of_column = 3 * 1000;
  const u32 time_of_figure = 20 * 1000;
  const u8 data_for_column[5] = {0x1, 0x2, 0x4, 0x8, 0x10};

  u8 col = 0;
  u32 time = 0;

  while (1) {
    for (; time < time_of_figure; time += delay_of_column, ++col) {
      col = col % 5;

      PORTC = ~one_hot_encoding(col);
      PORTD = data_for_column[col];

      delay_ms(delay_of_column);
    }
  }
}
