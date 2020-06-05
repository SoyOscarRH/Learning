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
  const u32 delay_of_column = 10;
  const u32 time_of_figure = 1 * 1000;
  const u8 data_for_column[9][5] = {
      {0x41, 0x2e, 0x36, 0x3a, 0x41},  // 0
      {0x3f, 0x3d, 0x00, 0x3f, 0x3f},  // 1
      {0x3d, 0x1e, 0x2e, 0x36, 0x39},  // 2
      {0x5d, 0x3e, 0x36, 0x36, 0x49},  // 3
      {0x67, 0x6b, 0x6d, 0x00, 0x7f},  // 4
      {0x58, 0x3a, 0x3a, 0x3a, 0x46},  // 5
      {0x43, 0x35, 0x36, 0x36, 0x4f},  // 6
      {0x7c, 0x7e, 0x0e, 0x76, 0x78},  // 7
      {0x49, 0x36, 0x36, 0x36, 0x49},  // 8
      {0x79, 0x66, 0x66, 0x56, 0x69}   // 9
  };

  u8 current_col = 0;
  u8 current_num = 0;
  u32 time = 0;

  while (1) {
    for (current_num = 0; current_num < 10; ++current_num) {
      for (time = 0; time < time_of_figure; time += delay_of_column, ++current_col) {
        current_col = current_col % 5;
        PORTC = one_hot_encoding(current_col);
        PORTD = data_for_column[current_num][current_col];
        delay_ms(delay_of_column);
      }
    }
  }
}
