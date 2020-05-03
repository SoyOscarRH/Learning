#include <delay.h>
#include <mega8535.h>

typedef unsigned char u8;
typedef unsigned int u32;

// HERE IT GOES CODEVISION CRAP

void main(void) {
  // HERE IT GOES CODEVISION CRAP

  const u32 MAX_VALUE = 300;
  const u32 DELAY_IN_A_DIGIT = 1;
  const u8 num_to_display[10] = {0x3f, 0x06, 0x5b, 0x4f, 0x66,
                                 0x6d, 0x7c, 0x07, 0x7f, 0x6f};

  u32 units, tens, hundreds;
  u32 current;

  while (1) {
    current = (read_adc(0) * MAX_VALUE) / 255;

    units = current % 10;
    tens = (current / 10) % 10;
    hundreds = current / 100;

    PORTC.0 = 1;
    PORTC.1 = 0;
    PORTC.2 = 0;
    PORTB = num_to_display[units];
    delay_ms(DELAY_IN_A_DIGIT);

    PORTC.0 = 0;
    PORTC.1 = 1;
    PORTC.2 = 0;
    PORTB = num_to_display[tens];
    delay_ms(DELAY_IN_A_DIGIT);

    PORTC.0 = 0;
    PORTC.1 = 0;
    PORTC.2 = 1;
    PORTB = num_to_display[hundreds];
    delay_ms(DELAY_IN_A_DIGIT);
  }
}