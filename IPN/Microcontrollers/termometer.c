#include <mega8535.h>
#include <delay.h>

typedef unsigned char u8;
typedef unsigned int u16;
typedef unsigned long int u32;

typedef float f16;

void main(void) {
  const u32 MAX_VALUE = 1100;
  const u32 ADC_MAX = 1024;
  const u32 DELAY_IN_A_DIGIT_MS = 40;

  const u8 num_to_display[10] = {0x3f, 0x06, 0x5b, 0x4f, 0x66,
                                 0x6d, 0x7c, 0x07, 0x7f, 0x6f};

  u32 units, tens, hundreds;
  u32 current;
  f16 farenheite, celsius;

  while (1) {
    current = (read_adc(0) * MAX_VALUE) / ADC_MAX;

    celsius = current;
    farenheite = (1.8 * celsius) + 320;
    
    current = PIND.0? farenheite : celsius;

    units = current % 10;
    tens = (current / 10) % 10;
    hundreds = current / 100;

    PORTC .0 = 0;
    PORTC .1 = 0;
    PORTC .2 = 1;
    PORTB = num_to_display[units];
    delay_ms(DELAY_IN_A_DIGIT_MS);

    PORTC .0 = 0;
    PORTC .1 = 1;
    PORTC .2 = 0;
    PORTB = num_to_display[tens];
    delay_ms(DELAY_IN_A_DIGIT_MS);

    PORTC .0 = 1;
    PORTC .1 = 0;
    PORTC .2 = 0;
    PORTB = num_to_display[hundreds];
    delay_ms(DELAY_IN_A_DIGIT_MS);
  }
}
