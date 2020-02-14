typedef const unsigned char byte_t;

byte_t num_to_7segments[] = {0x3F, 0x06, 0x5B, 0x4F, 0x66, 0x6D, 0x7D, 0x07,
                             0x7F, 0x6F, 0x77, 0x7C, 0x39, 0x5E, 0x79, 0x71};

byte_t lower = 0x0F, upper = 0xF0;

byte_t number_selected;
while (1) {
  number_selected = PIND & lower; // Just get the first 4 bits
  PORTB = num_to_7segments[number_selected];

  number_selected = (PIND & upper) >> 4; // Just get the last 4 bits
  PORTA = ~num_to_7segments[number_selected];
}
