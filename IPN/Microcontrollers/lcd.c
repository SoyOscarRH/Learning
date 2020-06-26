#include <alcd.h>
#include <delay.h>
#include <mega8535.h>

// Declare your global variables here
typedef unsigned char u8;
typedef unsigned int u16;
typedef unsigned long int u32;
typedef float f16;

void print_lcd(const char data[], const int n, const int x, const int y) {
  int i = 0;
  for (i = 0; i < n; ++i) {
    lcd_gotoxy(x + i, y);
    lcd_putchar(data[i]);
  }
}

u32 get(const char data[11], const int i) { return (data[i] - '0') % 10; }

void main(void) {
  char text[6] = "ESCOM";
  char date[11] = "XXXX-XX-XX";
  char time[11] = "XX:XX:XX";
  char termometer[11] = "12.3C";
  const u32 DELAY_IN_A_DIGIT_MS = 50;
  u32 milis = 0;
  u32 temperature;
  u32 second = 45, minute = 59, hour = 23;
  u32 day = 30, month = 06, year = 2014;
  u32 select_is_up = 1;

  lcd_init(16);
  print_lcd(text, 6, 11, 0);

  while (1) {
    temperature = read_adc(0) * 1.0638;
    termometer[3] = '0' + temperature % 10;
    termometer[1] = '0' + (temperature / 10) % 10;
    termometer[0] = '0' + (temperature / 100) % 10;

    milis += DELAY_IN_A_DIGIT_MS;
    if (milis >= 500) {
      milis = 0;

      second++;
      if (second == 60) {
        second = 0;
        minute++;
        if (minute == 60) {
          minute = 0;
          hour++;
          if (hour == 24) {
            day++;
            hour = 0;
          }
        }
      }

      time[0] = '0' + hour / 10;
      time[1] = '0' + hour % 10;

      time[3] = '0' + minute / 10;
      time[4] = '0' + minute % 10;

      time[6] = '0' + second / 10;
      time[7] = '0' + second % 10;
    }

    if (day == 31) {
      day = 1;
      month++;
      if (month == 13) month = 1;
    }

    date[3] = '0' + ((year / 1) % 10);
    date[2] = '0' + ((year / 10) % 10);
    date[1] = '0' + ((year / 100) % 10);
    date[0] = '0' + ((year / 1000) % 10);

    date[6] = '0' + ((month / 1) % 10);
    date[5] = '0' + ((month / 10) % 10);

    date[9] = '0' + ((day / 1) % 10);
    date[8] = '0' + ((day / 10) % 10);

    if (PIND .0 == 1) select_is_up = !select_is_up;
    if (PIND .1 == 1) select_is_up ? year++ : hour++;
    if (PIND .2 == 1) select_is_up ? month++ : minute++;
    if (PIND .3 == 1) select_is_up ? day++ : second++;
    lcd_gotoxy(9, 1);
    lcd_putchar(select_is_up ? 'U' : 'D');

    print_lcd(date, 11, 0, 0);
    print_lcd(time, 9, 0, 1);
    print_lcd(termometer, 7, 11, 1);

    delay_ms(DELAY_IN_A_DIGIT_MS);
  }
}