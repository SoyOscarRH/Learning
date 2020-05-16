#include <stdio.h>
#include <stdlib.h>

void show_final_message(const char* message) {
  fputs(message, stderr);
  fputc('\n', stderr);
  exit(1);
}
