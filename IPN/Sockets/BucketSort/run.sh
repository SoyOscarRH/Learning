gcc -std=c11 \
  -Wall -Wextra -Wshadow -Wpedantic -fsanitize=undefined -fsanitize=address \
  -O2 main.c -lpthread 

./a.out 20 > bucket.out