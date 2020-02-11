clear

g++ -std=c++17 \
  -Wall -Wextra -Wshadow -Wpedantic -fsanitize=undefined -fsanitize=address \
  -O2 main.cpp

./a.out > main.out