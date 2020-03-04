#include <algorithm>
#include <bitset>
#include <cstdint>
#include <iostream>
#include <set>

using namespace std;
using num = uint8_t;

auto flip_bits(const num n) -> num {
  auto bits = bitset<8>{n};
  auto buffer = bits;

  buffer[0] = not bits[7];
  buffer[1] = not bits[6];
  buffer[2] = not bits[5];
  buffer[3] = not bits[4];
  buffer[4] = not bits[3];
  buffer[5] = not bits[2];
  buffer[6] = not bits[1];
  buffer[7] = not bits[0];

  return buffer.to_ulong();
}

auto flip_dir(const num n) -> num {
  auto bits = bitset<8>{n};
  auto buffer = bits;

  buffer[1] = bits[4];
  buffer[3] = bits[6];
  buffer[6] = bits[3];
  buffer[4] = bits[1];

  return buffer.to_ulong();
}

auto get_copies(const num n) -> tuple<num, num, num> {
  const auto ones = flip_bits(n);
  const auto dirs = flip_dir(n);
  const auto crazy = flip_dir(flip_bits(n));

  return {ones, dirs, crazy};
}

auto add_id(const set<num> &seen, set<int> &ids, num a, num b, num c, num d) {
  auto representant = 256;

  if (not seen.count(a))
    representant = min<int>(representant, a);
  if (not seen.count(b))
    representant = min<int>(representant, b);
  if (not seen.count(c))
    representant = min<int>(representant, c);
  if (not seen.count(d))
    representant = min<int>(representant, d);

  if (representant != 256)
    ids.insert(representant);
}

auto main() -> int {
  auto seen = set<num>{};
  auto representants = set<int>{};

  auto i = 0;
  for (auto i = 0; i < 256; ++i) {
    num a = i;
    auto [b, c, d] = get_copies(a);

    add_id(seen, representants, a, b, c, d);
    printf("%d %d %d %d\n", a, b, c, d);

    seen.insert({a, b, c, d});
  }

  cout << endl;
  for (auto key : representants) {
    cout << key << endl;
  }

  return 0;
}