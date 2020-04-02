#include <bitset>
#include <cstdint>
#include <iostream>
#include <unordered_set>
#include <algorithm>
using namespace std;

class Solution {
public:
  auto isHappy(const int n) -> bool {
    auto visited = unordered_set<int>{};
    auto current = n;

    while (true) {
      auto next = 0;
      for (auto digit = 0; current != 0; current /= 10) {
        digit = current % 10;
        next += digit * digit;
      }

      if (next == 1) return true;
      if (visited.count(next)) return false;

      visited.insert(next);
      current = next;
    }

    return false;
  }
};

auto main() -> int {
  cout << Solution().isHappy(2) << endl;
  return 0;
}