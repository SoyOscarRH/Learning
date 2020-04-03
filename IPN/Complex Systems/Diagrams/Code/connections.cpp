#include <bitset>
#include <cstdint>
#include <iostream>
using namespace std;

auto show_rules(const uint8_t rule_id) {
  const auto rule = bitset<8>{rule_id};

  cout << "Rule " << rule.to_ulong() << endl;
  for (auto i = 0; i < 8; ++i) {
    const auto total = bitset<3>(i).to_string();
    const auto start = total.substr(0, 2);
    const auto end = total.substr(1, 2);
    cout << start << " to " << end << " -> " << rule[i] << endl;
  }
  cout << endl;
}

auto main() -> int {
  const auto rules = {15, 22, 30, 54, 90, 110};
  for (const auto rule : rules)
    show_rules(rule);

  return 0;
}