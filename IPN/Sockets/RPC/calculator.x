struct input {
  int input_1;
  int input_2;
};

program calculator {
  version calculator_demo {
    int sum(input) = 1;
    int substract(input) = 2;
    int multiply(input) = 3;
    float divide(input) = 4;
  } = 1;
} = 0x30000001;
