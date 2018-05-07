int Tribonacci(int num) {
    if (num == 0)
        return 0;
    else if (num == 1 or num == 2)
        return 1;
    else
        return Tribonacci(num-1) + Tribonacci(num-2) + Tribonacci(num-3);
}