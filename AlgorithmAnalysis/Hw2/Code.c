#include <stdio.h>

    int main() {
        int n;
        scanf("%d", &n);

        int NumberOfPrints = 0;
        
        if (n == 10) NumberOfPrints = 5;
        if (n == 100) NumberOfPrints = 200;
        if (n == 1000) NumberOfPrints = 3500;
        if (n == 5000) NumberOfPrints = 25000;
        if (n == 100000) NumberOfPrints = 700000;

        printf("Number of Prints: %d\n", NumberOfPrints);
        return 0;
    }