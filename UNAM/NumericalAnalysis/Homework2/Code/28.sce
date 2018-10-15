// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Laurrabaquio Rodríguez Miguel Salvador
// @Author: Pahua Castro Jesús Miguel Ángel

getd('/Users/mac/Documents/Projects/Learning/UNAM/NumericalAnalysis/Homework2/Code/Algorithms')
clc;


// ================ 1 ===============
A28 = [
    2  -1  0;
    -1  2 -1;
    0  -1  2;
];

disp("A")
disp(A28)

[L28] = CholeskyBanachiewicz(A28, 1)

disp("L")
disp(L28)

disp("L * L^t")
disp(L28 * L28')

[L28, D28] = CholeskyBanachiewicz(A28, 0)
disp("L")
disp(L28)

disp("D")
disp(D28)

disp("L * D * L^t")
disp(L28 * D28 * L28')


// ================ 2 ===============
A28 = [
    4  1  1  1;
    1  3 -1  1;
    1 -1  2  0;
    1  1  0  2;
];

disp("A")
disp(A28)

[L28] = CholeskyBanachiewicz(A28, 1)

disp("L")
disp(L28)

disp("L * L^t")
disp(L28 * L28')

[L28, D28] = CholeskyBanachiewicz(A28, 0)
disp("L")
disp(L28)

disp("D")
disp(D28)

disp("L * D * L^t")
disp(L28 * D28 * L28')


// ================ 3 ===============
A28 = [
    4  1 -1  0;
    1  3 -1  0;
   -1 -1  5  2;
    0  0  2  4;
];

disp("A")
disp(A28)

[L28] = CholeskyBanachiewicz(A28, 1)

disp("L")
disp(L28)

disp("L * L^t")
disp(L28 * L28')

[L28, D28] = CholeskyBanachiewicz(A28, 0)
disp("L")
disp(L28)

disp("D")
disp(D28)

disp("L * D * L^t")
disp(L28 * D28 * L28')


// ================ 4 ===============
A28 = [
    6  2  1 -1;
    2  4  1  0;
    1  1  4 -1;
   -1  0 -1  3;
];
disp("A")
disp(A28)

[L28] = CholeskyBanachiewicz(A28, 1)

disp("L")
disp(L28)

disp("L * L^t")
disp(L28 * L28')

[L28, D28] = CholeskyBanachiewicz(A28, 0)
disp("L")
disp(L28)

disp("D")
disp(D28)

disp("L * D * L^t")
disp(L28 * D28 * L28')
