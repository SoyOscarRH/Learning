// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Laurrabaquio Rodríguez Miguel Salvador
// @Author: Pahua Castro Jesús Miguel Ángel

getd('/Users/mac/Documents/Projects/Learning/UNAM/NumericalAnalysis/Homework3/Code/Algorithms')
clc;

A23 = [
    0.16 0.10;
    0.17 0.11;
    2.02 1.29;
]

b23 = [
    0.26;
    0.28;
    3.31;
]

b2_23 = [
    0.27;
    0.25;
    3.33;
]

disp("Ax = b")

disp("A:")
disp(A23)

disp("b:")
disp(b23)

x23 = LeastSquares(A23, b23)
disp("x:")
disp(x23)

disp("Ax:")
disp(A23 * x23)

disp("b_2:")
disp(b2_23)

x2_23 = LeastSquares(A23, b2_23)
disp("x_2:")
disp(x2_23)

disp("Ax_2:")
disp(A23 * x2_23)
