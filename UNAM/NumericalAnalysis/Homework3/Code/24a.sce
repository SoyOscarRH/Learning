// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Laurrabaquio Rodríguez Miguel Salvador
// @Author: Pahua Castro Jesús Miguel Ángel

getd('/Users/mac/Documents/Projects/Learning/UNAM/NumericalAnalysis/Homework3/Code/Algorithms')
clc;

x24 = [
    1.02;
    0.95;
    0.87;
    0.77;
    0.67;
    0.56;
    0.44;
    0.30;
    0.16;
    0.01;
]

y24 = [
    0.39;
    0.32;
    0.27;
    0.22;
    0.18;
    0.15;
    0.13;
    0.12;
    0.13;
    0.15;
]

A24 = eye(10, 5)
b24 = x24

for i = (1 : 10)
    A24(i, 1) = y24(i) * y24(i)
    A24(i, 2) = x24(i) * y24(i)
    A24(i, 3) = x24(i)
    A24(i, 4) = y24(i)
    A24(i, 5) = 1
    
    b24(i) = x24(i) * x24(i)
end

disp("Ax = b")

disp("xs:")
disp(x24)

disp("ys:")
disp(y24)

disp("A:")
disp(A24)

disp("b:")
disp(b24)

estimedx24 = LeastSquares(A24, b24)
disp("x:")
disp(estimedx24)

disp("Ax:")
disp(A24 * estimedx24)

clf()

function [x] = solveEquation(a, b, c)
    x = (-1 * b + sqrt(b*b - 4*a*c)) / (2 * a)
endfunction

function [y] = solve(coefficients, x)
    a = coefficients(1)
    b = coefficients(2)
    c = coefficients(3)
    d = coefficients(4)
    e = coefficients(5)

    reala = (a)
    realb = (b * x  + d)
    realc = (c * x  + e - x * x)

    y = solveEquation(reala, realb, realc)
endfunction

someX = linspace(0, 1.10, 50)
someY = someX

for i = (1 : 50)
    someY(i) = solve(estimedx24, someX(i))
end

plot(x24, y24, '.b')
plot(someX, someY, 'r-')
hl=legend(['Data'; 'Estimated Elipse']);
xtitle("Elipse estimation", "x-axis", "y-axis")