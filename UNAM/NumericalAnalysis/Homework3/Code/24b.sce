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

someX = linspace(0, 1, 50)'
someY = someX

for i = (1 : 50)
    someY(i) = solve(estimedx24, someX(i))
end

x24_2 = eye(10, 1)
y24_2 = eye(10, 1)

A24_2 = eye(10, 5)
b24_2 = x24_2

for i = (1 : 10)
    x24_2(i) = x24(i) + (rand() * 0.010 - 0.005)
    y24_2(i) = y24(i) + (rand() * 0.010 - 0.005)

    A24_2(i, 1) = y24_2(i) * y24_2(i)
    A24_2(i, 2) = x24_2(i) * y24_2(i)
    A24_2(i, 3) = x24_2(i)
    A24_2(i, 4) = y24_2(i)
    A24_2(i, 5) = 1
    
    b24_2(i) = x24_2(i) * x24_2(i)
end

estimedx24_2 = LeastSquares(A24_2, b24_2)

clf()

someX_2 = linspace(0, 1, 50)'
someY_2 = someX_2

for i = (1 : 50)
    someY_2(i) = solve(estimedx24_2, someX_2(i))
end


plot(x24, y24, '.b')
plot(someX, someY, 'r-')

disp("====")
disp(someX_2)
disp(someY_2)

plot(x24, y24, '.b')
plot(someX, someY, 'r-')

plot(x24_2, y24_2, '.green')
plot(someX_2, someY_2, 'black-')

hl=legend(['Data'; 'Estimated Elipse'; 'Data 2'; 'Estimated Elipse 2']);
xtitle("Elipse estimation", "x-axis", "y-axis")