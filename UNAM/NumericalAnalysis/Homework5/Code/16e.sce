// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Pahua Castro Jesús Miguel Ángel

getd(pwd() + Directory);
clc;

x = [
    1900;
    1910;
    1920;
    1930;
    1940;
    1950;
    1960;
    1970;
    1980;
];

y = [
    076212168;
    092228496;
    106021537;
    123202624;
    132164569;
    151325798;
    179323175;
    203302031;
    226542199;
];

function [y] = f4(t, j)
    y =  ( (t - 1940) / 40 )^(j -1)
endfunction

A4 = [
    f4(x(1), 1)  f4(x(1), 2)  f4(x(1), 3)  f4(x(1), 4)  f4(x(1), 5)  f4(x(1), 6)  f4(x(1), 7)  f4(x(1), 8)  f4(x(1), 9)  
    f4(x(2), 1)  f4(x(2), 2)  f4(x(2), 3)  f4(x(2), 4)  f4(x(2), 5)  f4(x(2), 6)  f4(x(2), 7)  f4(x(2), 8)  f4(x(2), 9)  
    f4(x(3), 1)  f4(x(3), 2)  f4(x(3), 3)  f4(x(3), 4)  f4(x(3), 5)  f4(x(3), 6)  f4(x(3), 7)  f4(x(3), 8)  f4(x(3), 9)  
    f4(x(4), 1)  f4(x(4), 2)  f4(x(4), 3)  f4(x(4), 4)  f4(x(4), 5)  f4(x(4), 6)  f4(x(4), 7)  f4(x(4), 8)  f4(x(4), 9)  
    f4(x(5), 1)  f4(x(5), 2)  f4(x(5), 3)  f4(x(5), 4)  f4(x(5), 5)  f4(x(5), 6)  f4(x(5), 7)  f4(x(5), 8)  f4(x(5), 9)  
    f4(x(6), 1)  f4(x(6), 2)  f4(x(6), 3)  f4(x(6), 4)  f4(x(6), 5)  f4(x(6), 6)  f4(x(6), 7)  f4(x(6), 8)  f4(x(6), 9)  
    f4(x(7), 1)  f4(x(7), 2)  f4(x(7), 3)  f4(x(7), 4)  f4(x(7), 5)  f4(x(7), 6)  f4(x(7), 7)  f4(x(7), 8)  f4(x(7), 9)  
    f4(x(8), 1)  f4(x(8), 2)  f4(x(8), 3)  f4(x(8), 4)  f4(x(8), 5)  f4(x(8), 6)  f4(x(8), 7)  f4(x(8), 8)  f4(x(8), 9)  
    f4(x(9), 1)  f4(x(9), 2)  f4(x(9), 3)  f4(x(9), 4)  f4(x(9), 5)  f4(x(9), 6)  f4(x(9), 7)  f4(x(9), 8)  f4(x(9), 9)  
]

disp(cond(A1))
disp(cond(A2))
disp(cond(A3))
disp(cond(A4))

Coefficients = inv(A4) * y
a0 = Coefficients(1)
a1 = Coefficients(2)
a2 = Coefficients(3)
a3 = Coefficients(4)
a4 = Coefficients(5)
a5 = Coefficients(6)
a6 = Coefficients(7)
a7 = Coefficients(8)
a8 = Coefficients(9)

disp(Coefficients)

function [x] = f(x)
    x = a0*f4(x, 1) + a1*f4(x, 2) + a2*f4(x, 3) + a3*f4(x, 4) + a4*f4(x, 5) + a5*f4(x, 6) + a6*f4(x, 7) + a7*f4(x, 8) + + a8*f4(x, 9) 
endfunction

points = linspace(1900, 1990)';

[estimations] = f(points);
plot(points, estimations, "black-");
plot(x, y, "m*");

[estimations2] = LagrangeInterpolant(x, y, points);
plot(points, estimations2, "m-");

legend(['Poly' ;'Points'; 'Lagrange'])

xtitle("Population data", "year", "population");
