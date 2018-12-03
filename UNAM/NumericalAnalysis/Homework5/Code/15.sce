
getd(pwd() + Directory);
clc;

x = [1; 2; 3; 4; 5;];
y = [1; 1; 2; 6; 24;];

A = [
    1 x(1) x(1)^2 x(1)^3 x(1)^4
    1 x(2) x(2)^2 x(2)^3 x(2)^4
    1 x(3) x(3)^2 x(3)^3 x(3)^4
    1 x(4) x(4)^2 x(4)^3 x(4)^4
    1 x(5) x(5)^2 x(5)^3 x(5)^4
]

Coefficients = inv(A) * y
a0 = Coefficients(1)
a1 = Coefficients(2)
a2 = Coefficients(3)
a3 = Coefficients(4)
a4 = Coefficients(5)

disp(Coefficients)

function [x] = f(x)
    x = a0 + a1*x + a2*x^2 + a3*x^3 + a4*x^4 
endfunction

points = linspace(1, 5)';

[estimations] = f(points);
plot(points, estimations, "black-");

disp(f(2))

[estimations] = Spline3Interpolant(x, y, points, 0);
plot(points, estimations, "blue-");
plot(points, gamma(points), "red-");
plot(x, y, "m*");

legend(['Poly'; 'Spline'; 'Real gamma' ;'Points'])

xtitle("Gamma", "x", "y");