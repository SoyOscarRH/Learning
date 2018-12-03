// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Pahua Castro Jesús Miguel Ángel

getd(pwd() + Directory);
clc;


x = [1; 2; 3; 4; 5;];
y = [1; 1; 2; 6; 24;];

points = linspace(1, 5)';

[estimations] = Spline3Interpolant(x, y, points, 0);
plot(points, estimations, "black-");
plot(x, y, "m*");

xtitle("Gamma", "x", "y");