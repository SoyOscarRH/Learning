// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Pahua Castro Jesús Miguel Ángel

getd(pwd() + Directory);
clc;

x1 = [
    1; 
    2; 
    5; 
    6;
    7;
    8;
    10;
    13;
    17;
];

y1 = [
    3.0; 
    3.7; 
    3.9; 
    4.2;
    5.7;
    6.6;
    7.1;
    6.7;
    4.5;
];

x2 = [
    17; 
    20; 
    23; 
    24;
    25;
    27;
    27.7;
];

y2 = [
    4.5; 
    7.0; 
    6.1; 
    5.6;
    5.8;
    5.2;
    4.1;
];

x3 = [
    27.7; 
    28; 
    29; 
    30;
];

y3 = [
    4.1; 
    4.3; 
    4.1; 
    3.0;
];

points1 = linspace(1, 17)';
plot(0, 0, "*red")

[estimations1] = Spline3Interpolant(x1, y1, points1, 0);
plot(points1, estimations1, "blue-");
plot(x1, y1, "m*");

points2 = linspace(17, 27.7)';

[estimations2] = Spline3Interpolant(x2, y2, points2, 0);
plot(points2, estimations2, "red-");
plot(x2, y2, "m*");

points3 = linspace(27.7, 30, 20)';

[estimations3] = Spline3Interpolant(x3, y3, points3, 0);
plot(points3, estimations3, "m-");
plot(x3, y3, "m*");

xtitle("Estimate a Snoppy", "x", "y");