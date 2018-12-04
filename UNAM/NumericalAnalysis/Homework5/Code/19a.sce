// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Pahua Castro Jesús Miguel Ángel

getd(pwd() + Directory);
clc;

//S0
X = [0, 0.6, 1, 1.8, 3, 4]
Y = [-3.2, -2, -1.4, -1, -0.4, 0]

points = linspace(0, 4);

[estimations1] = Spline3Interpolant(X, Y, points, 0);
plot(points, estimations1, "blue-");
plot(X, Y, "m*");

 //S1
 X = [0, 1, 2, 2.2, 3, 4]
 Y = [-3.2, -3, -2.2, -2, -1.2, 0]
 
 points = linspace(0, 4);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "m*");
 
 //S2
 X = [4, 4.2, 4.4, 4.8, 5.6, 6, 7.6, 8]
 Y = [0, 1, 2, 2.8, 4, 4.4, 5.2, 4.8]
 
 points = linspace(4, 8);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "m*");
 
 //S3
 X = [4, 5, 6, 7, 7.8, 8]
 Y = [0, 0.6, 1.6, 2.6, 3.8, 4.8]
 
 points = linspace(4, 8);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "m*");
 
 //S4
 X = [4, 5, 6, 7.2, 8, 9, 9.2]
 Y = [0, 0.2, 0.4, 0.8, 1.2, 1.8, 2.6]
 
 points = linspace(4, 9.2);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "m*");
 
 //S5
 X = [7.2, 7.4, 8, 9, 9.2]
 Y = [0.8, 1.6, 2.1, 2.7, 2.6]
 
 points = linspace(7.2, 9.2);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "m*");
 
 //S6
 X = [7.2, 7.4, 8, 9, 10, 11, 11.6, 12]
 Y = [0.8, 0.4, 0.2, 0.4, 0.8, 1.4, 2, 3]
 
 points = linspace(7.2, 12);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "m*");
 
 //S7
 X = [12, 12.4, 12.6]
 Y = [3, 2.4, 2]
 
 points = linspace(12, 12.6);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "m*");
 
 //S8
 X = [9.6, 10.4, 11, 11.6, 12, 12.4, 12.6]
 Y = [-0.1, -0.2, 0,  0.3, 0.6, 1, 2]
 
 points = linspace(9.6, 12.6);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "m*");
 
 //S9
 X = [9.6, 10, 11, 12, 13, 13.6, 14.4, 14.8]
 Y = [-0.1, 0.2, 0.3, 0.4, 0.6, 1, 2, 3]
 
 points = linspace(9.6, 14.8);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "m*");
 
 //S10
 X = [14.2, 14.3, 14.4]
 Y = [1, 1.6, 2]
 
 points = linspace(14.2, 14.4);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "m*");
 
 //S11
 X = [14.2, 14.6, 15, 15.8, 16.2, 16.6]
 Y = [1, 0.1, 0, 1, 2, 3]
 
 points = linspace(14.2, 16.6);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "m*");
 
 //S12
 X = [16.2, 16.4, 17, 18, 18.5, 18.8]
 Y = [2, 1.2, 1, 1.4, 2, 4]
 
 
 points = linspace(16.2, 18.8);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "m*");
 
 //S13
 X = [18.8, 19, 19.2]
 Y = [3, 2.5, 2]
 
 
 points = linspace(18.8, 19.2);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "m*");
 
 //S14
 X = [17.1, 18, 18.5, 19, 19.2]
 Y = [-0.1, 0, 0.4, 1, 2]
 
 
 points = linspace(17.1, 19.2);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "m*");
 
 //S15
 X = [17.1, 17.4, 18.5, 20.4]
 Y = [-0.1, 0.3, 0.4, 0.2]
 
 
 points = linspace(17.1, 20.4);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "m*");
 