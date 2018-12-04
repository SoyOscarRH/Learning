// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Pahua Castro Jesús Miguel Ángel

getd(pwd() + Directory);
clc;

 //S1
 X = [0.7, 0.8, 1.3, 2.4, 3.5, 5]
 Y = [4.2, 5.2, 6.4, 7.7, 8.5, 9]
 
 points = linspace(0.7, 5);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "blue*");
 
 //S2
 X = [0.7, 1.1, 1.6, 2.3, 3, 3.4, 3.9, 4.2, 4.7, 5]
 Y = [4.2, 3.4, 3, 3, 3.7, 4.5, 6, 7, 8, 9]
 
 points = linspace(0.7, 5);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "blue*");
 
 //S3
 X = [3.6, 4.5, 5, 5.5, 5.6]
 Y = [5, 3, 3.5, 4.5, 4.8]
 
 points = linspace(3.6, 5.6);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "blue*");

 //S5
 X = [5.6, 5.7, 6.0, 6.3, 6.4, 6.7]
 Y = [4.8, 3.8, 3.2, 4.1, 5, 3.5]
 
 points = linspace(5.6, 6.7);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "blue*");
 
 //S6
 X = [5.6, 5.9, 6.3, 6.7]
 Y = [0.8, 1.5, 3.0, 3.5]

 points = linspace(5.6, 6.7);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "blue*");
 
 //S7
X = [4.9, 5.3, 5.6]
Y = [0.9, 0.5, 0.8]

points = linspace(4.9, 5.6);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "blue*");
 
 //S8
 X = [4.9, 5.2, 5.6, 6.3, 7, 7.7, 8.5, 9, 9.5, 9.9, 10]
 Y = [0.9, 1.8, 2.4, 3, 3.6, 4.3, 5.3, 6, 7, 8, 8.9]
 
 points = linspace(4.9, 10);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "blue*");
 
 //S9
 X = [ 7.9, 8, 8.2, 8.6, 9, 10]
 Y = [ 3, 5, 6, 7, 8, 8.9]
 
 points = linspace(7.9, 10);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "blue*");
 
 //S10
 X = [8, 9, 9.7, 10.2]
 Y = [3, 3.8, 5, 6]
 
 points = linspace(8, 10.2);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "blue*");
 
 //S11
 X = [10.2, 10.6, 10.9, 11.1, 11.2]
 Y = [6, 5, 4.5, 4, 3]

 points = linspace(10.2, 11.2);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "blue*");
 plot(10.2, 7, "blue*");
 
 //S12
 X = [11.2, 11.5, 11.7, 11.8]
 Y = [3, 4, 5, 6]

 points = linspace(11.2, 11.8);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "blue*");
 
 //S13
 X = [11.2, 11.5, 11.9, 12.8]
 Y = [3, 4, 5, 6]
 
 points = linspace(11.2, 12.8);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "blue*");
 
 //S14
 X = [12.5, 12.6, 12.8, 12.6, 12.9]
 Y = [3.4, 4, 5, 6, 6]

 points = linspace(12.5, 12.9);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "blue*");
 
 //S15
 X = [12.5, 12.8, 13.5, 14, 14.5]
 Y = [3.4, 3.1, 3.8, 4.5, 4]

 points = linspace(12.5, 15);
 
 [estimations1] = Spline3Interpolant(X, Y, points, 0);
 plot(points, estimations1, "blue-");
 plot(X, Y, "blue*");
 