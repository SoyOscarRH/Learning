// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Pahua Castro Jesús Miguel Ángel

getd(pwd() + Directory);
clc;


function [x] = f(x)
    x = log(x + 1)
endfunction

points = [
    0;
    0.6;
    0.9;
]

valuations = [
    f(points(1));
    f(points(2));
    f(points(3));
]

derivatives = [
    ( f(points(1) + 0.00001) - f(points(1)) ) / (0.00001);
    ( f(points(2) + 0.00001) - f(points(2)) ) / (0.00001);
    ( f(points(3) + 0.00001) - f(points(3)) ) / (0.00001);
]

pointsToEvaluate = linspace(-0.1, 1, 30)'

pointsEvaluated1 = HermiteInterpolant(points, valuations, derivatives, pointsToEvaluate)
pointsEvaluated2 = LagrangeInterpolant(points, valuations, pointsToEvaluate)

plot(points, valuations, "*red")
plot(pointsToEvaluate, pointsEvaluated1, "-blue")
plot(pointsToEvaluate, pointsEvaluated2, "-green")
plot(pointsToEvaluate, f(pointsToEvaluate), "-m")

hl=legend(['Points'; 'Hermite'; 'Lagrange'; 'Real']);
xtitle("Hermite Interpolant", "x", "f(x)");

