// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Pahua Castro Jesús Miguel Ángel

getd(pwd() + Directory);
clc;

points = [
    -0.5;
    -0.25;
    0;
]

valuations = [
    -0.024;
    0.334;
    1.101;
]

derivatives = [
    0.751;
    2.189;
    4.002;
]

pointsToEvaluate = linspace(-1, 1, 20)'

pointsEvaluated = HermiteInterpolant(points, valuations, derivatives, pointsToEvaluate)

plot(points, valuations, "*red")
plot(pointsToEvaluate, pointsEvaluated, "-blue")

xtitle("Hermite Interpolant", "x", "f(x)");

