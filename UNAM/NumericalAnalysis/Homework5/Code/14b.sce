// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Pahua Castro Jesús Miguel Ángel

getd(pwd() + Directory);
clc;

points = [
    0.1;
    0.2;
    0.3;
    0.4;
]

valuations = [
    -0.620;
    -0.283;
    0.006;
    0.248;
]

pointsToEvaluate = linspace(-1, 1)'

pointsEvaluated = NewtonHomogeneousInterpolant(points, valuations, pointsToEvaluate)
pointsEvaluated2 = NewtonInterpolant(points, valuations, pointsToEvaluate)

plot(points, valuations, "*red")
plot(pointsToEvaluate, pointsEvaluated2, "-green")
plot(pointsToEvaluate, pointsEvaluated, "-blue")

xtitle("Newton Homogeneous Interpolant", "x", "f(x)");

