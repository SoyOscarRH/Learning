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

derivatives = [
    3.585;
    3.140;
    2.666;
    2.165;
]

pointsToEvaluate = linspace(-0.1, 0.6, 20)'

pointsEvaluated = HermiteInterpolant(points, valuations, derivatives, pointsToEvaluate)

plot(points, valuations, "*red")
plot(pointsToEvaluate, pointsEvaluated, "-blue")

xtitle("Hermite Interpolant", "x", "f(x)");

