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

pointsToEvaluate = linspace(-1, 1)

pointsEvaluated = NewtonInterpolant(points, valuations, pointsToEvaluate)

plot(points, valuations, "*red")
plot(pointsToEvaluate, pointsEvaluated, "-blue")
