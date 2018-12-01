clc

function [a, b, c] = MinimazeWithCuadratic(a, b, c, f, MaxIterations, tolerance)
    iterations = 0;

    while (iterations < MaxIterations)
        disp("a = " + string(a))
        disp("b = " + string(b))
        disp("c = " + string(c))
        disp("")

        plot(a, f(a), ".r")
        plot(b, f(b), ".r")
        plot(c, f(c), ".r")

        x = linspace(a, c)
        [minX, minY, yParabola] = EstimateParabola(a, b, c, f, x)
        plot(x, yParabola, "-cyan")
        plot(minX, minY, "*black")

        oldEstimation = b;

        if (a < minX && minX < b) 
            a = a
            b = minX
            c = b
        else
            a = b
            b = minX
            c = c
        end

        if (abs (oldEstimation - b) < tolerance) then break end

        iterations = iterations + 1;
    end
endfunction

function [minX, minY, pointsOut] = EstimateParabola(a, b, c, f, pointsIn)
    denom = (a - b) * (a - c) * (b - c)
    A = (c * (f(b) - f(a)) + b * (f(a) - f(c)) + a * (f(c) - f(b))) / denom
    B = (c^2 * (f(a) - f(b)) + b^2 * (f(c) - f(a)) + a^2 * (f(b) - f(c))) / denom
    C = (b * c * (b - c) * f(a) + c * a * (c - a) * f(b) + a * b * (a - b) * f(c)) / denom

    pointsOut = A * (pointsIn)^2 + B * (pointsIn) + C
    minX = -B / (2 * A)
    minY = C - B * B / (4 * A)
endfunction

function [x] = f(x)
    x = x^3 - 6*x^2 - 15*x + 2
endfunction

a = 0
b = 1
c = 9

x = linspace(-0.5, 10)
y = f(x)

plot(x, y)

[a, b, c] = MinimazeWithCuadratic(a, b, c, f, 6, 0.001);

disp("Minimum at " + string(b))
xtitle("Estimate the minimums", "x", "y");