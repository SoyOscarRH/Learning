clc

function [a, b, c] = MinimazeWithCuadratic(a, b, c, f, MaxIterations, tolerance)
    iterations = 0;

    while (iterations < MaxIterations)
        disp("$a = " + string(a) + "$")
        disp("$b = " + string(b) + "$")
        disp("$c = " + string(c) + "$")
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
            c = b
            b = minX
            a = a
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


    disp("$Ax^2 + Bx + C$ donde:")
    disp("$A = " + string(A) + "$")
    disp("$B = " + string(B) + "$")
    disp("$C = " + string(C) + "$")
    disp("")
    
    pointsOut = A * (pointsIn)^2 + B * (pointsIn) + C
    minX = -B / (2 * A)
    minY = C - B * B / (4 * A)

    disp("The min is in:")
    disp("min =" + string(minX))
endfunction

function [x] = f(x)
    x = 3*x^3 + 7*x^2 - 15*x - 3
endfunction

a = -2
b = 1.2
c = 3

x = linspace(-3, 3.1)
y = f(x)

plot(x, y)

[a, b, c] = MinimazeWithCuadratic(a, b, c, f, 5, 0.001);

disp("Minimum at " + string(b))
xtitle("Estimate the minimums", "x", "y");