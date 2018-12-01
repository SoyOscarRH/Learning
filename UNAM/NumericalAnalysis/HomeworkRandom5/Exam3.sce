
function [a, b, c] = interpolacionCuadrado(a, b, c, f, Mintentos, tol)
    iterations = 0;

    while (iterations < Mintentos)
        disp("a = " + string(a))
        disp("b = " + string(b))
        disp("c = " + string(c))
        disp("")

        plot(a, f(a), ".b")
        plot(b, f(b), ".b")
        plot(c, f(c), ".b")

        x = linspace(a, c)
        [yParabola, minimoX, minimoY] = intentoParabola(a, b, c, f, x)
        plot(x, yParabola, "-blue")
        plot(minimoX, minimoY, "*black")

        oldEstimation = b;

        if (a < minimoX && minimoX < b) 
            a = a
            b = minimoX
            c = b
        else
            a = b
            b = minimoX
            c = c
        end

        if (abs (oldEstimation - b) < tol) then break end

        iterations = iterations + 1;
    end
endfunction


function [puntos, minimoX, minimoY] = intentoParabola(a, b, c, f, puntos)
    denom = (a - b)*(a - c)*(b - c)
    A = (c * (f(b) - f(a)) + b * (f(a) - f(c)) + a * (f(c) - f(b))) / denom
    B = (c^2 * (f(a) - f(b)) + b^2 * (f(c) - f(a)) + a^2 * (f(b) - f(c))) / denom
    C = (b * c * (b - c) * f(a) + c * a * (c - a) * f(b) + a * b * (a - b) * f(c)) / denom

    puntos = A*(puntos)^2 + B*(puntos) + C
    minimoX = -B / (2*A)
    minimoY = C - B*B / (4*A)
endfunction

function [x] = f(x)
    x = 2*x^3 - 25 * x^2 - 12 * x + 15
endfunction

a = 1
b = 2
c = 13.5


plot(a, f(a), ".green")
plot(b, f(b), ".green")
plot(c, f(c), ".green")

x = linspace(1, 14)
y = f(x)

plot(x, y)

[a, b, c] = interpolacionCuadrado(a, b, c, f, 4, 0.001);

disp(b)