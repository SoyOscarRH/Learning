clc

function [y] = FowardSubstitution(L, b)
    [m, n] = size(L);
    y = zeros(n, 1);

    for i = (1 : n)
        if (L(i, i) == 0)
            error('Error: Singular matrix');
            return;
        end

        y(i) = b(i) / L(i, i);

        for j = (i + 1 : n)
            b(j) = b(j) - L(j, i) * y(i);
        end
    end
endfunction

function [x] = BackwardSubstitution(U, b)
    [m, n] = size(U);
    x = zeros(n, 1);

    for i = (n : -1 : 1)
        if (U(i, i) == 0)
            error('Error: Singular matrix');
            return;
        end

        x(i) = b(i) / U(i,i);

        for j = (1 : i - 1)
            b(j) = b(j) - U(j, i) * x(i);
        end
    end
endfunction

function[L, D] = CholeskyBanachiewicz(A, option)
    [m, n] = size(A);       
    D = eye(n, n);     
    L = eye(m, n);
    U = A; 

    for step = (1 : n - 1)
        if (A(step, step) == 0)
            error('Error: Singular matrix');
            return;
        end

        for row = (step + 1 : n)
            L(row, step) = U(row, step) / U(step, step);
            for column = (1 : n)
                U(row, column) = U(row, column) - L(row, step) * U(step, column);
            end
        end
    end

    if option == 1
        for step = (1 : n) 
            for row = (step : n) 
                L(row, step) = L(row, step) * sqrt(U(step, step));
            end
        end
    else
        for step = (1 : n) 
            D(step, step) = U(step, step);
        end
    end

endfunction


function [x] = LeastSquares(A, b)
    [L] = CholeskyBanachiewicz(A' * A, 1);

    y = FowardSubstitution(L, A' * b);
    x = BackwardSubstitution(L', y);
endfunction

function [a, b, c, d] = MinimazeWithCubic(a, b, c, f, MaxIterations, tolerance)
    iterations = 0

    while (iterations < MaxIterations)
        disp("a = " + string(a))
        disp("b = " + string(b))
        disp("c = " + string(c))
        disp("d = " + string(d))
        disp("")

        plot(a, f(a), ".b")
        plot(b, f(b), ".b")
        plot(c, f(c), ".b")
        plot(d, f(d), ".b")

        x = linspace(4, 7)
        [points, min] = EstimateCubic(a, b, c, d, f, x)

        if (iterations == 1)
            plot(x, points, "-black")
        elseif (iterations == 2)
            plot(x, points, "-m")
        else
            plot(x, points, "-g")
        end
        
        plot(min, f(min), "*black")
        disp("min = " + string(min))

        oldEstimation = abs(b - c);


        if (a < min && min < b) 
            d = c
            c = b
            b = min
            a = a
        elseif (b < min && min < c) 
            b = min
        else
            a = b
            b = c
            c = min
            d = d
        end

        if (abs(abs(b-c) - oldEstimation) < tolerance) then break end

        iterations = iterations + 1;
    end
endfunction


function [points, min] = EstimateCubic(a, b, c, d, f, points)

    A = [
        a^3 a^2 a^1 1;
        b^3 b^2 b^1 1;
        c^3 c^2 c^1 1;
        d^3 d^2 d^1 1;
    ]


    solutions = [
        f(a);
        f(b);
        f(c);
        f(d);
    ]

    [x] = LeastSquares(A, solutions);

    A = x(1)
    B = x(2)
    C = x(3)
    D = x(4)

    disp(x)

    points = A * (points)^3 + B * (points)^2 + C * points + D

    pointOfInflection1 = (-2*B + sqrt(4*B^2 - 12*A*C))/(6*A)
    pointOfInflection2 = (-2*B - sqrt(4*B^2 - 12*A*C))/(6*A)

    if (pointOfInflection1 < a || pointOfInflection1 > d) min = pointOfInflection2
    else min = pointOfInflection1 end

endfunction



function [x] = f(x)
    x = sin(x) + 2*cos(x+2)^2
endfunction

a = 4.6
b = 5
c = 6
d = 7.3

x = linspace(3, 9)
y = f(x)

plot(x, y, "-red")

[a, b, c] = MinimazeWithCubic(a, b, c, f, 4, 0.01);

disp("Minimum at " + string( (b + c) / 2 ));
hl=legend(['Original function';]);
xtitle("Estimate the minimums", "x", "y");