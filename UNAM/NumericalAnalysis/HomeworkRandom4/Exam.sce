// ==================================================================
// ===================   AUREAL SECTION        ======================
// ==================================================================

// /**
//  * Function to aproximate a minimum of f(x)
//  *
//  * @param a - a number such f is unimodal in [a, b] 
//  * @param b - a number such f is unimodal in [a, b] 
//  * @param f - a function :v
//  * @param tolerance - a number to set how exact you want a minimum radio
//  * @param MaxIterations - a number of maximum iterations
//  * @return a - a number such that the minimum is in [a, b]
//  * @return b - a number such that the minimum is in [a, b]
//  *
//  * @author: Rosas Hernandez Oscar Andres
//  * @author: Alarcón Alvarez Aylin Yadira Guadalupe
//  * @author: Laurrabaquio Rodríguez Miguel Salvador
//  * @author: Pahua Castro Jesús Miguel Ángel
//  */
function [a, b] = AurealSection(a, b, f, tolerance, MaxIterations)
    iterations = 0;
    tau = 0.61803398874989484
    x1 = a + (1 - tau) * (b - a)
    x2 = a + (tau) * (b - a)

    while (iterations < MaxIterations)
        if (f(x1) < f(x2))
            b = x2
            x2 = x1
            x1 = a + (1 - tau) * (b - a)
        else 
            a = x1
            x1 = x2
            x2 = a + (tau) * (b - a)
        end

        if (b - a < tolerance) break;
        end;

        iterations = iterations + 1;
    end
endfunction



// ==================================================================
// ===================           SPLINE        ======================
// ==================================================================
function [z, h] = Spline3(t, y)
    n = length(t);      

    h = zeros(n - 1, 1);
    b = zeros(n - 1, 1);
    u = zeros(n - 2, 1);
    v = zeros(n - 2, 1);
    z = zeros(n, 1);

    for i = (1 : n - 1)
        h(i) =  t(i+1) - t(i);
        b(i) = 6 * (y(i+1) - y(i)) / h(i);
    end

    u(2) = 2 * (h(1) + h(2));
    v(2) = b(2) - b(1);

    for i = (3 : n - 1)
        u(i) = 2 * (h(i) + h(i-1)) - h(i-1)**2 / u(i-1);
        v(i) = b(i) - b(i-1) - h(i-1) * v(i-1) / u(i-1);
    end   

    z(n) = 0;
    for i = (n - 1 : -1 : 2)
        z(i) = (v(i) - h(i) * z(i+1)) / u(i);
    end
    z(1) = 0;
endfunction   

function [i] = FindPoint(point, data) 
    middle = 1, start = 1
    final = length(data)

    while (start < final)
        middle = start + floor((final - start) / 2)

        if (point < data(middle)) then final = middle;
        else start = middle + 1;
        end
    end

    i = start - 1;
endfunction

function [estimation] = EvaluateSpline3(x, y, z, h, point)
    i = FindPoint(point, x);

    sum1 = z(i + 1) / (6 * h(i)) * (point - x(i))^3;
    sum2 = z(i) / (6 * h(i)) * (x(i+1) - point)^3;
    sum3 = ( y(i+1)/h(i) - z(i+1)/6 * h(i) ) * (point - x(i));
    sum4 = ( y(i)/h(i) - z(i)/6 * h(i) ) * (x(i+1) - point);

    estimation = sum1 + sum2 + sum3 + sum4;

endfunction

function [estimations] = EvaluateSpline3Points(x, y, z, h, points)
    dimension = length(points);
    estimations = zeros(dimension, 1);

    for i = (1 : dimension)
        estimations(i) = EvaluateSpline3(x, y, z, h, points(i));
    end

endfunction










// ==================================================================
// ===================           EXAM          ======================
// ==================================================================

// Points
x = [-1; 0; 2; 3; 5; 6];
y = [-1; 2; 0; 2; -1; 4];

// Get Spline data
[z, h] = Spline3(x, y);
points = linspace(-1, 6)';

// Easy evaluation
function estimation = f(point)
    [estimation] = EvaluateSpline3(x, y, z, h, point);
endfunction

// Find min 1
[a, b] = AurealSection(0.5, 3, f, 10e-8, 90);
min1 = a + (b - a) / 2;

// Find min 2
[a, b] = AurealSection(3.5, 6, f, 10e-8, 90);
min2 = a + (b - a) / 2;

// Find graph
[estimations] = EvaluateSpline3Points(x, y, z, h, points);
plot(points, estimations, "black-");
plot(x, y, "m*");

// Plot estimations
plot(min1, f(min1), "red.");
plot(min2, f(min2), "blue.");

hl=legend(['Estimated courve'; 'Spline data points'; 'min 1'; 'min 2']);
xtitle("Estimate a Function and the minimums", "x", "y");