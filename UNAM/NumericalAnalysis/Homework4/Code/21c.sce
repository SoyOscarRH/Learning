// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Laurrabaquio Rodríguez Miguel Salvador
// @Author: Pahua Castro Jesús Miguel Ángel

getd(pwd() + Directory);
clc;

tolerance = 10^-5
MaxIterations = 100

function [x] = f(x)
    x1 = x(1)
    x2 = x(2)
    x3 = x(3)
    x4 = x(4)
    x = [
        (x1 + 10*x2);
        sqrt(5) * (x3 - x4);
        (x2 - x3)**2;
        sqrt(10) * (x1 - x4)**2;
    ]
endfunction

estimation = [1;2;1;1]

[estimation, iterations] = NewtonRaphsonGeneralized(estimation, f, tolerance, MaxIterations)
disp("estimation = " + string(estimation))
disp("f(estimation) = " + string(f(estimation)))

disp("Error, because the LU found a zero in a pivot :v")
