// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Laurrabaquio Rodríguez Miguel Salvador
// @Author: Pahua Castro Jesús Miguel Ángel

getd(pwd() + Directory);
clc;

tolerance = 10^-7
MaxIterations = 40

function [x] = f(x)
    x = [
    (x(1) + x(2)*(x(2)*(5 - x(2)) - 2)  - 13);
    (x(1) + x(2)*(x(2)*(1 + x(2)) + 14) - 29);
    ]
endfunction

estimation = [
    15;
    -2;
]

[estimation, iterations] = NewtonRaphsonGeneralized(estimation, f, tolerance, MaxIterations)
disp("estimation = " + string(estimation))
disp("f(estimation) = " + string(f(estimation)))
