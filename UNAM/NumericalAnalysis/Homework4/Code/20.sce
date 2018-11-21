// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Laurrabaquio Rodríguez Miguel Salvador
// @Author: Pahua Castro Jesús Miguel Ángel

getd(pwd() + Directory);
clc;

tolerance = 10^-7
MaxIterations = 40

function [x] = f(input)
    U = input(1)
    phi = input(2)
    x = [
        (U + 0.27 / U - 1.31 * cos(phi));
        0.405 / U - 1.31 * sin(phi);
    ]
endfunction

estimation = [
    1;
    1;
]

[estimation, iterations] = NewtonRaphsonGeneralized(estimation, f, tolerance, MaxIterations)
disp("estimation = " + string(estimation))
disp("f(estimation) = " + string(f(estimation)))

estimation = [
    1;
    0;
]

[estimation, iterations] = NewtonRaphsonGeneralized(estimation, f, tolerance, MaxIterations)
disp("estimation = " + string(estimation))
disp("f(estimation) = " + string(f(estimation)))

estimation = [
    1000;
    %pi;
]

[estimation, iterations] = NewtonRaphsonGeneralized(estimation, f, tolerance, MaxIterations)
disp("estimation = " + string(estimation))
disp("f(estimation) = " + string(f(estimation)))


