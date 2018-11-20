// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Laurrabaquio Rodríguez Miguel Salvador
// @Author: Pahua Castro Jesús Miguel Ángel

getd(pwd() + Directory);
clc;

tolerance = 10^-4
MaxIterations = 100

function [x] = f(x)
    x = x**2 - 3
endfunction

estimation = Bisection(0, 10, f, tolerance, MaxIterations)
disp("a) f(x) = x^2 - 3")
disp("estimation = " + string(estimation))
disp("f(estimation) = " + string(f(estimation)))
