// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Laurrabaquio Rodríguez Miguel Salvador
// @Author: Pahua Castro Jesús Miguel Ángel

getd(pwd() + Directory);
clc;

tolerance = 10^-2
MaxIterations = 100

function [x] = g(x)
    x = %pi + 0.5 * sin(x / 2)
endfunction

estimation = FixedPoint(2, g, tolerance, MaxIterations)
disp("a) g(x) = pi + 0.5 * sin(x / 2)")
disp("estimation = " + string(estimation))
disp("g(estimation) = " + string(g(estimation)))

x = linspace(2, 2* 3.1416, 100)
plot(x, g(x), '-')
plot(estimation, g(estimation), 'r*')
plot(x, x, 'g-')

hl=legend(['function'; 'fixed point'; 'Identity']);
xtitle("Fixed Point", "x-axis", "y-axis")