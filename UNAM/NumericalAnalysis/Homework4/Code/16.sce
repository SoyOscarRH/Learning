// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Laurrabaquio Rodríguez Miguel Salvador
// @Author: Pahua Castro Jesús Miguel Ángel

getd(pwd() + Directory);
clc;

tolerance = 10^-6
MaxIterations = 100

function [x] = f(x)
    x = 230*x^4 + 18*x^3 + 9*x^2 - 221*x - 9
endfunction

disp("e) f(x) = 230*x^4 + 18*x^3 + 9*x^2 - 221*x - 9")

estimation = RegulaFalsi(-1, 0, f, tolerance, MaxIterations)
disp("estimation Regula Falsi = " + string(estimation))
disp("f(estimation) = " + string(f(estimation)))

estimation = RegulaFalsi(0, 1, f, tolerance, MaxIterations)
disp("estimation Regula Falsi = " + string(estimation))
disp("f(estimation) = " + string(f(estimation)))


estimation = Secant(-1, 0, f, tolerance, MaxIterations)
disp("estimation Secant = " + string(estimation))
disp("f(estimation) = " + string(f(estimation)))

estimation = Secant(0, 1, f, tolerance, MaxIterations)
disp("estimation Secant = " + string(estimation))
disp("f(estimation) = " + string(f(estimation)))

estimation = NewtonRaphson(-0.5, f, tolerance, MaxIterations)
disp("estimation NewtonRaphson = " + string(estimation))
disp("f(estimation) = " + string(f(estimation)))

estimation = NewtonRaphson(-0.5, f, tolerance, MaxIterations)
disp("estimation NewtonRaphson = " + string(estimation))
disp("f(estimation) = " + string(f(estimation)))
