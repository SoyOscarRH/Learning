// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Laurrabaquio Rodríguez Miguel Salvador
// @Author: Pahua Castro Jesús Miguel Ángel

getd(pwd() + Directory);
clc;

tolerance = 10^-7
MaxIterations = 40

function [x] = f(input)
    I = input(1)
    phi = input(2)
    delta = input(3)
    x = [
        (I * cos(phi) - 2/3);
        (cos(delta) + 0.91 * I * sin(delta + phi) - 1.22);
        (0.76 * I * cos(delta + phi) - sin(delta));
    ]
endfunction

estimation = [
    1;
    0.1;
    0.1;
]

[estimation, iterations] = NewtonRaphsonGeneralized(estimation, f, tolerance, MaxIterations)
disp("estimation = " + string(estimation))
disp("f(estimation) = " + string(f(estimation)))

disp("Good starting point :)")

estimation = [
    1;
    1;
    1;
]

[estimation, iterations] = NewtonRaphsonGeneralized(estimation, f, tolerance, MaxIterations)
disp("estimation = " + string(estimation))
disp("f(estimation) = " + string(f(estimation)))

disp("bad starting point :(")