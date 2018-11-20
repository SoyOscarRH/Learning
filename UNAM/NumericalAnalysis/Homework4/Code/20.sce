// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Laurrabaquio Rodríguez Miguel Salvador
// @Author: Pahua Castro Jesús Miguel Ángel

getd(pwd() + Directory);
clc;

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

