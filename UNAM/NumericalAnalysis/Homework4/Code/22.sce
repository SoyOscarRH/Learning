// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Laurrabaquio Rodríguez Miguel Salvador
// @Author: Pahua Castro Jesús Miguel Ángel

getd(pwd() + Directory);
clc;

tolerance = 0.001;
MaxIterations = 40

function [x] = f(x)
    x1 = x(1)
    x2 = x(2)
    x3 = x(3)
    x = [
        (5 - x2**2 - x3**2) / x1;
        (1 - (1 -x2 ));
        (3 - x1);
    ]
endfunction

function [x] = fReal(x)
    x1 = x(1)
    x2 = x(2)
    x3 = x(3)
    x = [
        (x1**2 + x2**2 + x3**2 - 5);
        (x1 + x2 - 1);
        (x1 + x3 - 3);
    ]
endfunction


estimation = [
    2; 1212; 2.1
]

[estimation, iterations] = FixedPoint(estimation, f, tolerance, MaxIterations)
disp("estimation = " + string(estimation))
disp("f(estimation) = " + string(fReal(estimation)))
