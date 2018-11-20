// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Laurrabaquio Rodríguez Miguel Salvador
// @Author: Pahua Castro Jesús Miguel Ángel

getd(pwd() + Directory);
clc;

tolerance = 10^-5
MaxIterations = 100

function [x] = fa(x)
    x = x - 2^(-x)
endfunction

function [x] = fb(x)
    x = exp(x) - x^2 + 3*x - 2
endfunction

function [x] = fc(x)
    x = 2*x * cos(2*x) - (x+1)^2
endfunction

function [x] = fd(x)
    x = x * cos(x) - 2*x*x + 3*x - 1
endfunction


estimation_fa = Bisection(0, 1, fa, tolerance, MaxIterations)
disp("a) f(x) = x - 2^-x")
disp("estimation_fa = " + string(estimation_fa))
disp("fa(estimation_fa) = " + string(fa(estimation_fa)))

estimation_fb = Bisection(0, 1, fb, tolerance, MaxIterations)
disp("b) f(x) = exp(x) - x^2 + 3*x - 2")
disp("estimation_fb = " + string(estimation_fb))
disp("fb(estimation_fb) = " + string(fb(estimation_fb)))

estimation_fc = Bisection(-3, -2, fc, tolerance, MaxIterations)
disp("c) f(x) = 2*x * cos(2*x) - (x+1)^2")
disp("estimation_fc = " + string(estimation_fc))
disp("fc(estimation_fc) = " + string(fc(estimation_fc)))

estimation_fc = Bisection(-1, 0, fc, tolerance, MaxIterations)
disp("estimation_fc = " + string(estimation_fc))
disp("fc(estimation_fc) = " + string(fc(estimation_fc)))

estimation_fd = Bisection(0.2, 0.3, fd, tolerance, MaxIterations)
disp("estimation_fd = " + string(estimation_fd))
disp("fd(estimation_fd) = " + string(fd(estimation_fd)))

estimation_fd = Bisection(1.2, 1.3, fd, tolerance, MaxIterations)
disp("estimation_fd = " + string(estimation_fd))
disp("fd(estimation_fd) = " + string(fd(estimation_fd)))