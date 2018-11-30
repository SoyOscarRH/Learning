clc

getd('/Users/mac/Documents/Projects/Reference/Code/NumericalAnalysis')

function [x] = f(x)
    x = x^3 - 6*x^2 - 15*x + 2
endfunction


a = 0
b = 1
c = 9


plot(a, f(a), ".green")
plot(b, f(b), ".green")
plot(c, f(c), ".green")

x = linspace(-0.5, 10)
y = f(x)

plot(x, y)

[a, b, c] = MinimazeWithCuadratic(a, b, c, f, 3, 0.001);

disp(b)