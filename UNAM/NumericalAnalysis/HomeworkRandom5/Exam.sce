clc

getd('/Users/mac/Documents/Projects/Reference/Code/NumericalAnalysis')

function [x] = f(x)
    x = x^3 - 6*x^2 - 15*x + 2
endfunction

a = 1
b = 2
c = 13.5


plot(a, f(a), ".green")
plot(b, f(b), ".green")
plot(c, f(c), ".green")

x = linspace(1, 14)
y = f(x)

plot(x, y)

[a, b, c] = MinimazeWithCuadratic(a, b, c, f, 3, 0.001);

disp(b)