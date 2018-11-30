clc

getd('/Users/mac/Documents/Projects/Reference/Code/NumericalAnalysis')

function [x] = f(x)
    x = sin(x) + 2*cos(x+2)^2
endfunction


a = 4.6
b = 5
c = 6
d = 7.3


plot(a, f(a), ".green")
plot(b, f(b), ".green")
plot(c, f(c), ".green")
plot(d, f(d), ".green")

x = linspace(4.4, 7.4)
y = f(x)

plot(x, y, "-red")

[a, b, c] = MinimazeWithCubic(a, b, c, f, 2, 0.01);

disp( (b + c) / 2)