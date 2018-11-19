// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Laurrabaquio Rodríguez Miguel Salvador
// @Author: Pahua Castro Jesús Miguel Ángel

getd(pwd() + Directory);
clc;

disp("==========================");
disp("====     ROOTS        ====");
disp("==========================");

disp("Select a method:");
disp("1) Bisection");
disp("2) Secant");
disp("3) Newton Rapshon");
disp("4) Regula Falsi");

number = input("Method?: ");
someFunction = input("Function as string f(x) = : ");
initialPoints = input("Initial point(s) (as vector): ");
tolerance = input("Tolerance: ");
MaxIterations = input("Max Iterations: ");

a = initialPoints(1)
try
    b = initialPoints(2)
catch
    b = 0
end

select number
    case 1 then
        deff('y = f(x)', ['y = evstr(someFunction)']);
        if (f(a) * f(b) >= 0) then
            disp("No valid point :(");
            break;
        end
        [estimation, iterations] = Bisection(a, b, someFunction, tolerance, MaxIterations)

end

disp("estimation: " + string(estimation))
disp("f(estimation): " + string(f(estimation)))
disp("Iterations required: " + string(iterations))