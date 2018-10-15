getd('/Users/mac/Documents/Projects/Reference/Code/NumericalAnalysis')

clc;
disp("Ax = b")

A26 = fscanfMat("/Users/mac/Documents/Projects/Learning/UNAM/NumericalAnalysis/Homewok2/Code/26.matrix");
b26 = zeros(100, 1);
b26(:,1) = 1;

disp("A:")
disp(A26)

disp("b:")
disp(b26)

disp("Solving...")

[L26, U26] = LUDecomposition(A26);

disp("L:");
disp(L26)

disp("U:");
disp(U26)

disp("Solving Ly = b")
y26 = FowardSubstitution(L26, b26);

disp("y:")
disp(y26)

disp("Solving Ux = y")
x26 = BackwardSubstitution(U26, y26);

disp("x:")
disp(x26)

disp("Getting the solution: Ax")
disp(A26 * x26)

disp("Expected solution (b)")
disp(b26)

disp("Cheking the error (Ax - b)")
disp(A26 * x26 - b26)

disp("|A - L*U|:")
disp(Norm1(A26 - (L*U)))