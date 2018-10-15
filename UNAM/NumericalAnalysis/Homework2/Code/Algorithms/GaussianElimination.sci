// Solve Ax = b using Gaussian Elimination
// @param: A a not singular matrix
// @return: x such Ax = b

// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Laurrabaquio Rodríguez Miguel Salvador
// @Author: Pahua Castro Jesús Miguel Ángel

function [x] = GaussianElimination(A, b)
    [m, n] = size(A);
    U = A, B = b, x = zeros(n, 1)

    for step = (1 : n - 1)
        if (A(step, step) == 0) then
            error('Error: Singular matrix'); 
        end

        pivot = U(step, step);
        B(step) = B(step) / pivot
        for column = (1 : n)
            U(step, column) = U(step, column) / pivot;
        end

        for row = (step + 1 : n)
            rowPivot = U(row, step)
            B(row) = B(row) - rowPivot * B(step)
            for column = (1 : n)
                U(row, column) = U(row, column) - rowPivot * U(step, column);
            end
        end
    end

    for i = (n : -1 : 1)
        x(i) = B(i) / U(i,i);

        for j = (1 : i - 1)
            B(j) = B(j) - U(j, i) * x(i);
        end
    end

endfunction