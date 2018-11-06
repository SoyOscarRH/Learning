// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Laurrabaquio Rodríguez Miguel Salvador
// @Author: Pahua Castro Jesús Miguel Ángel

// Creates a very bad conditioned least squares problem
// @param: m a number, the size of the problem
// @param: n a number, the size of the problem
// @param: epsilon a number, the pertubation size

// @return: Q a matriz in M_{m x25 m} that is ortogonal
// @return: R a matriz in M_{m x25 n} that is triangular superior


function [] = 25(m, n, epsilon)

    // Create the polynomial
    t = zeros(m, 1)
    for(i = 1 : m)
        t(i) = (i-1) / (m-1)
    end

    //Some valuation result 
    y25 = zeros(m, 1)
    for(i = 1 : m)
        for(j = 1 : n)
            y25(i) = y25(i) + t(i)^(j-1)
        end
    end

    //Create the least square matrix
    A25 = zeros(m, n)
    for(i = 1 : m)
        for(j = 1 : n)
            A25(i, j) = t(i)^(j - 1)
        end
    end

    //Lets create the perturbations
    y2_25 = zeros(m, 1)
    for(i = 1 : m)
        y2_25(i) = y25(i) + (2*rand() - 1) * epsilon
    end

    //Lets solve
    x25 = LeastSquares(A25, y25)
    z = LeastSquares(A25, y2_25)

    [Q1, R1] = GramSchmidt(A25, 0)
    x2_25 = QRDecomposition(Q1, R1, y25)

    [Q1, R1] = GramSchmidt(A25, 0)
    x2_25 = QRDecomposition(Q1, R1, y2_25)

   disp("a)")
   if(norm(x25-z) < norm(x2_25-z2))
       disp("Least square is the least sensitive to pertubations")
    else 
       disp("QR Decomposition is the least sensitive to pertubations")
    end
    
    disp("c)")
    if(A25*x25 == y25)
        disp("The difference in the solution to least squares do not affect the ajust in the polynomial")
    else
        disp("The difference in the solution to least squares do affect the ajust in the polynomial")
    end
    
    if(A25*x2_25 == y25)
        disp("The difference in the solution to QR do not affect the ajust in the polynomial")
    else
        disp("The difference in the solution to QR do affect the ajust in the polynomial")
    end

endfunction
