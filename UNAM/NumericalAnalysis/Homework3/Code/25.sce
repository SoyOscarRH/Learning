// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Laurrabaquio Rodríguez Miguel Salvador
// @Author: Pahua Castro Jesús Miguel Ángel

getd('/Users/mac/Documents/Projects/Learning/UNAM/NumericalAnalysis/Homework2/Code/Algorithms')
clc;


for k = (1 : 10)
    epsilon = 10 ** (-2 * k)

    A25 = [
        epsilon 1;
        1 1;
    ];
    
    b25 = [
        1 + epsilon;
        2;
    ];

    realX25 = [
        1;
        1;
    ]

    disp("A")
    disp(A25)

    disp("b")
    disp(b25)

    [x] = GaussianElimination(A25, b25);

    disp("Estimated Solution: A \tilde x")
    disp(A25 * x)

    disp("Real Solution: A x")
    disp(A25  * realX25)

    disp("Difference of Error: \tilde x - x")
    disp(x -  realX25)

    disp("Estimated Condition")
    disp( Condition(A25, 10) )

    disp("Real Condition")
    disp( cond(A25) )

end
