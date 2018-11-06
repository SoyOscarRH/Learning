// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Laurrabaquio Rodríguez Miguel Salvador
// @Author: Pahua Castro Jesús Miguel Ángel

getd('/Users/mac/Documents/Projects/Learning/UNAM/NumericalAnalysis/Homework3/Code/Algorithms')
clc;


values = [
    2;
    1.1;
    1;
    0.9;
    2e-01;
    2e-02;
    2e-05;
    2e-08;
    2e-10;
    2e-11;
]

[numberOfValues, _] = size(values)

for i = (1 : numberOfValues)

    e = values(i)
    disp("============")
    disp("epsilon:")
    disp(e)

    A27 = [
        1 1 1;
        e 0 0;
        0 e 0;
        0 0 e;
    ]

    b27 = [
        1;
        0;
        0;
        0;
    ]

    disp("Ax = b")

    disp("A:")
    disp(A27)

    try
        x27LeastSquares = LeastSquares(A27, b27)
        
        [Q27HH, R27HH] = HouseHolder(A27)
        x27HouseHolder = QRDecomposition(Q27HH, R27HH, b27)

        [Q27GS, R27GS] = GramSchmidt(A27, 0)
        x27GramSchmidt = QRDecomposition(Q27GS, R27GS, b27)

        [Q27G, R27G] = Givens(A27)
        x27Givens = QRDecomposition(Q27G, R27G, b27)


        disp("x of Least Squares:")
        disp(x27LeastSquares)

        disp("Ax of Least Squares:")
        disp(A27 * x27LeastSquares)

        disp("x of HouseHolder:")
        disp(x27HouseHolder)

        disp("Ax of HouseHolder:")
        disp(A27 * x27HouseHolder)

        disp("x of GramSchmidt:")
        disp(x27GramSchmidt)

        disp("Ax of GramSchmidt:")
        disp(A27 * x27GramSchmidt)

        disp("x of Givens:")
        disp(x27Givens)

        disp("Ax of Givens:")
        disp(A27 * x27Givens)
    catch
        disp("Error: Singular matrix")
    end


    

end