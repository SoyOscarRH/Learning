// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Laurrabaquio Rodríguez Miguel Salvador
// @Author: Pahua Castro Jesús Miguel Ángel

getd('/Users/mac/Documents/Projects/Learning/UNAM/NumericalAnalysis/Homework2/Code/Algorithms')
clc;
for i = (1 : 4)
    A23 = grand(8, 8, "unf", data23(i, 1), data23(i, 2));

    disp("Random Matrix");
    disp(A23);

    disp("Real condition");
    disp(cond(A23));

    disp("Estimated condition");
    disp(Condition(A23, 50));
end