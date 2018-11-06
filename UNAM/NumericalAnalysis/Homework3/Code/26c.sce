// @Author: Rosas H26ernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Laurrabaquio Rodríguez Miguel Salvador
// @Author: Pahua Castro Jesús Miguel Ángel

getd('/Users/mac/Documents/Projects/Learning/UNAM/NumericalAnalysis/Homework2/Code/Algorithms')
clc;


digitsAltered = eye(12, 1)
digitsClassic = eye(12, 1)
digitsTwo = eye(12, 1)
digitsNormal = eye(12, 1)
data = eye(12, 1)

for i = (2 : 12)
    H26 = eye(i, i)
    data(i) = i

    for i = (1 : i)
        for j = (1 : i)
            H26(i, j) = 1 / (i + j - 1)
        end
    end


    [Q26Classic, R26] = GramSchmidt(H26, 0)
    [Q26Altered, R26] = GramSchmidt(H26, 1)
    [Q26Two, R26] = GramSchmidt(Q26Classic, 0)
    [L26] = CholeskyGaussian(H26' * H26)
    Q26N = H26 * L26'

    digitsClassic(i) = -log10(norm(eye(i, i) - Q26Classic' * Q26Classic))
    digitsAltered(i) = -log10(norm(eye(i, i) - Q26Altered' * Q26Altered))
    digitsTwo(i)     = -log10(norm(eye(i, i) - Q26Two' * Q26Two))
    digitsNormal(i)  = -log10(norm(eye(i, i) - Q26N' * Q26N))

    disp("Q of GramSchmidt Classical:")
    disp(Q26Classic)

    disp("Q of GramSchmidt Altered:")
    disp(Q26Altered)

end

plot(data,digitsClassic, '.b-')
plot(data,digitsAltered, '.r-')
plot(data,digitsTwo, '.g-')
plot(data,digitsNormal, '.black-')


hl=legend(['Classical GramSchmidt'; 'Altered GramSchmidt'; 'Two times GramSchmidt'; 'Normal equation']);
xtitle("Presition Digits", "n size", "bits")
