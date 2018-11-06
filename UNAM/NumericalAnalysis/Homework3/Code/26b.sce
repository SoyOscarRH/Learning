// @Author: Rosas Hernandez Oscar Andres
// @Author: Alarcón Alvarez Aylin Yadira Guadalupe
// @Author: Laurrabaquio Rodríguez Miguel Salvador
// @Author: Pahua Castro Jesús Miguel Ángel

getd('/Users/mac/Documents/Projects/Learning/UNAM/NumericalAnalysis/Homework2/Code/Algorithms')
clc;


digitsAltered = eye(12, 1)
digitsClassic = eye(12, 1)
digitsTwo = eye(12, 1)
digitsHouse = eye(12, 1)
data = eye(12, 1)

for i = (2 : 12)
    H = eye(i, i)
    data(i) = i

    for i = (1 : i)
        for j = (1 : i)
            H(i, j) = 1 / (i + j - 1)
        end
    end


    [Q26Classic, R26] = GramSchmidt(H, 0)
    [Q26Altered, R26] = GramSchmidt(H, 1)
    [Q26Two, R26] = GramSchmidt(Q26Classic, 0)
    [Q26H, R26] = HouseHolder(H)

    digitsClassic(i) = -log10(norm(eye(i, i) - Q26Classic' * Q26Classic))
    digitsAltered(i) = -log10(norm(eye(i, i) - Q26Altered' * Q26Altered))
    digitsTwo(i)     = -log10(norm(eye(i, i) - Q26Two' * Q26Two))
    digitsHouse(i)   = -log10(norm(eye(i, i) - Q26H' * Q26H))

    disp("Q of GramSchmidt Classical:")
    disp(Q26Classic)

    disp("Digits:")
    disp(digitsClassic)
    
    disp("Q of GramSchmidt Altered:")
    disp(Q26Altered)

    disp("Digits:")
    disp(digitsAltered)
end

plot(data,digitsClassic, '.b-')
plot(data,digitsAltered, '.r-')
plot(data,digitsTwo, '.g-')
plot(data,digitsHouse, '.black-')

hl=legend(['Classical GramSchmidt'; 'Altered GramSchmidt'; 'Two times GramSchmidt'; 'HouseHolder']);
xtitle("Presition Digits", "n size", "bits")
