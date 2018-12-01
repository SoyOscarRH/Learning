clc


///Isaí López Servín 
// 18-Octubre-2018
//Función que resuelve el sistema Ax=b con
//A de dimensión mxn con m>n
//Input 
//A,b del sistema Ax=v
function[x]=MCuadrados(A,b)
    //Plantemos el sistema de mínimos cuadrados 
    b=A'*b;
    A=A'*A;
    n=size(A,'c');
    //Resolvemos mediante factorizaición de Cholesky 
    for j=(1:n)
        for k=(1:j-1)
            for i=(j:n)
                A(i,j)=A(i,j)-A(i,k)*A(j,k);
            end
        end
        A(j,j)=sqrt(A(j,j));
        for k=(j+1:n)
            A(k,j)=A(k,j)/A(j,j);
        end
    end
    //Resilvemos lel sistema 
    y=Lx(A,b);
    x=Ux(A',y)
endfunction
//Output
//x: Aproximación a la solución del sistema Ax=b

   //Función que resuelve un sistema diagonal inferior
//Parametros de entrada:
//L:Matríz diagonal inferior del sistema Lx=b
//b:Vector del sistema Lx=b
function [x] = Lx(L,b)  
    [m,n]=size(L);//Obtenemos las dimensiones de L
    x=zeros(m,1);//Vector que guardará la soluciones del sistema Lx=b
    for j=(1:n)
        if(L(j,j)==0)
            x='Error, hay un cero en la diagonal';//Si hay un cero en la diagonal, se manda mensaje de error
            break;
        else
            x(j)=b(j)/L(j,j);   //Obtenemos el vector de soluciones x
            for i=(j+1:n)
                b(i)=b(i)-L(i,j)*x(j);//Reasignamos los valores de b
            end
        end
    end
endfunction
//Parametros de salida:
//x:Solución del sistema Lx=b

//Función que resuelve un sistema diagonal inferior
//Parametros de entrada:
//U:Matríz diagonal superior del sistema Ux=b
//b:Vector del sistema Ux=b
function [x] = Ux(U,b)  
    [m,n]=size(U);//Obtenemos las dimensiones de L
    x=zeros(m,1);//Vector que guardará la soluciones del sistema Lx=b
    for j=(n:-1:1)
        if(U(j,j)==0)
            x='Error, hay un cero en la diagonal';;//Si hay un cero en la diagonal, se manda mensaje de error
            break;
        else
            x(j)=b(j)/U(j,j);   //Obtenemos el vector de soluciones x
            for i=(1:j-1)
                b(i)=b(i)-U(i,j)*x(j);//Reasignamos los valores de b
            end
        end
    end
endfunction
//Parametros de salida:
//x:Solución del sistema Ux=b


function [a, b, c, d] = InterpolacionCubica(a, b, c, f, MaxIterations, tolerance)
    intentos = 0

    while (intentos < MaxIterations)
        disp("a = " + string(a))
        disp("b = " + string(b))
        disp("c = " + string(c))
        disp("d = " + string(d))
        disp("")

        plot(a, f(a), ".m")
        plot(b, f(b), ".m")
        plot(c, f(c), ".m")
        plot(d, f(d), ".m")

        x = linspace(4, 7)
        [puntos, min] = intentocubica(a, b, c, d, f, x)

        if (intentos == 1)
            plot(x, puntos, "-black")
        elseif (intentos == 2)
            plot(x, puntos, "-m")
        else
            plot(x, puntos, "-g")
        end
        
        plot(min, f(min), "*black")
        disp("min = " + string(min))

        interacionAnterior = abs(b - c);


        if (a < min && min < b) 
            d = c
            c = b
            b = min
            a = a
        elseif (b < min && min < c) 
            b = min
        else
            a = b
            b = c
            c = min
            d = d
        end

        if (abs(abs(b-c) - interacionAnterior) < tolerance) then break end

        intentos = intentos + 1;
    end
endfunction


function [puntos, min] = intentocubica(a, b, c, d, f, puntos)

    A = [
        a^3 a^2 a^1 1;
        b^3 b^2 b^1 1;
        c^3 c^2 c^1 1;
        d^3 d^2 d^1 1;
    ]


    vectorSolucion = [
        f(a);
        f(b);
        f(c);
        f(d);
    ]

    [x] = MCuadrados(A, vectorSolucion);

    A = x(1)
    B = x(2)
    C = x(3)
    D = x(4)

    disp(x)

    puntos = A * (puntos)^3 + B * (puntos)^2 + C * puntos + D

    puntoInfl1 = (-2*B + sqrt(4*B^2 - 12*A*C))/(6*A)
    puntoInfl2 = (-2*B - sqrt(4*B^2 - 12*A*C))/(6*A)

    if (puntoInfl1 < a || puntoInfl1 > d) min = puntoInfl2
    else min = puntoInfl1 end

endfunction



function [x] = f(x)
    x = sin(x) + 2*cos(x+2)^2
endfunction


a = 4.6
b = 5
c = 6
d = 7.3


plot(a, f(a), ".m")
plot(b, f(b), ".m")
plot(c, f(c), ".m")
plot(d, f(d), ".m")

x = linspace(4.4, 7.4)
y = f(x)

plot(x, y, "-red")

[a, b, c] = InterpolacionCubica(a, b, c, f, 4, 0.01);

puntofinal = ( (b + c) / 2)

plot(puntofinal, f(puntofinal), ".y")