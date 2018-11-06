///Isaí López Servín 
// 18-Octubre-2018
//Función que aproxima la solución al sistema rectangular Ax=b
//Donde A es deimensión mxn con m>n mediante diagonalización
//Parámetros de entrada 
//A: Una matriz no singular de mxn
//b:Vector Solución del sistema Ax=b;
function[x]=Diagonali(A,b)
    n=size(A,'c');
    //Definimos las nuevesas dimensiones del sistema (nxn)
    A=A(1:n,1:n);
    b=b(1:n);
    //Obtenemos la aproximación del nuevo sistema sistema Ax=b
    [L,U]=FactorLU(A);
    y=Lx(L,b);
    x=Ux(U,y);
endfunction

//Función que calcula la factorización LU de A una matriz de nxn
//mediante matrices de eliminación
//Parámetros de entrada 
//A: Una matriz no singular de nxn
function [L,U] = FactorLU(A)
    [m,n]=size(A);//Obtenemos las dimensión de A
    invM=eye(m,n);//Matriz que guardará la inversa de M
    for k=(1:n-1)
        if(A(k,k)==0)
            A='Hay un 0 en la diagonal';
            invM='Hay un 0 en la diagonal';//Si algún elemento de la diagonal es 0 mandamos mensaje de error
        else
            for i=(k+1:n)
                invM(i,k)=A(i,k)/A(k,k);//Obtenemos la inversa de M
            end
            for j=(1:n)
                for i=(k+1:n)
                    A(i,j)=A(i,j)-invM(i,k)*A(k,j);//Reasignamos los valores de A
                end
            end
        end
    end
    //Asignamos los valores finales L y U
    L=invM; 
    U=A;              
endfunction
//Parametros de salida:
//x:Aproximación a la solución del sistema rectangular Ax=b;

//Parámetros de entrada
//L,U: Matrices de nxn que cumplen que LU=A
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
    
