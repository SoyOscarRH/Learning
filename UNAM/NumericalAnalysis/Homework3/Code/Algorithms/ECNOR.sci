////////EJERCICIO 25

//Murillo Villegas Yucely A. 
//Tirado Perez Frida A. 

//Esta funcion recibe una m y una n para crear el problema de mínimos cuadrados mal condicionado y un epsilon para calcular la perturbacion
function[x,z,x2,z2]=ECNOR(m,n,e)
    //Necesitamos m variables definidas como ti=(i-1)/(m-1)
    for(i=1:m)
        t(i)=(i-1)/(m-1)
    end
    //Creamos un vector de mx1 "y" de ceros
    y=zeros(m,1)
    //Modificamos el vector de ceros para cada y(i)
    for(i=1:m)
        //Cada y(i) será la suma de t(i) elevado al grado j-1 desde j=1 hasta n
        for(j=1:n)
        y(i)=y(i)+t(i)^(j-1)
        end
    end
    //Creamos la matriz de mxn que seran las t(i)'s elevadas a cada una de las potenci    as
    A=zeros(m,n)
    for(i=1:m)
        for(j=1:n)
        A(i,j)=t(i)^(j-1)
        end
    end
    //Creamos el vector de x's que debería ser un vector de 1's y queremos ver si los metodos  aproximan bien la solucion de Ax=y, también queremos ver que tan sensible a perturbaciones es nuestro metodo por lo que generamos una ligera perturbación lo llamaremos vector g
   ///Generamos la perturbacion necesaria
   g=zeros(m,1)
    for(i=1:m)
        g(i)=y(i)+2*(grand(1,1,"unf",0,1)-1)*e
    end
    //Resolvemos el problema de mínimos cuadrados con ecuaciones normales con factorización choleski y queremos ver que tanto se acerca a el vector de 1's y que tan sensible es a la perturbacion
    //X es el vector solucion con la y original y z es el vector solucion con la perturbacion
    x=MCuadrados(A,y)
    z=MCuadrados(A,g)
    
    //Ahora vamos a solucionar el mismo sistema pero con un metodo de factorizacion QR
    //x2 sera la solucion a Ax2=y y z2 sera la solucion a Az2=g es decir la solución original contra la solucion de la perturbación para esto elegimos el metodo de HouseHolder
    x1=Ux(A,y)
    z1=Ux(A,g)
    for(i=1:n)
    x2(i)=x1(i)
    z2(i)=z1(i)
    end
//a)Para cual de los métodos la solución es más sensible a la perturbación generada?



//Ahora vamos a calcular las diferencias entre la solución bajo la perturbacion y bajo la y real para cada uno de los dos metodos 
//Es decir la norma de (x2-z2) y la norma de (x-z) para saber cual es más sensible ante perturbaciones

   norma1=norm(x-z)
   norma2=norm(x2-z2)
   disp("a)")
   if(norma1<norma2)
       
       disp("El método de mínimos cuadrados con ecuaciones normales es menos sensible a perturbaciones")
    else 
        disp("El método de factorizacion QR es menos sensible a perturbaciones")
    end
//b) ¿Cual de los metodos esta mas proximo a tener la solucion exacta dada por x=1
    disp("b) La solución bajo ecuaciones normales está mucho más cercana a la solución de xi=1")

//c) ¿La diferencia en las soluciones afecta en el ajuste de puntos ti,yi?¿Por que?
//Vamos a hacer la multiplicación de AX y la multiplicación de AX2 para saber si afecta el ajuste de (ti,yi)
    disp("c)")
    if(A*x==y)
    disp("La diferencia en las solucion por ecuaciones normales no afecta el ajuste de puntos por el polinomio")
    else
    disp("La diferencia en las solucion por ecuaciones normales si afecta el ajuste de puntos por el polinomio")
    end
    if(A*x2==y)
    disp("La diferencia en las solucion por factorización QR no afecta el ajuste de puntos por el polinomio")
    else
    disp("La diferencia en las solucion por ecuaciones normales no afecta el ajuste de puntos por el polinomio")
    end


    


    endfunction
    
   //La funcion devuelve la solución para el vector original y para el vector perturbado bajo los metodos de mínimos cuadrados y householder 
   //X,Z,X2,Z2
