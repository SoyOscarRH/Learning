| Nombre                         | No. De Cuenta   | Email
|---|---|---|
| Ernesto Muñoz Nieves            | 314133586        | ernestn@ciencias.unam.mx
| Oscar Andres Rosas Hernandez    | 417024956        | SoyOscarRH@ciencias.unam.mx
| Sergio Ugalde Marcano           | 418003059        | sergio.ugalde.m@ciencias.unam.mx

Notas: 
Los operadores booleanos (así como las funciones condicionales) heredan algunos atributos de aquellas de Racket. Es decir, las primitivas distintas a `false`, incluyendo a los números, toman un valor de verdad verdadero `true`. Como efecto secundario de esto el `or` de una lista de elementos distintos de `false` devuelve el último elemento, y el `and` de una lista similar devuelve el primero. 

Esto también se refleja en el `if`, pues se aceptan primitivas en las condicionales, que a menos que sean `false`, evaluarán a verdadero.

En estas situaciones, consideramos como primitivas  a los numerales y booleanos.
