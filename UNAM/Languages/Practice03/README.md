| Nombre                         | No. De Cuenta   | Email
|---|---|---|
| Ernesto Muñoz Nieves            | 314133586        | ernestn@ciencias.unam.mx
| Oscar Andres Rosas Hernandez    | 417024956        | SoyOscarRH@ciencias.unam.mx
| Sergio Ugalde Marcano           | 418003059        | sergio.ugalde.m@ciencias.unam.mx

Notas:
- Los métodos subst e interp asumen que parse convirtió la expresión en una equivalente sin with*
- En test se imprimen solo los errores
- El test: `(test (prueba'{with* {{x y} {y 1}} x}) 1)` se cambió por `(test/exn (prueba'{with* {{x y} {y 1}} x}) "interp: Variable libre: 'y")`, para reflejar el orden de izquierda a derecha en el with*. En ese caso, y no está en el alcance cuando se intenta asignar a x, de modo que resulta en un error.

