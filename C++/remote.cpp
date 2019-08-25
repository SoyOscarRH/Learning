#include <iostream>
#include <vector>
#include <string>

using namespace std;

int existe(int boton, int canal);

int main(){
    // 1. Se crea un vector vacio para los botones rotos.
    auto botones_rotos = vector<int> {};
    // 2. Variable para el numero de botones rotos.
    auto numero_botones_rotos = 0;
    // 3. Variable para un solo boton roto.
    auto boton_roto = 0;

    // 4. Se guarda el primer numero.
    cin >> numero_botones_rotos;

    // 5. Se almacenan los siguientes numeros.
    for (auto i=0; i<numero_botones_rotos; i++)
    {   // 6. Se guarda en una variable.
        cin >> boton_roto;
        // 7. Se agrega al final del vector.
        botones_rotos.push_back(boton_roto);
    }

    // 8. Se crea variable para el canal a buscar.
    auto canal = 0;
    // 9. Se guarda el siguiente numero como canal.
    cin >> canal;
    // 10. Se crea una variable contador.
    auto cont = 0;
    // 11. Se crea la variable para el numero menor mas cercano al canal.
    auto numero_abajo = 0;

    // 12. Se busca el numero menor mas cercano al canal.
    for(auto i=0; i<=canal; i++){
        for(auto boton : botones_rotos){
            if(existe(boton, i) == 0){
                cont++;
            }
        }

        if(cont == numero_botones_rotos){
            numero_abajo = i;
        }
        cont = 0;
    }
    // 13. Se crea la variable para el numero mayor mas cercano al canal.
    auto numero_arriba = 0;

    // 14. Se busca el numero mayor mas cercano al canal.
    for(auto i=999; i>=canal; i--){
        for(auto boton : botones_rotos){
            if(existe(boton, i) == 0){
                cont++;
            }
        }

        if(cont == numero_botones_rotos){
            numero_arriba = i;
        }
        cont = 0;
    }
    
    // 15. Se cuentan los numeros entre los encontrados y el canal.
    numero_abajo = canal - numero_abajo;
    numero_arriba = numero_arriba - canal;

    // 16. Se evalua cual es el mas cercano y se imprime.
    if(numero_abajo <= numero_arriba){
        cout << numero_abajo;
    } else {
        cout << numero_arriba;
    }
    
    return 0;
}


// Retorna 1 si el boton si existe en el canal.
// Retorna 0 si el boton no existe en el canal.
int existe(int boton, int canal){
    int i=100, a=canal, b=-1;
    if(a < 100){
        i=10;
    }
    while(i > 0){
        b = a/i;
        a = a%i;
        if(b == boton){
            return 1;
            break;
        }
        i = i/10;
    }

    return 0;
}// @luisr-dev