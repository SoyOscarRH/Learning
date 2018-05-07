int FuncionRecursiva (int num) {
    if (num == 0)
        return 1;
    else if (num < 3) {
        resultado = 0;
        for(i=0; i < (num * num); i++)
            resultado *= num;
        return resultado;
    }
    else
        return 
            ((FuncionRecursiva(num - 1) * FuncionRecursiva(num - 2)) / FuncionRecursiva(num - 3));
}