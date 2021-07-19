/* 
En esta fase dos usamos el algoritmo de fuerza bruta para resolver todas las posibles matrices
*/
fase2(BM,SM, Size) :-
        fillMatriz(BM,SM,Size),
        regla1(SM),
        regla2(SM),
        trampose(SM,TSM),
        regla1(TSM),
        regla2(TSM),
        regla3(SM).