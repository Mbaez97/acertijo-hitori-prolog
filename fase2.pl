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

/* 
    Regla 1 
    usa la recursion para la busqueda de duplicados en la primera linea
    La tranpuesta de la matriz para checkear las columnas: trampose(BM, SM)
*/
regla1(M) :- 
        regla1Check(M).


regla1Check([]).
regla1Check([H|T]) :-
        checkLinea2(H),
        regla1Check(T).

checkLinea2(L) :- 
        checkLinea2(L,L).
checkLinea2(_,[]).
checkLinea2(L, [H|T]) :-
        isUnknown(H),!,
        checkLinea2(L,T).
checkLinea2(L, [H|T]) :-
        H = 0,
        checkLinea2(L,T).
checkLinea2(L, [H|T]) :-
        H \= 0,
        noDuplicados(L,H),
        checkLinea2(L,T).

noDuplicados(L,V) :-
        noDuplicados(L, V, 0).
noDuplicados([],_,1).
noDuplicados([],_,0).
noDuplicados([V|T], X, R) :- 
        isUnknown(V),
        noDuplicados(T,X,R).
noDuplicados([V|T], X, R) :-
        V = X,
        A is R + 1,
        A < 2,
        noDuplicados(T, X, A).
noDuplicados([V|T], X, R) :-
        V \= X,
        noDuplicados(T,X,R).


/*
    Regla 2
    Usamos la recursion para buscar los posibles celdas negras adjacentes.
    La matriz trampuesta para checkear: trampose(BM,SM)

*/

regla2(M) :-
        regla2Check(M).

regla2Check([H|T]) :-
        checkLinea(H),
        regla2Check(T).

checkLinea([_]) :- true.
checkLinea([I1,I2|T]) :-
        not(ParNegro(I1,I2)),
        checkLinea([I2|T]).

ParNegro(I1,I2) :-
        I1 == 0,
        I2 == 0.

/*
    Regla 3
    Usa un algoritmo de llenado para recorrer la matriz 
    y checkea si todas las celdas no blancas están conectadas

*/
regla3(M) :-
        prepareForFlood(M,PM),      % Prepara la lista para un efectivo checkeo con el algoritmo flood-fill
        primerNoNegro(PM,Start),    % Encuentra la primera celda no negra en la matriz
        flood(PM,Start,[],R),       % Hace el algoritmo de llenado
        flatten(M,FM),              % Funcion de Prolog que aplana la matriz original
        noNegros(FM, NoNegros)      % Filtra todos los negros (0) celdas
        same_length(R, NoNegros),!. % Si la celda visitada por el algoritmo tiene el mismo tamaño de la lista
                                    % de todos los no negros, todas las celdas soon visitadas.

/*
    Algoritmo Flood-fill(llenado)
    Las celdas son representadas como lista de tres elementos.
    Necesita que la matriz sea tridimensional.
*/
flood(_,[V,_,_],Visitado,Visitado) :- V=0,!.
flood(_,[V,X,Y],Visitado,Visitado) :- member([V,X,Y], Visitado),!.
flood(Matriz, [V,X,Y], Visitado,R0) :-
        V \= 0, append(Visitado,[[V,X,Y]], R1),
        Xleft is X - 1, elementAt(Matriz, Xleft,Y,Eleft),       % Encuentra el elemento de la izquierda
        Xright is X + 1, elementAt(Matriz, Xright,Y,Eright),    % Encuentra el elemento de la derecha
        Yup is Y - 1, elementAt(Matriz,X,Yup,Eup),              % Encuentra el elemento de arriba
        Ydown is Y + 1, elementAt(Matriz,X,Ydown,Edown)         % Encuentra el elemento de abajo
        flood(Matriz,Eleft,R1,R2),
        flood(Matriz,Eright,R2,R3),
        flood(Matriz,Edown,R3,R4),
        flood(Matriz,Eup,R4,R5),!,
        list_to_set(R5, R0),!.

/*
    Encuentra el primer elemento en la lista de los no blancos
    Por la regla 1 y regla 2 sabemos que una de las celdas de arriba a la izquierda debe ser blanca
*/

primerNoNegro([H|_],V) :- exclude(ib, H, [V|_]).
ib([V,_,_]) :- V==0.

% Filtra la lista. Retorna todas las celdas no negras.
noNegros(L,Resultado) :- exclude(isNegro, L, Resultado).
isNegro(X) :- X == 0.

% Utilizamos nth0 para traer el elemento dado la fila y la columna
elementAt(M,X,Y,E) :- nth0(Y, M, Row), nth0(X,Row,E).
elementAt(_,_,_,[0,0,0]).

% Prepara una matriz bidimensional para el algoritmo de llenado
prepareForFlood(A,B) :-
        prepareForFlood(A,B,0).
prepareForFlood([],[],_).
prepareForFlood([H|T], [A|B], Y) :- 
        prepareFila(H,A,0,Y),
        Y2 is Y+1,
        prepareForFlood(T,B,Y2).

prepareFila([],[],_,_).
prepareFila([V|T1], [[V,X,Y]|T2],X,Y) :-
        X2 is X+1,
        prepareFila(T1,T2,X2,Y).

/*
    Aplana la matriz en la lista
    Pintamos las celdas en la matriz solucion con su color original o negro
    Si pasa por un item resuelto pasa
*/
fillMatriz(BM, SM, Size) :-
        flatten(BM, FBM),
        flatten(SM, FSM),
        fillAlgoritmo(FBM,FSM,Size).
        
fillAlgoritmo(A,B,Size) :- fillAlgoritmo(A,B,[],[],Size).
fillAlgoritmo([],[],_,_,_).
fillAlgoritmo([H1|T1], [H2|T2], A, B, Size) :- 
        fill(A,B,[H1|T1], [H2|T2], Size),
        append(A,[H1],A2),
        append(B,[H2],B2),
        fillAlgoritmo(T1,T2,A2,B2,Size).
    
% Si la celda ya se recorrio y se determino su color, pasa.
fill(_,_,[0|_],[_|_],_) :- !.
fill(H1,H2,[E1|T1],[E2|T2],Size) :-
        E2=E1,                                              % Coloca el valor original en la celda
        append(H1,[E1|T1],A1), append(H2,[E2|T2],A2),       % Combina la cabeza y la cola de la lista
        lista2matriz(A1,Size,R1), lista2matriz(A2,Size,R2),% Transforma la lista en matriz otra vez
        doChainReactions(R1,R2).                            % Reaccion en cadena
fill(H1,H2,[E1|T1],[E2|T2],Size) :-
        E2=0,                                               % Coloca el valor original en la celda
        append(H1,[E1|T1],A1), append(H2,[E2|T2],A2),       % Combina la cabeza y la cola de la lista
        lista2matriz(A1,Size,R1), lista2mamtriz(A2,Size,R2),% Transforma la lista en matriz otra vez
        doChainReactions(R1,R2).                            % Reaccion en cadena
        
% Tamaño de la lista
length_(Length, List) :- length(List, Length),

% Convierte una lista dada en su forma original de matriz, en base al tamaño de las filas
lista2matriz(List, RowSize, Matriz) :-
        length(List,L),
        CuantasFilas is L div RowSize,
        length(Matriz ,CuantasFilas),
        maplist(length_(RowSize), Matriz),
        append(Matriz, List).        

