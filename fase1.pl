% Matriz base -> BM
% Matriz Solucion -> SM

/*
    En la fase 1 buscara patrones en la matriz

    si el tablero es valido, el tablero siempre será valido luego de la fase1 
*/

fase1(BM,SM,_) :- 
        doChainReactions(BM,SM),
        patronUnico(BM, SM).

% patronUnico busca las celdas que son unicas en su fila y columna
% Si la celda es unica pinta en blanco
patronUnico(BM,SM) :- patronUnico(BM,SM,BM,SM,0).
patronUnico(_,_,[],[],_). %Final del final de la lista
patronUnico(BM,SM,[H1|T],[A1|B],Y) :- 
        uniqueness(BM,SM,H1,A1,0,Y), 
        Y2 is Y+1,
        patronUnico(BM,SM,T,B,Y2).

% Termina al final de la lista
uniqueness(_,_,[],[],_,_).
uniqueness(BM,SM, [H|T], [A|B], X, Y) :- 
        isUnknown(A), % Sólo haga lo siguiente si la celda no está limitada
        row(BM,Y,BMRow), row(SM,Y,SMRow), countDuplicadosNoResueltos(BMRow, SMRow,H,CRow), CRow=1,
        col(BM,X,BMCol), col(SM,X,SMCol), countDuplicadosNoResueltos(BMCol, SMCol,H,CCol), CCol=1,!,A=H, 
        % Si no hay duplicados, A debe ser H
        X2 is X+1, uniqueness(BM,SM,T,B,X2,Y). %Recursion
uniqueness(BM,SM, [_|T], [_|B], X, Y) :- 
        X2 is X+1, uniqueness(BM,SM,T,B,X2,Y).


% Contadores
count([],_,0).
count([X|T],X,Y) :-
        count(X,Y,Z),
        Y is 1+Z.
count([X1|T],X,Z) :-
        X1\=X,
        count(T,X,Z).

countNoResueltoEnLaMatriz(M,Y) :-
        flatten(M, Flat), 
        countNoResuelto(Flat,Y).

countNoResuelto([],0).
countNoResuelto([X|T], Y) :-
        isUnknown(X),!,
        countNoResuelto(T,Z),
        Y is 1+Z.
countNoResuelto([X|T], Y) :-
        countNoResuelto(T,Y).

countDuplicadosNoResueltos([],[],_,0).
countDuplicadosNoResueltos([X|T], [A|B], V, Y) :-
        isUnknown(A),
        X=V,!,
        countDuplicadosNoResueltos(T,B,V,Z),
        Y is 1+Z.
countDuplicadosNoResueltos([X|T],[A|B], V, Y) :-
        countDuplicadosNoResueltos(T, B, V, Y).
                