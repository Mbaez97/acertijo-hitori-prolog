% Matriz base -> BM
% Matriz Solucion -> SM

/*
    En la fase 1 buscara patrones en la matriz

    si el tablero es valido, el tablero siempre será valido luego de la fase1 
*/

fase1(BM,SM,_) :- 
        patronSandwich(BM, SM),
        patronQuadCorner(BM,SM),
        patronTripleCorner(BM,SM),
        patronDobleCorner(BM, SM),
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

% Patron Sandwich.
patronSandwich(M, EM) :- 
        extractLinea1(M, EM), trampose(M,T), trampose(EM,TEM), extractLinea1(T,TEM).

extractLinea1([],[]).
extractLinea1([H|T],[A|B]) :-
        checkLinea1(H,A), extractLinea1(T,B).

checkLinea1([_,_],[_,_]).
checkLinea1([E1,E2,E1|T], [_,E2,A3|B]) :- 
        checkLinea1([E2,E1|T],[E2,A3|B]).
checkLinea1([E1,E1,E1|T], [0,E1,0|B]) :- 
        checkLinea1([_,_|T],[_,_|B]).
checkLinea1([E1,E2,E3|T], [_,A2,A3|B]) :- 
        (E1\=E3; E1\=E2; E1\=E3), checkLinea1([E2,E3|T], [A2|B]).

% Patron doble corner
patronDobleCorner(M,S) :-
        rotateMatrix(M,M1), rotateMatrix(S,S1), patronDC(M1,S1),
        rotateMatrix(M1,M2), rotateMatrix(S1,S2), patronDC(M2,S2),
        rotateMatrix(M2,M3), rotateMatrix(S2,S3), patronDC(M3,S3),
        rotateMatrix(M3,M4), rotateMatrix(S3,S4), patronDC(M4,S4).

patronDC([E1,E2|_], [A1,A2|_]) :-
        checkDC(E1,E2,A1,A2).

checkDC([E1,E1|_], [E3,_|_], [_,_|_], [E3,_|_]).
checkDC([E1,E2|_], [E1,_|_], [_,E2|_], [_,_|_]).
checkDC([_,E2|_], [E3,E3|_], [_,E2|_], [_,_|_]).
checkDC([_,E2|_], [E3,E2|_], [_,_|_], [E3,_|_]).
checkDC([_,_|_], [_,_|_], [_,_|_], [_,_|_]).

% Patron triple Corner
patronTripleCorner(M,S) :-
        rotateMatrix(M,M1), rotateMatrix(S,S1), patronTC(M1,S1),
        rotateMatrix(M1,M2), rotateMatrix(S1,S2), patronTC(M2,S2),
        rotateMatrix(M2,M3), rotateMatrix(S2,S3), patronTC(M3,S3),
        rotateMatrix(M3,M4), rotateMatrix(S3,S4), patronTC(M4,S4).

patronTC([E1,E2|T], [A1,A2|B]) :-
        checkTC(E1,E2,A1,A2).

checkTC([E1,E1|_], [E1,_|_], [0,E1|_], [E1,_|_]).
checkTC([E1,E2|_], [E2,E2|_], [_,E2|_], [E2,0|_]).
checkTC([E1,E2|_], [E3,E4|_], [A1,A2|_], [A3,A4|_]).

% Patron quad corner
patronQuadCorner(M,S):-
        rotateMatrix(M,M1), rotateMatrix(S,S1), patronQC(M1,S1),
        rotateMatrix(M1,M2), rotateMatrix(S1,S2), patronQC(M2,S2),
        rotateMatrix(M2,M3), rotateMatrix(S2,S3), patronQC(M3,S3),
        rotateMatrix(M3,M4), rotateMatrix(S3,S4), patronQC(M4,S4).

patronQC([E1,E2|T], [A1,A2|B]) :-
        checkQC(E1,E2,A1,A2).

checkQC([E1,E1|_], [E1,E1|_], [0,E1|_], [E1,0|_]).
checkQC([E1,E1|_], [E2,E2|_], [0,E1|_], [E2,0|_]).
checkQC([E1,E2|_], [E1,E2|_], [0,E2|_], [E1,0|_]).
checkQC([E1,E2|_], [E3,E4|_], [A1,A2|_], [A3,A4|_]).

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
                