% Rotar una matriz dada a la derecha
rotarMatriz(M, Rotated) :-
    transpose(M, X),
    maplist(reverse, X, Rotated).

% Obtiene la fila del tablero B dado por un indice I
row(B,I,R) :- nth0(I, B, R).

% Obtiene la columna del tablero B dado por un indice I
col(B,I,C) :- 
        transpose(B, T), 
        row(T,I,C).

% Realiza el algoritmo de reacciones en cadena hasta que no haya cambios
doChainReactions(M,S) :- doChainReactions(M,S,99999,0).         % Empieza con un valor alto hasta el ultimo simbolo
doChainReactions(M, S, CAntes, CDespues) :- 
    CDespues<CAntes,
    countNoResueltoEnLaMatriz(S,C1),
    chainReactions(M,S),!,
    countNoResueltoEnLaMatriz(S,C2),
    prep(M,S,M2),
    doChainReactions(M2,S,C1,C2),!.

doChainReactions(M,S,CAntes,CDespues) :- CDespues>=CAntes.

/*
 * EL algoritmo de reaccion en cadena pinta las distintas celdas en base a las reglas ya escritas para el hitori
 * Matrix is rotated for each time the black pattern is checked
 */
chainReactions(M,S) :-
    rotarMatriz(M, M1), rotarMatriz(S,S1), 
    patronDeCicloEstandarNegro(M1,S1),!,
    rotarMatriz(M1, M2), rotarMatriz(S1,S2), 
    patronDeCicloEstandarNegro(M2,S2),!, 
    rotarMatriz(M2, M3), rotarMatriz(S2,S3), 
    patronDeCicloEstandarNegro(M3,S3),!, 
    rotarMatriz(M3, M4), rotarMatriz(S3,S4),
    patronDeCicloEstandarNegro(M4,S4),
    patronDeCicloEstandarBlanco(M4,S4),!,
    patronUnico(M4,S4).
    
% Sets all duplicates in a white cells row and column as black
patronDeCicloEstandarBlanco(BM,SM) :- patronDeCicloEstandarBlanco(BM,SM,BM,SM,0).
patronDeCicloEstandarBlanco(_,_,[],[],_).
patronDeCicloEstandarBlanco(BM,SM,[E1|T],[A1|B],Y) :- checkCicloEstandarBlanco(BM,SM,E1,A1,0,Y), Y2 is Y+1, patronDeCicloEstandarBlanco(BM,SM,T,B,Y2).

checkCicloEstandarBlanco(_,_,[],[],_,_).
checkCicloEstandarBlanco(BM,SM,[_|T],[A|B],X,Y) :- isUnknown(A), !, X2 is X+1, checkCicloEstandarBlanco(BM,SM,T,B,X2,Y).
checkCicloEstandarBlanco(BM,SM,[H|T],[A|B],X,Y) :- isSolved(H),!, X2 is X+1, checkCicloEstandarBlanco(BM,SM,T,B,X2,Y).
checkCicloEstandarBlanco(BM,SM,[_|T],[A|B],X,Y) :- A=0,!, X2 is X+1, checkCicloEstandarBlanco(BM,SM,T,B,X2,Y).
checkCicloEstandarBlanco(BM,SM,[H|T],[A|B],X,Y) :- A=H, 
    row(BM,Y,BMRow), row(SM,Y,SMRow), buscaCicloEstandarBlanco(BMRow,SMRow,[A,X]),
    col(BM,X,BMCol), col(SM,X,SMCol), buscaCicloEstandarBlanco(BMCol,SMCol,[A,Y]), 
    X2 is X+1, checkCicloEstandarBlanco(BM,SM,T,B,X2,Y).

buscaCicloEstandarBlanco(BL,SL,I) :- buscaCicloEstandarBlanco(BL,SL,I,0).
buscaCicloEstandarBlanco([],[],_,_).
buscaCicloEstandarBlanco([H|T],[A|B],[V,I],I2) :- isSolved(H),!, I3 is I2+1, buscaCicloEstandarBlanco(T,B,[V,I],I3).
buscaCicloEstandarBlanco([H|T],[A|B],[V,I],I2) :- H=V, I2\=I, A=0,!, I3 is I2+1, buscaCicloEstandarBlanco(T,B,[V,I],I3).
buscaCicloEstandarBlanco([H|T],[A|B],[V,I],I2) :- H=V, I2=I,!,I3 is I2+1, buscaCicloEstandarBlanco(T,B,[V,I],I3).
buscaCicloEstandarBlanco([H|T],[_|B],[V,I],I2) :- H\=V,!, I3 is I2+1, buscaCicloEstandarBlanco(T,B,[V,I],I3).

% Sets the left neighbour to a cell as white if the cell itself is painted black
patronDeCicloEstandarNegro([],[]).
patronDeCicloEstandarNegro([E1|T],[A1|B]) :- checkCicloEstandarNegro(E1,A1), patronDeCicloEstandarNegro(T,B).

checkCicloEstandarNegro([_],[_]).
checkCicloEstandarNegro([E1,E2|T1], [A1,A2|B1]) :- ( isSolved(E1) ; isSolved(E2) ; isUnknown(A1) ), !, checkCicloEstandarNegro([E2|T1], [A2|B1]).
checkCicloEstandarNegro([E1,E2|T1], [A1,A2|B1]) :- A1=0, A2=E2, !, checkCicloEstandarNegro([E2|T1], [A2|B1]).
checkCicloEstandarNegro([E1,E2|T1], [A1,A2|B1]) :- A1\=0, !, checkCicloEstandarNegro([E2|T1], [A2|B1]).

% Definicion de una celda resuelta
isSolved(V) :- V=0.

% Definicion de una celda que no se sabe
isUnknown(V) :- integer(V), !, false.
isUnknown(_).