:- use_module(library(clpfd)).

% Incluimos las fases, como solemos hacerlo en lenguaje c
:- include('fase2').
:- include('fase1').
:- include('juego').

% Creamos una matriz en blanco con la misma dimension de la matriz dada
createMatrizEnBlanco([],[]).
createMatrizEnBlanco([H|T], [A|B]) :- createFilaEnBlanco(H,A), createMatrizEnBlanco(T,B).

createFilaEnBlanco([],[]).
createFilaEnBlanco([_|T1],[_|T2]) :- createFilaEnBlanco(T1,T2).

% Intercambia todos los valores originales de los elementos resueltos en la matriz base con 0.
prep(M1,M2,M3) :- prepareMatriz(M1,M2,M3),!.

prepareMatriz([],[],[]).
prepareMatriz([H|T], [A|B], [C|D]) :- prepareFila(H,A,C),!, prepareMatriz(T,B,D).

prepareFila([],[],[]).
prepareFila([_|T1],[B|T2],[0|T3]) :- integer(B), !, prepareFila(T1,T2,T3).
prepareFila([A|T1],[_|T2],[A|T3]) :- prepareFila(T1,T2,T3).

% Copia/Mapea el tablero en la nueva matriz
mapMatriz([],[]).
mapMatriz([H|T], [A|B]) :- mapFila(H,A), mapMatriz(T,B).

mapFila([],[]).
mapFila([H|T1],['X'|T2]) :- integer(H), H=0, mapFila(T1,T2).
mapFila([H|T1],[H|T2]) :- integer(H), mapFila(T1,T2).

% Testing
mapFila([_|T1],['U'|T2]) :- mapFila(T1,T2).

/*
 * Resuelve una matriz dada 
 * fase1 -> algoritmo de reaccion en cadena
 * fase2 -> Fuerzabruta con reaccion en cadena
 */ 
solveMatriz(Size, _, BaseMatrix, ResultMatrix) :-
    createMatrizEnBlanco(BaseMatrix,SolutionMatrix),     
    fase1(BaseMatrix, SolutionMatrix, Size),             
    prep(BaseMatrix, SolutionMatrix, PreparedMatrix),!,   
    fase2(PreparedMatrix, SolutionMatrix, Size),          
    mapMatriz(SolutionMatrix, ResultMatrix). 