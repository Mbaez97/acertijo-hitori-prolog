:- include('solver').

% Runner
run():- 
        pistaHitori(X),
        write("Matriz Pista"),nl,
        write(X),nl, 
        solveProblems(5), told, seen,!,
        write("Matriz Solucion"), nl.
run():- told, seen.

solveProblems(0).
solveProblems(N):-
        N > 0,
        solveMatriz(X,Y,I,S), !,
        N1 is N-1,
        solveProblems(N1).
solveProblems(_):- write('no solutions').
