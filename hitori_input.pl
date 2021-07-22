:- include('solver').

% Runner
run :- pistaHitori(X), see(X), tell(F), solveProblems(5), told, seen,!.
run :- told, seen.

solveProblems(0).
solveProblems(N):- 
        N > 0, 
        solveMatriz(X,Y,I,S), !,
        N1 is N-1,
        solveProblems(N1).
solveProblems(_):- write('no solutions').
