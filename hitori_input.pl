:- include('solver').

/* Escribe el resultado */
writeFullOutput(S, X, Y):- write(X), write('x'), write(Y), nl, writeOutput(S).

writeOutput([]).
writeOutput([E|R]):- writeLinea(E), writeOutput(R).

writeLinea([]):- nl.
writeLinea([E|R]):- write(' '), write(E), writeLinea(R).

/* Leemos el input */
readProblem(N,M,Problem):- readInt(N), readInt(M), M=N, length(Problem, M), readProblemLines(N,Problem).

readProblemLines(_,[]).
readProblemLines(N,[H|T]):- length(H,N), readLine(H), readProblemLines(N,T).

readLine([]).
readLine([E|R]):- readInt(E), readLine(R).

readInt(N):- get_code(M), handleCode(M,N).

handleCode(M,N):- is_number_code(M,N1), !, continueInt(N1,N).
handleCode(-1,_):- !, fail. /* EOF */
handleCode(_,N):- readInt(N).

continueInt(O,N):- get_code(M), is_number_code(M,M1), !, H is 10*O+M1, continueInt(H,N).
continueInt(N,N).

is_number_code(N, N1):- N>=48, N<58, N1 is N-48.

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
