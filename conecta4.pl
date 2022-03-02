tamCol(7).
tamFila(6).


%Para añadir una ficha al tablero
elemAtPos(1,[X|_],X).
elemAtPos(N,[_|L],R):- N1 is N-1, elemAtPos(N1,L,R).%Devuelve la cola seleccionada

colaLlena(L):- length(L,LEN), tamCol(LEN).

insertar(E,L,[E|L]).    % --> Cuando ya solo queda la lista L, añadimos el elemento a la lista donde habíamos pasado todo.
insertar(E,[X|Y],[X|Z]):- insertar(E,Y,Z). % --> llama cabeza cola cabeza cola hasta que tenemos toda la lista pasada a la lista de salida, añadimos ese elemento

borrar(X,[X|Y],Y).   %--> devuelve la lista sin X que es la cola.
borrar(X,[Z|L],[Z|M]):-borrar(X,L,M).  % --> va pasando los elementos de la lista L a M y cuando ve que está el elemento que queremos borrar en la cabeza, lo salta.

borrarCol(1, [_|Y], Y).
borrarCol(N, [Z|L], [Z|M]):- N1 is N-1, borrarCol(N1, L, M).

insertarAtPos(1, E, L, [E|L]).
insertarAtPos(N, E, [X|L], [X|T]):- N1 is N-1, insertarAtPos(N1, E, L, T).






