tamCol(7).
tamFila(6).

blanco(1).
negro(0).

borrar(X,[X|Y],Y).   %--> devuelve la lista sin X que es la cola.
borrar(X,[Z|L],[Z|M]):-borrar(X,L,M).  % --> va pasando los elementos de la lista L a M y cuando ve que est� el elemento que queremos borrar en la cabeza, lo salta.

%Para a�adir una ficha al tablero
elemAtPos(1,[X|_],X).
elemAtPos(N,[_|L],R):- N1 is N-1, elemAtPos(N1,L,R).%Devuelve la cola seleccionada

colaLlena(L):- length(L,LEN), tamCol(LEN).

insertar(E,L,[E|L]).    % --> Cuando ya solo queda la lista L, a�adimos el elemento a la lista donde hab�amos pasado todo.
insertar(E,[X|Y],[X|Z]):- insertar(E,Y,Z). % --> llama cabeza cola cabeza cola hasta que tenemos toda la lista pasada a la lista de salida, a�adimos ese elemento

borrarCol(1, [_|Y], Y).
borrarCol(N, [Z|L], [Z|M]):- N1 is N-1, borrarCol(N1, L, M).

insertarAtPos(1, E, L, [E|L]).
insertarAtPos(N, E, [X|L], [X|T]):- N1 is N-1, insertarAtPos(N1, E, L, T).

%Generar tablero inicial
lista_repe(1,X,[X]).
lista_repe(N,X,[X|L]):- N1 is N-1, lista_repe(N1,X,L).

generar_tablero(L,ROW,COL):- lista_repe(COL, ' ', L1),
                             lista_repe(ROW, L1, L).
                             
%Imprimir tablero
escribir(X):- put(X).
escribir_lista([]).
escribir_lista([X|Y]):- write(X), escribir_lista(Y).
escribir_lista_con_barra([]).
escribir_lista_con_barra([X|Y]):- write(X), write('|'), escribir_lista_con_barra(Y).

escribir_tablero([]):- lista_repe(15,'-',L1), write(''),
                        escribir_lista(L1), nl.
escribir_tablero([X|L]):- lista_repe(15, '-' , L1), write(''),
                          escribir_lista(L1), nl,
                          write('|'), escribir_lista_con_barra(X), nl,
                          escribir_tablero(L).

escribir_indices(NMAX, NMAX):- write(' '), write(NMAX), nl.
escribir_indices(NMIN, NMAX):- write(' '), write(NMIN), N1 is NMIN+1, escribir_indices(N1, NMAX).


%JUEGO
imprimir_turno(0, J1, _):- write('Es el turno de: '), write(J1), nl.
imprimir_turno(1, _, J2):- write('Es el turno de: '), write(J2), nl.
imprimir_turno(TURNO, J1, J2):- MOD is mod(TURNO,2), imprimir_turno(MOD,J1,J2).

preguntar_jugadores(J1,J2):- write('Introduzca el nombre del jugador 1'), nl,
                             read(J1), nl,
                             write('Introduzca el nombre del jugador 2'), nl,
                             read(J2).
                             
%preguntar_jugada(COL_SEL, L, TURNO):- write('Introduzca la columna: '),
%                                      read(COL_SEL).
%                                      %introducir_ficha

jugar(L,ROW,COL,J1,J2):- preguntar_jugadores(J1,J2),
                   escribir_indices(1,COL),
                   generar_tablero(L,ROW,COL),
                   jugando(0, L, J1, J2).
                   
jugando(TURNO, L, J1,J2):- imprimir_turno(TURNO, J1, J2),
                           escribir_tablero(L),
                           %preguntar_jugada(_, L, TURNO),
                           read(_), %para que no pete de momento
                           TURNO_SIG is TURNO+1,
                           jugando(TURNO_SIG, L, J1, J2).

                           




