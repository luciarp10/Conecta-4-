tamCol(3).
tamFila(2).


borrar(X,[X|Y],Y).   %--> devuelve la lista sin X que es la cola.
borrar(X,[Z|L],[Z|M]):-borrar(X,L,M).  % --> va pasando los elementos de la lista L a M y cuando ve que est� el elemento que queremos borrar en la cabeza, lo salta.

%COMPROBAR TAMA�OS

columnaLlena(L):- length(L,LEN), tamFila(LEN), imprimir_msg('La columna est� llena').
imprimir_msg(M):- write(M), nl.

tablero_lleno(TAB, 1, ROWS):- elemAtPos(1, TAB, COLX),             % Comprobamos que las columnas del tablero tengan longitud de fila. Si es as� en todas, est� lleno.
                              length(COLX, ROWSX),
                              (ROWS =:= ROWSX),
                              imprimir_msg('EMPATE. El tablero est� lleno').

tablero_lleno(TAB, COLS, ROWS):- elemAtPos(COLS, TAB, COLX),
                                 length(COLX, ROWSX),
                                 (ROWS =:= ROWSX),
                                 COLS_DEC is COLS-1,
                                 tablero_lleno(TAB, COLS_DEC, ROWS).
                                 

%INSERTAR FICHA
elemAtPos(1,[X|_],X).
elemAtPos(N,[_|L],R):- N1 is N-1, elemAtPos(N1,L,R).%Devuelve la cola seleccionada

insertar(E,L,[E|L]).    % --> Cuando ya solo queda la lista L, a�adimos el elemento a la lista donde hab�amos pasado todo.
insertar(E,[X|Y],[X|Z]):- insertar(E,Y,Z). % --> llama cabeza cola cabeza cola hasta que tenemos toda la lista pasada a la lista de salida, a�adimos ese elemento

borrarCol(1, [_|Y], Y).
borrarCol(N, [Z|L], [Z|M]):- N1 is N-1, borrarCol(N1, L, M).

insertarAtPos(1, E, L, [E|L]).
insertarAtPos(N, E, [X|L], [X|T]):- N1 is N-1, insertarAtPos(N1, E, L, T).

elegir_simbolo_e_insertar(TURNO, COL_SELECT, COL_CON_FICHA):- FICHA is TURNO mod 2,
                                                              insertar(FICHA, COL_SELECT, COL_CON_FICHA).

insertar_ficha(TURNO, POS_COL, TAB, TABRES):- elemAtPos(POS_COL, TAB, COL_SELECT),
                                          elegir_simbolo_e_insertar(TURNO, COL_SELECT, COL_CON_FICHA),
                                          borrarCol(POS_COL, TAB, TABNUEVO),
                                          insertarAtPos(POS_COL, COL_CON_FICHA, TABNUEVO, TABRES). %comprobar_victoria(parametros necesarios), desde alli se llama a jugando o se acaba, de momento aqu� para probar


%Generar tablero inicial
lista_repe(1,X,[X]).
lista_repe(N,X,[X|L]):- N1 is N-1,
                        lista_repe(N1, X,L).
                        
generar_tablero_inicial(L,COL):- lista_repe(COL,[],L).

                             
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
imprimir_turno(TURNO, J1, J2):- MOD is TURNO mod 2, imprimir_turno(MOD,J1,J2).

preguntar_jugadores(J1,J2):- write('Introduzca el nombre del jugador 1'), nl,
                             read(J1), nl,
                             write('Introduzca el nombre del jugador 2'), nl,
                             read(J2).

preguntar_jugada(L, TURNO, J1, J2, COL, ROW):- write('Introduzca la columna: '),
                                     read(COL_SEL), nl,
                                     elemAtPos(COL_SEL, L, COL_SELECT),
                                     not(columnaLlena(COL_SELECT)),
                                     insertar_ficha(TURNO, COL_SEL, L, TABRES),
                                     TURNO_SIG is TURNO+1,
                                     jugando(TURNO_SIG, TABRES, J1, J2, COL, ROW).



jugando(TURNO, L, J1,J2, COL, ROW):- not(tablero_lleno(L, COL, ROW)),
                           escribir_indices(1,COL),
                           escribir_tablero(L), nl,
                           imprimir_turno(TURNO, J1, J2), nl,
                           preguntar_jugada(L, TURNO, J1,J2,COL, ROW).


jugar(L,ROW,COL,J1,J2):- preguntar_jugadores(J1,J2),
                   generar_tablero_inicial(L,COL),
                   jugando(0, L, J1, J2, COL, ROW).
                   

                   
                           



                           






