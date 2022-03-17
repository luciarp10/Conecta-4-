
borrar(X,[X|Y],Y).   %--> devuelve la lista sin X que es la cola.
borrar(X,[Z|L],[Z|M]):-borrar(X,L,M).  % --> va pasando los elementos de la lista L a M y cuando ve que está el elemento que queremos borrar en la cabeza, lo salta.

%COMPROBAR TAMAÑOS

columnaLlena(L):- length(L,LEN), tamCol(LEN), imprimir_msg('La columna está llena, prueba otra'). %length(A,B) devuelve la longitud B de la lista A, si concide con el tamaño de columna, llena
imprimir_msg(M):- writeln(M).

tablero_lleno(TAB, 1, ROWS):- columnaAtPos(1, TAB, COLX),             % Comprobamos que las columnas del tablero tengan longitud de fila. Si es así en todas, está lleno.
                              length(COLX, ROWSX),
                              (ROWS =:= ROWSX).

tablero_lleno(TAB, COLS, ROWS):- columnaAtPos(COLS, TAB, COLX),
                                 length(COLX, ROWSX),
                                 (ROWS =:= ROWSX),
                                 COLS_DEC is COLS-1,
                                 tablero_lleno(TAB, COLS_DEC, ROWS).


%INSERTAR FICHA

columnaAtPos(1,[X|_],X).
columnaAtPos(N,[_|L],R):- N1 is N-1, columnaAtPos(N1,L,R).%Devuelve la columna seleccionada

insertar(E,L,[E|L]).    % --> Cuando ya solo queda la lista L, añadimos el elemento a la lista donde habíamos pasado todo.
insertar(E,[X|Y],[X|Z]):- insertar(E,Y,Z). % --> llama cabeza cola cabeza cola hasta que tenemos toda la lista pasada a la lista de salida, añadimos ese elemento

borrarCol(1, [_|Y], Y).
borrarCol(N, [Z|L], [Z|M]):- N1 is N-1, borrarCol(N1, L, M).

insertarAtPos(1, E, L, [E|L]).
insertarAtPos(N, E, [X|L], [X|T]):- N1 is N-1, insertarAtPos(N1, E, L, T).

elegir_simbolo_e_insertar(TURNO, COL_SELECT, COL_CON_FICHA):- FICHA is TURNO mod 2,
                                                              insertar(FICHA, COL_SELECT, COL_CON_FICHA).

insertar_ficha(TURNO, POS_COL, TAB, TABRES):- columnaAtPos(POS_COL, TAB, COL_SELECT),
                                          elegir_simbolo_e_insertar(TURNO, COL_SELECT, COL_CON_FICHA),
                                          borrarCol(POS_COL, TAB, TABNUEVO),
                                          insertarAtPos(POS_COL, COL_CON_FICHA, TABNUEVO, TABRES). %comprobar_victoria(parametros necesarios), desde alli se llama a jugando o se acaba, de momento aquí para probar


%Generar tablero inicial
lista_repe(1,X,[X]).
lista_repe(N,X,[X|L]):- N1 is N-1,
                        lista_repe(N1, X,L).

generar_tablero_inicial(L,COL):- lista_repe(COL,[],L).


%Imprimir tablero

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

%Modo 1: humano vs humano: Pregunta los nombres de los jugadores y les da estrategia = 0 (humana)
definir_jugadores(1,J1,J2,E1,E2):- write('Modo de jugador humano vs jugador humano'), nl,
                                 write('Introduce el nombre del jugador 1: '), nl,
                                 read(J1), nl,
                                 E1 is 0,
                                 write('Introduce el nombre del jugador 2: '), nl,
                                 read(J2),
                                 E2 is 0.

%Modo 2: humano vs PC: Pregunta el nombre del J1 (humano), le da estrategia E1 = 0 (humana) y luego pregunta por la estrategia E2 del J2 (PC)
definir_jugadores(2,J1,J2,E1,E2):- write('Introduce el nombre del jugador humano'), nl,
                                  read(J1), nl,
                                  E1 is 0,
                                  atom_string(J2,'PC Bueno'),
                                  write('Tu PC rival es '), write(J2), nl,
                                  definir_estrategia_PC(J2, E2).

%Modo 3: PC vs PC: Pregunta por las estrategias de ambos
definir_jugadores(3,J1,J2,E1,E2):- atom_string(J1,'PC Bueno'),
                                   atom_string(J2,'PC Buenaga'),
                                   write('Van a enfrentarse los PCs '), write(J1), write(' y '), write(J2), nl,
                                   definir_estrategia_PC(J1,E1),
                                   definir_estrategia_PC(J2,E2).

%Pregunta al usuario por la estrategia E de un PC
definir_estrategia_PC(PC,E):- write('Define la estrategia de '), write(PC), write(' (1 para estrategia simple y 2 para avanzada): '), read(E), nl.

%Presenta los modos y da a elegir, al elegir se definen los jugadores dependiendo del modo
seleccionar_modo_juego(J1,J2,E1,E2):- write('Quieres que la partida sea entre: '), nl,
                                 write('1: Jugadores humanos'), nl,
                                 write('2: Jugador humano y PC'), nl,
                                 write('3: PC y PC  '), nl,
                                 read(MODO_JUEGO),
                                 definir_jugadores(MODO_JUEGO,J1,J2,E1,E2).

%Pregunta la jugada al jugador humano y luego se simula la del pc
preguntar_jugada(TURNO, TAB, J1, J2, 0, 1, COL, ROW):- write('Introduce la columna: '),
                                               read(NUM_COL), nl,
                                               columnaAtPos(NUM_COL, TAB, COL_SELECT),
                                               not(columnaLlena(COL_SELECT)),
                                               insertar_ficha(TURNO, NUM_COL, TAB, TABRES),
                                               TURNO_SIG is TURNO+1,
                                               simular_jugada_simple(TURNO_SIG, TABRES, J1, J2, 0,1, COL, ROW).

preguntar_jugada(TURNO, TAB, J1, J2, 0, 1, COL, ROW):-
                                               preguntar_jugada(TURNO, TAB, J1, J2, 0, 1, COL, ROW).
                                               
                                               


%Se pregunta la jugada de un jugador, y la columna no esta llena, por lo que inserta la ficha y cambia el turno al otro
preguntar_jugada(TURNO, TAB, J1, J2, E1, E2, COL, ROW):- write('Introduce la columna: '),
                                               read(NUM_COL), nl,
                                               columnaAtPos(NUM_COL, TAB, COL_SELECT),
                                               not(columnaLlena(COL_SELECT)),
                                               insertar_ficha(TURNO, NUM_COL, TAB, TABRES),
                                               TURNO_SIG is TURNO+1,
                                               turno(TURNO_SIG, TABRES, J1, J2, E1, E2, COL, ROW).

%Predicado de jugada con la columna llena, se vuelve a preguntar la jugada otra vez
preguntar_jugada(TURNO, TAB, J1, J2, E1, E2, COL, ROW):-
                                               preguntar_jugada(TURNO, TAB, J1, J2, E1, E2, COL, ROW).


simular_jugada_simple(TURNO, TAB, J1, J2, 0,1, COL, ROW):-
                                               random(0, COL, COL_ALEATORIA),
                                               columnaAtPos(COL_ALEATORIA, TAB, COL_SELECT),
                                               not(columnaLlena(COL_SELECT)),
                                               insertar_ficha(TURNO, COL_ALEATORIA, TAB, TABRES),
                                               write('Turno simulado'),nl,
                                               imprimir_turno(TURNO, J1, J2), nl,
                                               escribir_indices(1,COL),
                                               escribir_tablero(TABRES), nl,
                                               TURNO_SIG is TURNO+1,
                                               turno(TURNO_SIG, TABRES, J1, J2, 0, 1, COL, ROW).

simular_jugada_simple(TURNO, TAB, J1, J2, 0,1, COL, ROW):-
                                               simular_jugada_simple(TURNO, TAB, J1, J2, 0, 1, COL, ROW).


%Simboliza el turno cuando se ha seleccionado la estrategia (humano vs pc), se imprime el tablero, se pregunta jugada y se simula la jugada del pc
turno(TURNO, TAB, J1, J2, 0, 1, COL, ROW):-  not(tablero_lleno(TAB,COL,ROW)),
                                              NUMERO_JUGADA is TURNO+1,
                                              write('Jugada numero '), write(NUMERO_JUGADA), write('. '), nl,
                                              imprimir_turno(TURNO, J1, J2), nl,
                                              escribir_indices(1,COL),
                                              escribir_tablero(TAB), nl,
                                              preguntar_jugada(TURNO, TAB, J1, J2, 0,1, COL, ROW).

%Simboliza el turno de un jugador (humano o PC), se imprime el tablero, se pregunta jugada y se cambia el turno
turno(TURNO, TAB, J1, J2, E1, E2, COL, ROW):- not(tablero_lleno(TAB, COL, ROW)),
                                           NUMERO_JUGADA is TURNO + 1,
                                           write('Jugada numero '), write(NUMERO_JUGADA), write('. '), nl,
                                           imprimir_turno(TURNO, J1, J2), nl,
                                           escribir_indices(1,COL),
                                           escribir_tablero(TAB), nl,
                                           preguntar_jugada(TURNO, TAB, J1, J2, E1, E2, COL, ROW).

%Turno con el tablero lleno, fin del juego
turno(TURNO, TAB, _, _, _, _, COL, ROW):- tablero_lleno(TAB, COL, ROW),
                                           write('El tablero, se ha llenado, el juego ha acabado en EMPATE tras '), write(TURNO), write(' jugadas.'), nl,
                                           write('Otra partida? '), nl.


%Establece el juego (jugadores y tablero) y empieza el juego
jugar(ROW,COL):- seleccionar_modo_juego(J1,J2,E1,E2),
                 assertz(tamCol(ROW)), %El tamaño de una columna es el numero de filas
                 assertz(tamRow(COL)), %El tamaño de una fila es el numero de columnas
                 assertz(numRows(ROW)),
                 assertz(numCols(COL)),
                 assertz(nombre_j1(J1)),
                 assertz(nombre_j2(J2)),
                 assertz(estrategia_j1(E1)),
                 assertz(estrategia_j2(E2)),
                 generar_tablero_inicial(TAB, COL),
                 write('Comienza el juego entre '), write(J1), write(' y '), write(J2), nl,
                 turno(0, TAB, J1, J2, E1, E2, COL, ROW). %Turno inicial para el jugador 0
