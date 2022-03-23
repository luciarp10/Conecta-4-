borrar(X,[X|Y],Y).   %--> devuelve la lista sin X que es la cola.
borrar(X,[Z|L],[Z|M]):-borrar(X,L,M).  % --> va pasando los elementos de la lista L a M y cuando ve que está el elemento que queremos borrar en la cabeza, lo salta.

%COMPROBAR TAMAÑOS

columnaLlena(L,1):-length(L,LEN), tamCol(LEN). %Columna llena que no imprime (para PC)
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

columnaAtPos(1,[X|_],X).  %La correcta, cuando ya se ha iterado COL_SEL veces, está en la cabeza
columnaAtPos(COL_SEL,[_|L],RES):- COL_SIG is COL_SEL-1, columnaAtPos(COL_SIG,L,RES).%La columna se alcanza al iterar COL_SEL veces el tablero

insertar_ficha(FICHA,L,[FICHA|L]).    % --> Cuando ya solo queda la lista L, añadimos el elemento a la lista donde habíamos pasado todo.
insertar_ficha(FICHA,[X|Y],[X|Z]):- insertar(FICHA,Y,Z). % --> llama cabeza cola cabeza cola hasta que tenemos toda la lista pasada a la lista de salida, añadimos ese elemento

insertarAtPos(1, E, L, [E|L]).
insertarAtPos(N, E, [X|L], [X|T]):- N1 is N-1, insertarAtPos(N1, E, L, T).


borrarCol(1, [_|Y], Y).
borrarCol(N, [Z|L], [Z|M]):- N1 is N-1, borrarCol(N1, L, M).

insertar_columna(1, COL_NUEVA, L, [COL_NUEVA|L]). %Inserta la columa en la posicion dada
insertar_columna(N, COL_NUEVA, [X|L], [X|T]):- N1 is N-1, insertar_columna(N1, COL_NUEVA, L, T). %Va avanzando hasta que consigue poner la columna y vuelve recursivamente, incluyendo las columnas saltadas

elegir_simbolo_e_insertar(TURNO, COL_SELECT, COL_CON_FICHA):- FICHA is TURNO mod 2,
                                                              length(COL_SELECT,LEN),
                                                              not(LEN==0),
                                                              POS is LEN+1,
                                                              insertarAtPos(POS, FICHA, COL_SELECT, COL_CON_FICHA).
elegir_simbolo_e_insertar(TURNO, COL_SELECT, COL_CON_FICHA):- FICHA is TURNO mod 2,
                                                              insertar_ficha(FICHA, COL_SELECT, COL_CON_FICHA).

insertar_ficha(TURNO, POS_COL, TAB, TABRES):- columnaAtPos(POS_COL, TAB, COL_SELECT),
                                          elegir_simbolo_e_insertar(TURNO, COL_SELECT, COL_CON_FICHA),
                                          borrarCol(POS_COL, TAB, TABNUEVO),
                                          insertar_columna(POS_COL, COL_CON_FICHA, TABNUEVO, TABRES). %comprobar_victoria(parametros necesarios), desde alli se llama a jugando o se acaba, de momento aquí para probar

%Elemento de la posición N de una lista
%Si no hay nada en esa posicion porque no existe devuelve un espacio
elem_at_pos(_,[],' ').
elem_at_pos(1,[X|_],X).
elem_at_pos(N,[_|L],R):-N1 is N-1,
                        elem_at_pos(N1,L,R).



%Generar tablero inicial
lista_repe(1,X,[X]).
lista_repe(N,X,[X|L]):- N1 is N-1,
                        lista_repe(N1, X,L).

generar_tablero_inicial(L,COL):- lista_repe(COL,[],L).

%Imprimir tablero
escribir_fila(TAB, ROW, COL, COLS_TAB):- not(COL > COLS_TAB),
                                         columnaAtPos(COL, TAB, COL_AT_POS),
                                         elem_at_pos(ROW, COL_AT_POS, ELEM),
                                         write('|'), write(ELEM),
                                         COL_SIG is COL+1,
                                         escribir_fila(TAB, ROW, COL_SIG, COLS_TAB).

%Cuando ya hemos mirado todas las posiciones de una fila
escribir_fila(_, _, _, _):- write('|'), nl.

%Imprimir tablero desde la fila mas alta hasta la mas baja
escribir_tablero(TAB, ROW, COL):- (ROW > 0),
                                  escribir_fila(TAB, ROW, 1, COL),
                                  ROW_SIG is ROW-1,
                                  escribir_tablero(TAB, ROW_SIG, COL).
escribir_tablero(_,_,_):- nl.


escribir_indices(NMAX, NMAX):- write(' '), write(NMAX), nl.
escribir_indices(NMIN, NMAX):- write(' '), write(NMIN), N1 is NMIN+1, escribir_indices(N1, NMAX).


%COMPROBACION VICTORIA
%Primero se comprueba que haya X iguales en la misma columna
comprobar_victoria(TAB, COL, ULTIMA_COL, CONECTA_X, TURNO, 0):-   columnaAtPos(ULTIMA_COL, TAB, COLUMNA_ACTUAL),
                                                                   length(COLUMNA_ACTUAL, ALTURA),
                                                                   elem_at_pos(ALTURA, COLUMNA_ACTUAL, FICHA),
                                                                   mirar_abajo(FICHA,COLUMNA_ACTUAL, ALTURA, CONECTA_X, FIN),  %No puede haber nada arriba
                                                                   write('Resultado ABAJO '), write(FIN), nl,
                                                                   partida_terminada_ab(FIN, TAB, COL, ULTIMA_COL, CONECTA_X, TURNO).
%Se comprueba si hay X iguales en la misma fila
comprobar_victoria(TAB, COL, ULTIMA_COL, CONECTA_X, TURNO, 1):-   columnaAtPos(ULTIMA_COL, TAB, COLUMNA_ACTUAL),
                                                                 length(COLUMNA_ACTUAL, ALTURA),
                                                                 elem_at_pos(ALTURA, COLUMNA_ACTUAL, FICHA),
                                                                 contar_izquierda(TAB, ULTIMA_COL, FICHA, ALTURA, 0, CONT_IZQ),
                                                                 contar_derecha(TAB, ULTIMA_COL, COL, FICHA, ALTURA,0,CONT_DER),
                                                                 SUM is CONT_IZQ+CONT_DER+1,   %+1 POR LA QUE ACABAS DE METER
                                                                 RES is CONECTA_X-SUM,
                                                                 write('Resultado LATERAL '), write(RES), nl,
                                                                 partida_terminada_lat(RES,TAB, COL, ULTIMA_COL, CONECTA_X, TURNO).
%Se comprueba si hay X iguales en las diagonales
comprobar_victoria(TAB, COL, ULTIMA_COL, CONECTA_X, TURNO, 2):-   columnaAtPos(ULTIMA_COL, TAB, COLUMNA_ACTUAL),
                                                                 length(COLUMNA_ACTUAL, ALTURA),
                                                                 elem_at_pos(ALTURA, COLUMNA_ACTUAL, FICHA),
                                                                 contar_diag_arriba_izq(TAB, ULTIMA_COL, FICHA, ALTURA, 0, CONT_ARI),
                                                                 contar_diag_arriba_der(TAB, ULTIMA_COL, COL, FICHA, ALTURA, 0, CONT_ARD),
                                                                 contar_diag_abajo_izq(TAB, ULTIMA_COL, FICHA, ALTURA, 0, CONT_ABI),
                                                                 contar_diag_abajo_der(TAB, ULTIMA_COL, COL, FICHA, ALTURA, 0, CONT_ABD),
                                                                 SUM_DIAG_1 is CONT_ARI+CONT_ABD+1,
                                                                 SUM_DIAG_2 is CONT_ARD+CONT_ABI+1,
                                                                 RES_DIAG1 is CONECTA_X-SUM_DIAG_1,
                                                                 RES_DIAG2 is CONECTA_X-SUM_DIAG_2,
                                                                 RES is min(RES_DIAG1, RES_DIAG2),
                                                                 write(RES), nl,
                                                                 partida_terminada_diag(RES,TAB, COL, ULTIMA_COL, CONECTA_X, TURNO).

%PARA ESTRATEGIA AVANZADA
calcular_max_seguidas(TAB, COL_SEL, COL, MAX):-  columnaAtPos(COL_SEL, TAB, COLUMNA_ACTUAL),
                                            length(COLUMNA_ACTUAL, ALTURA),
                                            elem_at_pos(ALTURA, COLUMNA_ACTUAL, FICHA),
                                            contar_abajo(FICHA, COLUMNA_ACTUAL, ALTURA, 0, CONT_AB),
                                            contar_izquierda(TAB, COL_SEL, FICHA, ALTURA, 0, CONT_IZQ),
                                            contar_derecha(TAB, COL_SEL, COL, FICHA, ALTURA,0, CONT_DER),
                                            contar_diag_arriba_izq(TAB, COL_SEL, FICHA, ALTURA, 0, CONT_ARI),
                                            contar_diag_arriba_der(TAB, COL_SEL, COL, FICHA, ALTURA, 0, CONT_ARD),
                                            contar_diag_abajo_izq(TAB, COL_SEL, FICHA, ALTURA, 0, CONT_ABI),
                                            contar_diag_abajo_der(TAB, COL_SEL, COL, FICHA, ALTURA, 0, CONT_ABD),
                                            SUM_AB is CONT_AB+1,
                                            SUM_LAT is CONT_IZQ+CONT_DER+1,
                                            write(CONT_IZQ), nl,
                                            write(CONT_DER), nl,
                                            SUM_DIAG_1 is CONT_ARI+CONT_ABD+1,
                                            write(SUM_DIAG_1), nl,
                                            SUM_DIAG_2 is CONT_ARD+CONT_ABI+1,
                                            write(SUM_DIAG_2), nl,
                                            MAX1 is max(SUM_LAT, SUM_AB),
                                            MAX2 is max(SUM_DIAG_1, SUM_DIAG_2),
                                            MAX is max(MAX1, MAX2).
                                            
contar_abajo(_,_,1,INI,CONT):- CONT is INI.
contar_abajo(FICHA, COLUMNA_ACTUAL, ALTURA, INI, CONT):- ALTURA_SIGUIENTE is ALTURA-1,
                                                         elem_at_pos(ALTURA_SIGUIENTE, COLUMNA_ACTUAL, FICHA),
                                                         INI1 is INI+1,
                                                         contar_abajo(FICHA, COLUMNA_ACTUAL, ALTURA_SIGUIENTE, INI1, CONT).
contar_abajo(_,_,_,INI, CONT):- CONT is INI.
                                            
                                            
%Preficados para comprobar la columna
mirar_abajo(_,_,_,1,FIN):- FIN is 0.
mirar_abajo(FICHA, COLUMNA_ACTUAL, ALTURA, CONECTAR, FIN):- ALTURA_SIGUIENTE is ALTURA-1,
                                                            elem_at_pos(ALTURA_SIGUIENTE, COLUMNA_ACTUAL, FICHA),
                                                            CONECTAR_SIG is CONECTAR-1,
                                                            mirar_abajo(FICHA, COLUMNA_ACTUAL, ALTURA_SIGUIENTE, CONECTAR_SIG, FIN).
mirar_abajo(_,_,_,_,FIN):- FIN is 1.


%Predicados para comprobar la fila
contar_izquierda(_,1,_,_,INI, CONT):- CONT is INI.
contar_izquierda(TAB, ULTIMA_COL, FICHA, ALTURA,INI, CONT):-  COL_IZQ is ULTIMA_COL-1,
                                                              columnaAtPos(COL_IZQ, TAB, FICHAS_COL_IZQ),
                                                              elem_at_pos(ALTURA, FICHAS_COL_IZQ, FICHA),
                                                              INI1 is INI+1,
                                                              contar_izquierda(TAB, COL_IZQ, FICHA, ALTURA, INI1, CONT).

contar_izquierda(_,_,_,_,INI,CONT):- CONT is INI.


contar_derecha(TAB, ULTIMA_COL, COL, FICHA, ALTURA,INI, CONT):-  COL_DER is ULTIMA_COL+1,
                                                              COL_DER=<COL,
                                                              columnaAtPos(COL_DER, TAB, FICHAS_COL_DER),
                                                              elem_at_pos(ALTURA, FICHAS_COL_DER, FICHA),
                                                              INI1 is INI+1,
                                                              contar_derecha(TAB, COL_DER, COL, FICHA, ALTURA, INI1, CONT).
contar_derecha(_,_,_,_,_,INI, CONT):- CONT is INI.

%Predicados para comprobar las diagonales
contar_diag_arriba_izq(_,1,_,_,INI,CONT_ARI):- CONT_ARI is INI.
contar_diag_arriba_izq(TAB, ULTIMA_COL, FICHA, ALTURA, INI, CONT_ARI):- COL_IZQ is ULTIMA_COL-1,
                                                                        columnaAtPos(COL_IZQ, TAB, FICHAS_COL_IZQ),
                                                                        ALTURA1 is ALTURA+1,
                                                                        length(FICHAS_COL_IZQ, ALTURA_COL_IZQ),
                                                                        ALTURA1=<ALTURA_COL_IZQ,
                                                                        elem_at_pos(ALTURA1, FICHAS_COL_IZQ, FICHA),
                                                                        INI1 is INI+1,
                                                                        contar_diag_arriba_izq(TAB, COL_IZQ, FICHA, ALTURA, INI1, CONT_ARI).
contar_diag_arriba_izq(_,_,_,_,INI,CONT_ARI):- CONT_ARI is INI.


contar_diag_arriba_der(TAB, ULTIMA_COL, COL, FICHA, ALTURA, INI, CONT_ARD):- COL_DER is ULTIMA_COL+1,
                                                                        COL_DER=<COL,
                                                                        columnaAtPos(COL_DER, TAB, FICHAS_COL_DER),
                                                                        ALTURA1 is ALTURA+1,
                                                                        length(FICHAS_COL_DER, ALTURA_COL_DER),
                                                                        ALTURA1=<ALTURA_COL_DER,
                                                                        elem_at_pos(ALTURA1, FICHAS_COL_DER, FICHA),
                                                                        INI1 is INI+1,
                                                                        contar_diag_arriba_der(TAB, COL_DER, COL, FICHA, ALTURA, INI1, CONT_ARD).
contar_diag_arriba_der(_,_,_,_,_,INI,CONT_ARD):- CONT_ARD is INI.

contar_diag_abajo_izq(_,1,_,_,INI,CONT_ABI):- CONT_ABI is INI.
contar_diag_abajo_izq(TAB, ULTIMA_COL, FICHA, ALTURA, INI, CONT_ABI):-  COL_IZQ is ULTIMA_COL-1,
                                                                        columnaAtPos(COL_IZQ, TAB, FICHAS_COL_IZQ),
                                                                        ALTURA1 is ALTURA-1,
                                                                        length(FICHAS_COL_IZQ, ALTURA_COL_IZQ),
                                                                        ALTURA1=<ALTURA_COL_IZQ,
                                                                        ALTURA1>0,
                                                                        elem_at_pos(ALTURA1, FICHAS_COL_IZQ, FICHA),
                                                                        INI1 is INI+1,
                                                                        contar_diag_abajo_izq(TAB, COL_IZQ, FICHA, ALTURA, INI1, CONT_ABI).
contar_diag_abajo_izq(_,_,_,_,INI,CONT_ABI):- CONT_ABI is INI.

contar_diag_abajo_der(TAB, ULTIMA_COL, COL, FICHA, ALTURA, INI, CONT_ABD):- COL_DER is ULTIMA_COL+1,
                                                                        COL_DER=<COL,
                                                                        columnaAtPos(COL_DER, TAB, FICHAS_COL_DER),
                                                                        ALTURA1 is ALTURA-1,
                                                                        length(FICHAS_COL_DER, ALTURA_COL_DER),
                                                                        ALTURA1=<ALTURA_COL_DER,
                                                                        ALTURA1>0,
                                                                        elem_at_pos(ALTURA1, FICHAS_COL_DER, FICHA),
                                                                        INI1 is INI+1,
                                                                        contar_diag_arriba_der(TAB, COL_DER, COL, FICHA, ALTURA, INI1, CONT_ABD).
contar_diag_abajo_der(_,_,_,_,_,INI,CONT_ABD):- CONT_ABD is INI.


es_ganador(TURNO):-
    JUGADOR is TURNO mod 2,
    ganador(JUGADOR).

no_ganador(TURNO):-
    JUGADOR is TURNO mod 2,
    retract(ganador(JUGADOR)),
    asserta(ganador(-1)).

%Predicados para comprobar si se cumple alguna de las condiciones anteriores.
partida_terminada_ab(FIN,_,_,_,_,TURNO):-  FIN==0,
                           JUGADOR is TURNO mod 2,
                           retract(ganador(-1)),
                           asserta(ganador(JUGADOR)).
partida_terminada_ab(_,TAB, COL, ULTIMA_COL, CONECTA_X,TURNO):- comprobar_victoria(TAB, COL, ULTIMA_COL, CONECTA_X, TURNO,1).

partida_terminada_lat(FIN,_,_,_,_,TURNO):-  FIN=<0,
                                      JUGADOR is TURNO mod 2,
                                      retract(ganador(-1)),
                                       asserta(ganador(JUGADOR)).
partida_terminada_lat(_,TAB, COL, ULTIMA_COL, CONECTA_X,TURNO):- comprobar_victoria(TAB, COL, ULTIMA_COL, CONECTA_X, TURNO, 2).

partida_terminada_diag(FIN,_,_,_,_,TURNO):-  FIN=<0,
                                       JUGADOR is TURNO mod 2,
                                       retract(ganador(-1)),
                                       asserta(ganador(JUGADOR)).
partida_terminada_diag(_,_,_,_,_,_).














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

%Pregunta el numero de filas y columnas que quiere para el tablero
seleccionar_tamano_tablero(ROW, COL, CONECTA_X):- write('Introduce el numero de filas: '),
                                      read(ROW), nl,
                                      write('Introduce el numero de columnas: '),
                                      read(COL), nl,
                                      DIF is ROW - COL,
                                      -3 =< DIF, DIF =< 3,
                                      max(ROW, COL) >= 6,
                                      CONECTA_X is max(ROW, COL)- 3,
                                      write('Para ganar hacen falta '), write(CONECTA_X), write(' fichas seguidas'), nl.

seleccionar_tamano_tablero(ROW, COL, CONECTA_X):-
                                      write('No es una proporción válida.'), nl,
                                      seleccionar_tamano_tablero(ROW, COL, CONECTA_X).


%Se pregunta la jugada de un jugador, y la columna no esta llena, por lo que inserta la ficha y cambia el turno al otro
preguntar_jugada(TURNO, TAB, J1, J2, E, EAUX, COL, ROW, CONECTA_X):-
                                               write('Introduce la columna: '),
                                               read(COL_INSERT), nl,
                                               columnaAtPos(COL_INSERT, TAB, COL_SELECT),
                                               not(columnaLlena(COL_SELECT)),
                                               insertar_ficha(TURNO, COL_INSERT, TAB, TABRES),
                                               comprobar_victoria(TABRES, COL, COL_INSERT, CONECTA_X, TURNO, 0),
                                               TURNO_SIG is TURNO+1,
                                               turno(TURNO_SIG, TABRES, J1, J2, EAUX, E, COL, ROW, CONECTA_X). %Intercambio de E y EAUX (la primera es la del jugador que esta jugando)

%Predicado de jugada con la columna llena, se vuelve a preguntar la jugada otra vez
preguntar_jugada(TURNO, TAB, J1, J2, E, EAUX, COL, ROW, CONECTA_X):-
                                               preguntar_jugada(TURNO, TAB, J1, J2, E, EAUX, COL, ROW, CONECTA_X).


simular_jugada_simple(TURNO, TAB, J1, J2, E, EAUX, COL, ROW, CONECTA_X):-
                                               COL_MAX is COL+1,
                                               random(0, COL_MAX, COL_ALEATORIA),
                                               columnaAtPos(COL_ALEATORIA, TAB, COL_SELECT),
                                               not(columnaLlena(COL_SELECT,1)),
                                               insertar_ficha(TURNO, COL_ALEATORIA, TAB, TABRES),
                                               comprobar_victoria(TABRES, COL, COL_ALEATORIA, CONECTA_X, TURNO, 0),
                                               write('Turno simulado'),nl,
                                               imprimir_turno(TURNO, J1, J2), nl,
                                               escribir_indices(1,COL),
                                               escribir_tablero(TABRES, ROW, COL), nl,
                                               TURNO_SIG is TURNO+1,
                                               turno(TURNO_SIG, TABRES, J1, J2, EAUX, E, COL, ROW, CONECTA_X). %Intercambio de E y EAUX (la primera es la del jugador que esta jugando)

simular_jugada_simple(TURNO, TAB, J1, J2, E, EAUX, COL, ROW, CONECTA_X):-
                                               simular_jugada_simple(TURNO, TAB, J1, J2, E, EAUX, COL, ROW, CONECTA_X).


%Cuando ya se han comparado todas las alternativas, se inserta en la mejor
simular_jugada_avanzada(0, TURNO, TAB, J1, J2, E, EAUX, COL, ROW, CONECTA_X, MEJOR_COL, CONSECUTIVAS):-
     insertar_ficha(TURNO, MEJOR_COL, TAB, TABRES),
     write('INSERTADO EN COLUMNA '), write(MEJOR_COL), write(' PERMITE SEGUIDAS '), writeln(CONSECUTIVAS),
     escribir_indices(1,COL),
     escribir_tablero(TAB, ROW, COL), nl,
     TURNO_SIG is TURNO + 1,
     turno(TURNO_SIG, TABRES, J1, J2, EAUX, E, COL, ROW, CONECTA_X).
     
%Cuando se encuentra la posicion ganadora, el jugador gana
simular_jugada_avanzada(COL_ACTUAL, TURNO, TAB, J1, J2, E, EAUX, COL, ROW, CONECTA_X, _, _):-
     columnaAtPos(COL_ACTUAL, TAB, COL_SELECT),
     not(columnaLlena(COL_SELECT,1)),
     insertar_ficha(TURNO, COL_ACTUAL, TAB, TAB_PRUEBA),
     comprobar_victoria(TAB_PRUEBA, COL, COL_ACTUAL, CONECTA_X, TURNO, 0),
     es_ganador(TURNO),
     escribir_indices(1,COL),
     escribir_tablero(TAB, ROW, COL), nl,
     writeln('HA ENCONTRADO PARA GANAR'), write('EN COLUMNA NUMERO '), writeln(COL_ACTUAL),
     TURNO_SIG is TURNO + 1,
     turno(TURNO_SIG, TAB_PRUEBA, J1, J2, EAUX, E, COL, ROW, CONECTA_X).

%Cuando se encuentra que es la posicion ganadora para el rival
simular_jugada_avanzada(COL_ACTUAL, TURNO, TAB, J1, J2, E, EAUX, COL, ROW, CONECTA_X, MEJOR_COL, MEJOR_VALOR):-
     columnaAtPos(COL_ACTUAL, TAB, COL_SELECT),
     not(columnaLlena(COL_SELECT,1)),
     TURNO_RIVAL is (TURNO + 1) mod 2, %Para que piense que somos el rival
     insertar_ficha(TURNO_RIVAL, COL_ACTUAL, TAB, TAB_PRUEBA),
     comprobar_victoria(TAB_PRUEBA, COL, COL_ACTUAL, CONECTA_X, TURNO_RIVAL, 0),
     es_ganador(TURNO_RIVAL),
     writeln('HA ENCONTRADO PARA TAPAR JUGADA DEL RIVAL'), write('EN COLUMNA NUMERO '), writeln(COL_ACTUAL),
     no_ganador(TURNO_RIVAL), %Quitamos que sea ganador, ya que era temporal
     MEJOR_COL is COL_ACTUAL,
     MEJOR_VALOR is CONECTA_X, %Maxima prioridad a esta columna, ya que nos haría perder
     COL_SIG is COL_ACTUAL - 1,
     simular_jugada_avanzada(COL_SIG, TURNO, TAB, J1, J2, E, EAUX, COL, ROW, CONECTA_X, MEJOR_COL, MEJOR_VALOR).

%Caso general, no se gana ni se pierde, se busca encadenar el numero maximo posible de fichas
simular_jugada_avanzada(COL_ACTUAL, TURNO, TAB, J1, J2, E, EAUX, COL, ROW, CONECTA_X, MEJOR_COL, MEJOR_VALOR):-
     columnaAtPos(COL_ACTUAL, TAB, COL_SELECT),
     not(columnaLlena(COL_SELECT,1)),
     insertar_ficha(TURNO, COL_ACTUAL, TAB, TAB_PRUEBA),
     calcular_max_seguidas(TAB_PRUEBA, COL_ACTUAL, COL, CONSECUTIVAS),
     write('COLUMNA NUMERO '), write(COL_ACTUAL), write(' PERMITE SEGUIDAS '), writeln(CONSECUTIVAS),
     actualizar_mejor_col(MEJOR_COL, MEJOR_VALOR, COL_ACTUAL, CONSECUTIVAS),
     COL_SIG is COL_ACTUAL - 1,
     simular_jugada_avanzada(COL_SIG, TURNO, TAB, J1, J2, E, EAUX, COL, ROW, CONECTA_X, MEJOR_COL, MEJOR_VALOR).
     
%Si esa columna esta llena, pasa a la siguiente
simular_jugada_avanzada(COL_ACTUAL, TURNO, TAB, J1, J2, E, EAUX, COL, ROW, CONECTA_X, MEJOR_COL, MEJOR_VALOR):-
     columnaAtPos(COL_ACTUAL, TAB, COL_SELECT),
     columnaLlena(COL_SELECT,1),
     write('COLUMNA NUMERO '), write(COL_ACTUAL), writeln(' LLENA '),
     COL_SIG is COL_ACTUAL - 1,
     simular_jugada_avanzada(COL_SIG, TURNO, TAB, J1, J2, E, EAUX, COL, ROW, CONECTA_X, MEJOR_COL, MEJOR_VALOR).
     
actualizar_mejor_col(MEJOR_COL, MEJOR_VALOR, COL_ACTUAL, CONSECUTIVAS):-
     CONSECUTIVAS > MEJOR_VALOR,
     MEJOR_COL is COL_ACTUAL,
     MEJOR_VALOR is CONSECUTIVAS.
actualizar_mejor_col(_, _, _, _).



%Simboliza el turno de un humano (E = 0), se imprime el tablero y se pregunta jugada
turno(TURNO, TAB, J1, J2, 0, EAUX, COL, ROW, CONECTA_X):-   not(tablero_lleno(TAB,COL,ROW)),
                                                            ganador(-1),
                                                            NUMERO_JUGADA is TURNO+1,
                                                            write('Jugada numero '), write(NUMERO_JUGADA), write('. '), nl,
                                                            imprimir_turno(TURNO, J1, J2), nl,
                                                            escribir_indices(1,COL),
                                                            escribir_tablero(TAB, ROW, COL), nl,
                                                            preguntar_jugada(TURNO, TAB, J1, J2, 0, EAUX, COL, ROW, CONECTA_X).

%Simboliza el turno de un PC simple (E = 1), se imprime el tablero y se simula la jugada del pc
turno(TURNO, TAB, J1, J2, 1, EAUX, COL, ROW, CONECTA_X):-   not(tablero_lleno(TAB,COL,ROW)),
                                                            ganador(-1),
                                                            NUMERO_JUGADA is TURNO+1,
                                                            write('Jugada numero '), write(NUMERO_JUGADA), write('. '), nl,
                                                            simular_jugada_simple(TURNO, TAB, J1, J2, 1, EAUX, COL, ROW, CONECTA_X).

%Simboliza el turno de un PC avanzado (E = 2), se imprime el tablero y se simula la jugada del pc
turno(TURNO, TAB, J1, J2, 2, EAUX, COL, ROW, CONECTA_X):-   not(tablero_lleno(TAB,COL,ROW)),
                                                            ganador(-1),
                                                            NUMERO_JUGADA is TURNO+1,
                                                            write('Jugada numero '), write(NUMERO_JUGADA), write('. '), nl,
                                                            %La jugada avanzada va a empezar a analizar el tablero de derecha a izquierda y va a insertar en la mejor
                                                            simular_jugada_avanzada(COL, TURNO, TAB, J1, J2, 2, EAUX, COL, ROW, CONECTA_X, COL, 0).


%Turno con el tablero lleno, fin del juego
turno(TURNO, TAB, _, _, _, _, COL, ROW, _):-   tablero_lleno(TAB, COL, ROW),
                                                ganador(-1),
                                                       write('El tablero, se ha llenado, el juego ha acabado en EMPATE tras '), write(TURNO), write(' jugadas.'), nl,
                                                       write('Otra partida?'), nl.

%Turno con victoria, fin del juego
turno(_, _, J1, _, _, _, _, _, _):-
    ganador(0),
    write('Fin de la partida! Ha ganado '), write(J1).

%Turno con victoria, fin del juego
turno(_, _, _, J2, _, _, _, _, _):-
    write('Fin de la partida! Ha ganado '), write(J2).


%Establece el juego (jugadores y tablero) y empieza el juego
jugar():-
    seleccionar_modo_juego(J1,J2,E1,E2),
    seleccionar_tamano_tablero(ROW, COL, CONECTA_X),
    assertz(tamCol(ROW)), %El tamaño de una columna es el numero de filas
    assertz(tamRow(COL)), %El tamaño de una fila es el numero de columnas
    assertz(numRows(ROW)),
    assertz(numCols(COL)),
    assertz(nombre_j1(J1)),
    assertz(nombre_j2(J2)),
    assertz(estrategia_j1(E1)),
    assertz(estrategia_j2(E2)),
    assertz(ganador(-1)),
    generar_tablero_inicial(TAB, COL),
    escribir_indices(1,COL),
    escribir_tablero(TAB, ROW,COL),
    write('Comienza el juego entre '), write(J1), write(' y '), write(J2), nl,
    turno(0, TAB, J1, J2, E1, E2, COL, ROW, CONECTA_X). %Turno inicial para el jugador 0
