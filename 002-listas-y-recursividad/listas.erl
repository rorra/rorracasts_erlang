-module(listas).
-compile(export_all).

cantidad([]) ->
    0;
cantidad([_Cabeza | Cola]) ->
    1 + cantidad(Cola).

cantidadO(L) ->
    cantidadOH(L, 0).

cantidadOH([], Suma) ->
    Suma;
cantidadOH([_H|T], Suma) ->
    cantidadOH(T, Suma + 1).

suma([]) ->
    0;
suma([Cabeza | Cola]) ->
    Cabeza + suma(Cola).

reversa([]) ->
    [];
reversa([Cabeza | Cola]) ->    
    reversa(Cola) ++ [Cabeza].

combinar([], []) ->
    [];
combinar([], L) ->
    L;
combinar(L, []) ->
    L;
combinar([H1 | T1], [H2 | T2]) ->
    [H1] ++ [H2] ++ combinar(T1, T2).
