-module(funs).
-compile(export_all).

doble([]) ->
    [];
doble([H|T]) ->
    [H * 2] ++ doble(T).

suma_uno([]) ->
    [];
suma_uno([H|T]) ->
    [H + 1] ++ suma_uno(T).

map(_F, []) ->
    [];
map(F, [H|T]) ->
    [F(H)] ++ map(F, T).

multiplicador(N) ->
    fun(X) ->
	    X * N
    end.
	
