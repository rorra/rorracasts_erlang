-module(compresion).
-compile(export_all).

map(F, Lista) ->
    [F(X) || X <- Lista].

ordenar([Pivote|Cola]) ->
    ordenar([X || X <- Cola,
		   X < Pivote]) ++
	[Pivote] ++
	ordenar([X || X <- Cola,
		      X >= Pivote]);
ordenar([]) -> [].



