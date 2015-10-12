-module(decisiones).
-compile(export_all).

mayor_que_1(X) ->
    if X > 5 ->
	    super_true;
       X > 1 ->
	    true;
       true ->
	    false
    end.

mayor_que_0(X) ->
    case X of
	_ when X > 0 ->
	    true;
	_ ->
	    false
    end.

for(N, N, F) ->
    F(N);
for(I, N, F) ->
    F(I),
    for(I+1, N, F).

while() ->
    Repetir = io:get_line("Repetir? [S/N]: "),
    case Repetir of
	"S\n" ->
	    while();
	_ ->
	    void
    end.
    


