-module(ejemplo_sin_tipos).

-record(usuario, {
		  id,
		  nombre,
		  estado
		 }).

devolver_entero() ->
    3.

division(Dividendo, Divisor) ->
    Dividendo / Divisor.

lunes() ->
    lunes.

get_usuario() ->
    #usuario{id=1, nombre="carlos", estado=activo}.

main() ->
    devolver_entero(),
    lunes(),
    division(10, -1),
    get_usuario().
