-module(ejemplo).
-export([main/0]).
-export_type([numero/0]).

-type numero() :: number().
-type dia_de_la_semana() :: lunes | martes | miercoles | jueves | viernes | sabado | domingo.

-record(usuario, {
		  id :: pos_integer(),
		  nombre :: string(),
		  estado :: activo | inactivo
		 }).

-spec devolver_entero() -> integer().
devolver_entero() ->
    3.

-spec division(Dividendo, Divisor) -> Resultado when
      Dividendo :: numero(),
      Divisor :: pos_integer(),
      Resultado :: numero().
division(Dividendo, Divisor) ->
    Dividendo / Divisor.

-spec lunes() -> dia_de_la_semana().
lunes() ->
    lunes.

-spec get_usuario() -> #usuario{}.
get_usuario() ->
    #usuario{id="juan", nombre="carlos", estado=activo}.

main() ->
    devolver_entero(),
    lunes(),
    division(10, 1),
    get_usuario().
