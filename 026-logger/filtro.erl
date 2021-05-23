-module(filtro).
-include_lib("kernel/include/logger.hrl").
-export([log_error/1, filter/2]).

log_error(Mensaje) ->
    ?LOG_ERROR(Mensaje),
    ok.

filter(Evento, Argumentos) ->
    io:format("Evento: ~p~n", [Evento]),
    io:format("Argumentos: ~p~n", [Argumentos]),
    #{msg := Mensaje} = Evento,
    case Mensaje of
	{string, "stop"} -> stop;
	{string, "ignore"} -> ignore;
	{string, "Modificar"} -> Evento#{msg := {string, "lo modifique"}};
	_ -> Evento
    end.
