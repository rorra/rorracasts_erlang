-module(mercado).
-export([init/0, stop/0, loop/1]).

%% Cliente

init() ->
  Pid = spawn(?MODULE, loop, [[]]),
  register(mercado, Pid),
  Pid.

stop() ->
  whereis(mercado) ! {terminate, normal}.

%% Servidor

loop(Estado) ->
  receive
    {terminate, _Reason} ->
      ok;
    _ ->
      loop(Estado)
  end.

%% Funciones Internas
