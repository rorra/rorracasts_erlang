-module(web_server).
-export([init/0, stop/0]).

init() ->
  application:start(crypto),
  application:start(cowlib),
  application:start(ranch),
  application:start(cowboy),
  Dispatch = compilar_rutas(),
  {ok, _} = cowboy:start_http(my_http_listener, 100,
                              [{port, 8080}],
                              [{env, [{dispatch, Dispatch}]}]
  ).

stop() ->
  application:stop(cowboy),
  application:stop(ranch),
  application:stop(cowlib),
  application:stop(crypto).

compilar_rutas() ->
  cowboy_router:compile([
    {'_', [
      {"/usuarios", usuarios_handler, []},
      {"/subastas", subastas_handler, []}
    ]}
  ]).
