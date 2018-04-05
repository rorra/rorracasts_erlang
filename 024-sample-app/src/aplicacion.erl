-module(aplicacion).
-export([start/0, start/2, stop/1]).

start() ->
  start(1, 2).

start(_StartType, _StartArgs) ->
  db:init(),
  web_server:init(),
  mercado:init(),
  ok.

stop(_State) ->
  web_server:stop(),
  mercado:stop(),
  ok.
