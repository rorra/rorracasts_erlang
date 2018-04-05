-module(subasta_sup).
-include("registros.hrl").
-export([init/0, loop/1, crear/5, ofertar/3, consultar/1]).

%% Api

init() ->
  {Pid, Ref} = spawn_monitor(?MODULE, loop, []),
  ok = register(Pid, ?MODULE),
  Pid ! {self(), cargar_subastas},
  {Pid, Ref}.

crear(UsuarioId, Titulo, Descripcion, Precio, FechaFin) ->
  Pid = whereis(?MODULE),
  Pid ! {self(), {nueva_subasta, UsuarioId, Titulo, Descripcion, Precio, FechaFin}}.

ofertar(SubastaId, UsuarioId, Precio) ->
  Pid = whereis_subasta(SubastaId#subasta.id),
  Pid ! {self(), {ofertar, UsuarioId, Precio}}.

consultar(SubastaId) ->
  Pid = whereis_subasta(SubastaId),
  Pid ! {self(), {consultar}}.

%% Servidor

loop(Estado) ->
  receive
    {_Desde, {nueva_subasta, UsuarioId, Titulo, Descripcion, Precio, FechaFin}} ->
      subasta:init(UsuarioId, Titulo, Descripcion, Precio, FechaFin),
      loop(Estado);
    {_Desde, {cargar_subasta, SubastaId}} ->
      subasta:init(SubastaId),
      loop(Estado);
    {_Desde, {cargar_subastas}} ->
      cargar_subastas(),
      loop(Estado);
    {'DOWN', _MonitorRef, _Type, {RegisteredName, _Object}, Info} ->
      case Info of
        normal ->
          ok;
        _ ->
          Id = atom_to_list(RegisteredName),
          cargar_subasta(Id)
      end,
      loop(Estado);
    _ ->
      loop(Estado)
  end.

% Funciones internas

cargar_subastas() ->
  F = fun() ->
    ok = mnesia:lock({table, subasta}, write),
    {atomic, Subastas} = mnesia:match_object(subasta, #subasta{_='_'}, write),
    listas:each(fun(X) -> cargar_subasta(X) end, Subastas)
  end,
  mnesia:transaction(F).

cargar_subasta(Subasta = #subasta{}) ->
  subasta:init(Subasta).

whereis_subasta(SubastaId) ->
  case whereis(SubastaId) of
    undefined ->
      whereis(?MODULE) ! {self(), {cargar_subasta, SubastaId}},
      whereis(SubastaId);
    _ ->
      whereis(SubastaId)
  end.
