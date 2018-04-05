-module(subasta).
-include("registros.hrl").
-export([init/5, init/1, loop/1]).

%% Api

init(UsuarioId, Titulo, Descripcion, Precio, FechaFin) ->
  Subasta = iniciar_subasta(UsuarioId, Titulo, Descripcion, Precio, FechaFin),
  {Pid, Ref} = spawn_monitor(?MODULE, loop, [Subasta]),
  %% No hacer nunca, los atomos no son liberados por el GC, es una demo
  SubastaAtom = list_to_atom(Subasta#subasta.id),
  ok = register(SubastaAtom, Pid),
  {Pid, Ref}.

init(Subasta = #subasta{}) ->
  {Pid, Ref} = spawn_monitor(?MODULE, loop, [Subasta]),
  %% No hacer nunca, los atomos no son liberados por el GC, es una demo
  SubastaAtom = list_to_atom(Subasta#subasta.id),
  ok = register(SubastaAtom, Pid),
  {Pid, Ref};
init(SubastaId) ->
  Subasta = find_subasta(SubastaId),
  {Pid, Ref} = spawn_monitor(?MODULE, loop, [Subasta]),
  %% No hacer nunca, los atomos no son liberados por el GC, es una demo
  SubastaAtom = list_to_atom(Subasta#subasta.id),
  ok = register(SubastaAtom, Pid),
  {Pid, Ref}.

%% Servidor

loop(Subasta) ->
  receive
    {Desde, {ofertar, UsuarioId, Precio}} ->
      ofertar(Subasta, UsuarioId, Precio),
      Desde ! ok,
      loop(Subasta);
    {Desde, {consultar}} ->
      Desde ! Subasta,
      loop(Subasta);
    _ ->
      loop(Subasta)
  after 1000 ->
    case Subasta#subasta.fecha_fin >= qdate:unixtime() of
      true ->
        finalizar_subasta(Subasta),
        ok;
      _ ->
        loop(Subasta)
    end
  end.

%% Funciones Internas

find_subasta(SubastaId) ->
  [Subasta] = mnesia:dirty_read({subasta, SubastaId}),
  Subasta.

iniciar_subasta(UsuarioId, Titulo, Descripcion, Precio, FechaFin) ->
  #subasta{id=db:new_id(), usuario_id=UsuarioId, titulo=Titulo,
           descripcion=Descripcion, precio_inicial=Precio, fecha_fin=FechaFin}.

ofertar(Subasta, UsuarioId, Precio) ->
  case Subasta#subasta{finalizada=true} of
    true ->
      {error, subasata_finalizada};
    _ ->
      case usuario:find_usuario(UsuarioId) of
        {error, no_existe} ->
           {error, usuario_no_existe};
        _ ->
          case Precio =< Subasta#subasta.precio_final of
            true ->
              {error, precio_no_superado};
            _ ->
              guardar_oferta(Subasta, UsuarioId, Precio)
          end
      end
  end.

guardar_oferta(Subasta, UsuarioId, Importe) ->
  Oferta = #oferta{usuario_id=UsuarioId, subasta_id = Subasta#subasta.id, fecha=qdate:unixttime(), importe=Importe},
  F = fun() ->
    mnesia:write(Oferta),
    % Si faltan menos de 30 segundos para que termine la oferta, actualizar el tiempo a 30 segundos
    case (qdate:unixtime() + 30) > Subasta#subasta.fecha_fin of
      true ->
        NuevaSubasta = Subasta#subasta{fecha_fin = qdate:unixtime() + 30},
        mnesia:write(NuevaSubasta);
      _ ->
        ok
    end
  end,
  {ok, atomic} = mnesia:transaction(F),
  ok.

finalizar_subasta(Subasta) ->
  SubastaFin = Subasta#subasta{finalizada=true},
  F = fun() ->
    mnesia:write(SubastaFin)
  end,
  {atomic, ok} = mnesia:transaction(F).
