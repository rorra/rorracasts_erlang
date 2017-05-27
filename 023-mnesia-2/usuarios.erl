-module(usuarios).
-include("registros.hrl").
-compile(export_all).

nuevo(Nombre, Email) ->
  Usuario = #usuario{id=db:new_id(), nombre=Nombre, email=Email},
  case buscar_por_email(Email) of
    #usuario{} ->
      {error, "ya existe un usuario con el Email"};
    _ ->
      {guardar(Usuario)}
  end.

guardar(Usuario = #usuario{}) ->
  F = fun() -> mnesia:write(Usuario) end,
  mnesia:activity(sync_transaction, F).

listar() ->
  F = fun() ->
    mnesia:match_object(usuario, #usuario{_='_'}, read)
  end,
  mnesia:activity(sync_transaction, F).

buscar_por_email(Email) ->
  F = fun() ->
    mnesia:index_match_object(usuario, #usuario{email=Email, _='_'}, #usuario.email, read)
  end,
  {atomic, Dato} = mnesia:transaction(F),
  case Dato of
    [] -> no_encontrado;
    _ -> hd(Dato)
  end.

buscar_por_nombre(Nombre) ->
  F = fun() ->
    mnesia:match_object(usuario, #usuario{nombre=Nombre, _='_'}, read)
  end,
  {atomic, Dato} = mnesia:transaction(F),
  case Dato of
    [] -> no_encontrado;
    _ -> hd(Dato)
  end.

eliminar(ID) ->
  F = fun() -> mnesia:delete({usuario, ID}) end,
  mnesia:activity(sync_transaction, F).
