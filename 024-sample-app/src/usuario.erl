-module(usuario).
-include("registros.hrl").
-export([nuevo_usuario/3,
         find_usuario/1,
         notificar_oferta_creada/2,
         notificar_nueva_oferta/2]).

%% Api
nuevo_usuario(_Nombre, _Email, _Password) ->
  ok.

find_usuario(UsuarioId) ->
  R = usuario:dirty_read(usuario, UsuarioId),
  case R of
    [] ->
      {error, no_existe};
    [Usuario] ->
      {ok, Usuario}
  end.

notificar_oferta_creada(_UsuarioId, _Oferta) ->
  ok.

notificar_nueva_oferta(_UsuarioId, _Oferta) ->
  ok.
