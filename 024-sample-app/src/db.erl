-module(db).
-include("registros.hrl").
-export([init/0, new_id/0]).

init() ->
  mnesia:start(),
  mnesia:create_schema([node()]),
  cargar_tablas(),
  cargar_datos(),
  ok.

cargar_tablas() ->
  mnesia:create_table(usuario, [
    {attributes, record_info(fields, usuario)},
    {type, set},
    {index, [email]}]),
  mnesia:create_table(subasta, [
    {attributes, record_info(fields, subasta)},
    {type, set},
    {index, [usuario_id, finalizada]}]),
  mnesia:create_table(oferta, [
    {attributes, record_info(fields, oferta)},
    {type, set},
    {index, [usuario_id, subasta_id]}]),
  esperar_tablas().

esperar_tablas() ->
  ok = mnesia:wait_for_tables([usuario, subasta, oferta], 1000).

cargar_datos() ->
  case hay_datos() of
    true -> ok;
    _ -> cargar_datos2()
  end.

cargar_datos2() ->
  cargar_usuarios().

hay_datos() ->
  case mnesia:table_info(categoria, size) of
    0 -> false;
    _ -> true
  end.

cargar_usuarios() ->
  Usuarios = [
    {new_id(), "rodrigo", "rorra@rorra.com.ar", "123"},
    {new_id(), "juan", "juan@rorra.com.ar", "123"}
  ],
  Guardar = fun({ID, Nombre, Email, Password}) ->
    mnesia:write(#usuario{id=ID, nombre=Nombre, email=Email, password=Password})
  end,
  F = fun() -> lists:foreach(Guardar, Usuarios) end,
  {atomic, ok} = mnesia:transaction(F),
  ok.

new_id() ->
  uuid:uuid_to_string(uuid:get_v4_urandom()).
