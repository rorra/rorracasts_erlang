-module(db).
-include("registros.hrl").
-compile(export_all).

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
  mnesia:create_table(categoria, [
    {attributes, record_info(fields, categoria)},
    {type, set},
    {index, [nombre]}]),
  mnesia:create_table(subasta, [
    {attributes, record_info(fields, subasta)},
    {type, set},
    {index, [categoria_id, usuario_id, ganador_id, finalizada]}]),
  mnesia:create_table(oferta, [
    {attributes, record_info(fields, oferta)},
    {type, set},
    {index, [usuario_id, subasta_id]}]),
  esperar_tablas().

esperar_tablas() ->
  ok = mnesia:wait_for_tables([usuario, categoria, subasta, oferta], 1000).

cargar_datos() ->
  case hay_datos() of
    true -> ok;
    _ -> cargar_datos2()
  end.

cargar_datos2() ->
  cargar_usuarios(),
  cargar_categorias().

hay_datos() ->
  case mnesia:table_info(categoria, size) of
    0 -> false;
    _ -> true
  end.

cargar_usuarios() ->
  Usuarios = [
    {1, "rodrigo", "rorra@rorra.com.ar"},
    {2, "juan", "juan@rorra.com.ar"},
    {3, "alberto", "alberto@rorra.com.ar"}
  ],
  Guardar = fun({ID, Nombre, Email}) ->
    mnesia:write(#usuario{id=ID, nombre=Nombre, email=Email})
  end,
  F = fun() -> lists:foreach(Guardar, Usuarios) end,
  {atomic, ok} = mnesia:transaction(F),
  ok.

cargar_categorias() ->
  Categorias = [
    {1, "autos"},
    {2, "laptops"}
  ],
  Guardar = fun({ID, Nombre}) ->
    mnesia:write(#categoria{id=ID, nombre=Nombre})
  end,
  F = fun() -> lists:foreach(Guardar, Categorias) end,
  {atomic, ok} = mnesia:transaction(F),
  ok.

new_id() ->
  erlang:phash2({node(), now()}).
