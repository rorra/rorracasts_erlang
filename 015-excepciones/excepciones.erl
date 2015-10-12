-module(excepciones).
-compile(export_all).

test() ->
    %% El formato es:
    %%
    %% try Expresion of
    %%     Patron1 [when Guarda1] ->
    %%             Cuerpo1;
    %%     PatronN [when GuardaN] ->
    %%             CuerpoN
    %% catch
    %%     [Clase1:]PatronExcepcion1 [when GuardaExcepcion1] ->
    %%             CuerpoExcepcion1
    %%     [ClaseN:]PatronExcepcionN [when GuardaExcepcionN] ->
    %%             CuerpoExcepcionN
    %% end
    %%
    %% Si queremos atrapar todo, podemos usar _:_ o Tipo:Error, por ejemplo:
    %% try Expresion of
    %%     ...
    %% catch
    %%     Tipo:Error ->
    %%                  ...
    %%     _:_ ->
    %%           ...
    %% end
    %%
    %% Hay tres clases de errrores:
    %% # error: Este tipo de error los genera Erlang y se pueden generar con erlang:error(Termino)
    %% # throw: Este tipo de error se generan usando la sentencia throw(Termino)
    %% # exit: Este tipo de error se genera usando la sentencia exit(Razon), se usa para terminar procesos y no tiene un valor de retorno.
    %%
    try tirar_error(error) of
	_ ->
	     io:format("tirar_error(error) sin error~n")
    catch
	error:Motivo1 ->
	     io:format("~p~n", [{error, Motivo1}])
    end,
    
    try tirar_error(throw) of
	_ ->
	    io:format("tirar_error(throw) sin error~n")
    catch
	throw:Motivo2 ->
	    io:format("~p~n", [{throw, Motivo2}])
    end,
    
    try tirar_error(exit) of
	_ ->
	    io:format("tirar_error(exit) sin error~n")
    catch
	exit:Motivo3 ->
	    io:format("~p~n", [{exit, Motivo3}])
    end.
    

tirar_error(error) ->
    1 / 0;
tirar_error(throw) ->
    throw(hola_mundo);
tirar_error(exit) ->
    exit(saliendo_con_exit).

	       
