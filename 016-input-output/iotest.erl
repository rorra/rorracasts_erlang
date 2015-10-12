-module(iotest).
-compile(export_all).

test() ->
    %% standard_io es el nombre reservado para la entrada/salida standard
    %% standard_error es el nombre reservado para la salida de error
    
    %% Escribiendo a la salida estandard
    io:format("Probando la salida~n"),
    
    %% Viendo los caracteres de escape en detalle
    %% ~c es un entero ascii que se imprime como un caracter, se puede usar un ~tc para imprimir un unicode
    io:format("Un caracter ascii y un caracter unicode: ~c | ~tc~n", [97, 1024]),
    %% Se pueden usar los modificadores de enteros para seleccionar los espacios y la cantidad de caracteres a imprimir
    io:format("Un caracter ascii en 10 espacios publicado 5 veces: ~10.5c~n", [97]),
    %% ~f escribe flotantes, la precision por defecto es 6 pero se puede cambiar con un numero despues del punto
    io:format("~~f por defecto es ~f y ~~.10f es ~.10f~n", [6.123456789, 6.123456789]),
    %% ~e sirve para notacion cientifica
    io:format("~~e por defecto es ~e y ~~.10e es ~.10e~n", [600000.123456789, 600000.123456789]),
    %% ~g usa ~f si el argumento esta entre >= 0.1 y < 10000.0, sino utiliza ~e
    io:format("~~g para 6.0 es ~g y ~~g para 60000.0 es ~g~n", [6.0, 60000.0]),
    %% ~s imprime cadenas, pero puede ser listas de enteros ascii, binarios o atomos.
    %% si se utiliza enteros a la izquierda del punto, se alinea a la izquierda.
    %% si se utiliza enteros a la derecha del punto, se alinea a la derecha.
    %% puedo tener un modificador de unticode utilizando ~ts
    io:format("~~10s es ~10s y ~~.10s es ~.10s, ~~ts imprimie unicode: ~ts~n", [hola, "chau", [1123, 1125, 2500]]),
    %% ~w imprime termino de erlang con su sintaxis por defecto
    io:format("~~w imprime enteros: ~w~n", [1]),
    io:format("~~w imprime flotantes: ~w~n", [1.12321]),
    io:format("~~w imprime cadenas: ~w~n", ["hola mundo"]),
    io:format("~~w imprime atomos: ~w~n", [hola]),
    io:format("~~w imprime tuplas: ~w~n", [{coordenadas, 23.23, 100.23}]),
    %% ~p imprime al igual que ~w pero rompe las lineas para que se vea mejor, es muy util con tuplas
    Tupla = {datos,
	     [{
		{id, 2000},
		{nacimiento, {fecha, 10, 10, 2014}}
	      },
	      {
		{id, 2010},
		{nacimiento, {fecha, 20, 8, 1981}}
	      }
	     ]
	    },
    io:format("~~p imprime tuplas de manera linda: ~p~n", [Tupla]),
    %% ~W y ~P son iguales a ~w y ~p, pero utilizan un argumento extra para indicar la maxima profundidad a imprimir y el resto lo reemplazan con ...
    io:format("~~W imprime con un nivel maximo: ~W~n", [Tupla, 8]),
    io:format("~~P imprime con un nivel maximo: ~P~n", [Tupla, 8]),
    %% ~.NB imprime con una base N
    io:format("100 en ~~.2B es ~.2B y 100 es ~~.16B es ~.16B~n", [100, 100]),
    
    %% Obteniendo input leyendo una linea entera
    Nombre = io:get_line("Ingrese su nombre: "),
    io:format("Ingreso ~s", [Nombre]),
    
    %% Escribiendo caracteres y leyendo caracteres, standard_io es por defecto, podemos reemplazarlo por otros flujos, archivos, etc.
    io:put_chars(standard_io, "Hola Mundo~n"),
    Caracteres = io:get_chars(standard_io, "Ingrese 4 caracteres: ", 4),
    io:format("Ingreso ~s~n", [Caracteres]),
    
    %% Leyendo y escribiendo terminos de erlang
    io:write(hola),
    _Termino = io:read("Termino de Erlang: "),
    
    %% Los dispositivos de lectura/escritura por lo general tienen columnas y filas que las podemos preguntar
    {ok, Columnas} = io:columns(),
    {ok, Columnas} = io:columns(standard_io),
    {ok, Filas} = io:rows(),
    {ok, Filas} = io:rows(standard_io),
    io:format("La pantalla tiene ~w filas y ~w columnas~n", [Filas, Columnas]),
    
    %% Se pueden obtener opciones y poner opciones de los distintos dispositivos, y el formato de las opciones no esta normalizado,
    %% y tambien podemos cambiar las opciones sobre los dispositivos de entrada/salida
    {ok, Null} = file:open("/dev/null", [read]),
    io:format("Las opciones para /dev/null: ~p~n", [io:getopts(Null)]),
    io:setopts(Null, [{encoding, unicode}]),
    io:format("Las opciones para /dev/null despues de setear el codificado a utf8: ~p~n", [io:getopts(Null)]),
    file:close(Null),
    io:format("Opciones para standard_io: ~p~n", [io:getopts(standard_io)]),
    
    %% Opciones para la apertura de archivos:
    %% read - lectura, debe existir antes
    %% write - escritura, si no existe se crea, si existe se trunca
    %% append - se crea si no existe, si existe las operaciones de escritura se hacen al final del archivo
    %% exclusive - apertura exclusiva del proceso, si existe tira un error
    %% raw - acceso mas rapido porque no se crea un proceso de erlang, pero queda limitada a read, read_line y write
    %% binary - cuando se tuliza, erlang devuelve binarios en lugar de listas
    %% compressed - se puede leer/escribir archivos gzip
    %% {encoding, Encoding} pudiendo el Encoding ser latin1, utf8, utf16, utf32.
    %% {ok, Archivo} = file:open("archivo_con_path", [Modos]).

    %% En archivos podemos escribir terminos de erlang y leerlos de vuelta, fwrite es similar a format
    {ok, Archivo1} = file:open("test.txt", [write]),
    io:fwrite(Archivo1, "~w.~n", [Tupla]),
    file:close(Archivo1),
    
    {ok, Archivo2} = file:open("test.txt", [read]),
    Data1 = io:read(Archivo2, ''),
    file:close(Archivo2),
    io:format("Data: ~p~n", [Data1]),
    
    %% Podemos leer y escribir lineas
    {ok, Archivo3} = file:open("test2.txt", [write]),
    io:format(Archivo3, "esto es una linea~n", []),
    file:close(Archivo3),
    
    {ok, Archivo4} = file:open("test2.txt", [read]),
    Data2 = io:get_line(Archivo4, ''),
    file:close(Archivo4),
    io:format("Linea: ~p~n", [Data2]),
    
    %% Para un uso eficaz de archivos, podemos usar raw y binary
    {ok, Archivo5} = file:open("test3.txt", [write, raw, binary]),
    file:write(Archivo5, <<"datos binarios">>),
    file:close(Archivo5),
    
    {ok, Archivo6} = file:open("test3.txt", [read, raw, binary]),
    Data3 = file:read(Archivo6, 100),
    file:close(Archivo6),
    io:format("Binarios: ~p~n", [Data3]),
    
    %% Podemos usar un acceso directo utilizando file:position, donde la posicion puede ser bog, eof, o un entero
    {ok, Archivo7} = file:open("test3.txt", [read, raw, binary]),
    file:position(Archivo7, 6),
    Data4 = file:read(Archivo7, 100),
    io:format("Binarios de nuevo: ~p~n", [Data4]).
    


