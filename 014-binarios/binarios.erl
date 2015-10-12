-module(binarios).
-compile(export_all).

test() ->
    %% Se escriben numeros en base 2 con el 2# delante
    io:format("2#01: ~w~n", [2#01]),
    io:format("2#10: ~w~n", [2#10]),
    io:format("2#100: ~w~n", [2#100]),
    %% Se escriben numeros en base 16 con el 16# delante
    io:format("16#A: ~w~n", [16#A]),
    io:format("16#10: ~w~n", [16#10]),
    %% Se escribe en general en base N con el N# delante
    io:format("5#321: ~w~n", [5#321]),
    
    %% Erlang maneja binarios (binary), donde cada uno de los elementos del binario es un byte (8 bits).
    %% Erlang maneja bitstrings, que es una secuencia de 0 o mas bits, donde los bits no necesariamente son multiplos de 8.
    %% Por ende, todos los binarios son bitstrings, pero no todos los bitstrings son binarios.

    %% Como los binarios (binary) cuentan con 8 bits, el menor elemento a representar es el 0, y el mayor es el 8.
    _Menor = <<0>>,
    _Mayor = <<255>>,
    
    %% Los binarios se pueden convertir a lista y viceversa
    Lista = "hola",
    BLista = list_to_binary(Lista),
    Lista = binary_to_list(BLista),
    
    %% Los binarios se pueden construir como una secuencia de enteros donde los mismos no sean mayores a 255.
    %% Si es mayor que 255, se utiliza el resto de la division por 255
    io:format("<<255, 256, 0>>: ~w\n", [<<255, 256, 0>>]),
    %% <<"abc">> es lo mismo que hacer <<$a, $b, $c>>
    <<97, 98, 99>> = <<"abc">>,
    %% Se pueden construir binarios desde enteros
    A = 1, B = 2, C = 3,
    io:format("123: ~w~n", [<<A, B, C>>]),
    
    %% Los bitstrings se construyen dandole la cantidad de bits a cada uno, por ejemplo
    %% En 2 bits, puedo almacenar del 0 al 3, es decir, del 2#00 al 2#11
    %% Pero si intento almacenar un 4 en 2 bits, que seria 2#100, lo que hace erlang es almacenarme los dos ultimos bits
    <<0:2>> = <<4:2>>,
    %% Un bitstring de 7 bits podria ser <<1:1, 16:6>>, que almacenar 2#1 y seguido por 2#010000, que todo junto es 2#10100000, que en 10#80
    _Foo1 = <<1:1, 16:6>>,
    
    %% El formato para los binarios de Erlang es Expresion:Tamanio/ListaTipo,
    %% puediendo utilizar Expresion, Expresion:Tamanio
    %% Expresion: Expresion que evalua a un numero
    %% Tamanio: Cantidad en bits para almacenar la Expresion
    %% ListaTipo: Son identificadores separados por -, pudiendo los identificadores ser:
    %%            Tipos: Los tipos validos son integer, float, binary, byte, bites y bitstring
    %%            Signo: Puede ser signed (con signo) o unsigned (sin signo).
    %%            Endian: Puede ser big (por defecto, grande), puede ser little (pequenio), puede ser native.
    %%            Unidad: unit:Val, el tamanio de bits a utilizar va a ser N*Val, donde N es el tipo de dato.
    io:format("<<5:4/little-signed-integer-unit: 4>> ~w~n", [<<5:4/little-signed-integer-unit: 4>>]),
    io:format("<<5:4/big-signed-integer-unit: 4>>: ~w~n", [<<5:4/big-signed-integer-unit: 4>>]),
    
    %% Ejemplo de pattern matching
    Binario1 = <<11, 23, 2523:16>>,
    <<B11:16, B12, B13/binary>> = Binario1,
    _Foo2 = [B11, B12, B13],
    
    %% Cualquier termino de erlang puede ser convertido a binario y viceersa para su serializacion
    Termino = {1, test, 1.3, {otra_tupla}, "una lista", <<"un binario">>},
    TerminoBinario = term_to_binary(Termino),
    Termino = binary_to_term(TerminoBinario),
    
    %% Operadores sobre bits
    %% band compara bit a bit y cuando ambos son 1, deja el 1
    Band = 2#10000 band 2#10010,
    Band = 16 band 18,
    %% bor compara bit a bit y cuando alguno es 1, deja el 1
    Bor = 2#10000 bor 2#000010,
    Bor = 2 bor 16,
    %% bxor compara bit a bit, y solo si apare 1 en alguno, deja el 1
    Bxor = 2#10000 bxor 2#10010,
    Bxor = 16 bxor 18,
    %% bnot
    -4 = bnot(3),
    %% bsl, bits a la izquierda
    Bsl = 2#11 bsl 1,
    Bsl = 6,
    %% bsr, bits a la derecha
    Bsr = 2#110 bsr 1,
    Bsr = 3.
    
    
