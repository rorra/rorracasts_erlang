-module(geometria).
-export([area/1]).

-import(math, [pi/0]).

area({circulo, Radio}) ->
    pi() * (Radio * Radio);
area({cuadrado, Lado}) ->
    Lado * Lado;
area({rectangulo, Alto, Ancho}) ->
    Alto * Ancho.
