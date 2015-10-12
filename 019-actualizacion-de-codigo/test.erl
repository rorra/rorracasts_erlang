-module(test).
-compile(export_all).
-vsn(1.1).

-define(VERSION, v1_1).

start() ->
    V1 = spawn(?MODULE, loop, []),
    register(?VERSION, V1),
    V1.

loop() ->
    timer:sleep(2000),
    io:format("Version ~w~n", [?VERSION]),
    test:loop().
