-module(irc_server_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


init_test() ->
    MyServer = irc_server:start(),
    {ok, Socket} = gen_tcp:connect({127,0,0,1}, 6667, [{packet, line}, {active, false}]),
    {ok, {Host, Puerto}} = inet:peername(Socket),
    ?assert(MyServer),
    ?assert(Host =:= {127,0,0,1}),
    ?assert(Puerto =:= 6667).
