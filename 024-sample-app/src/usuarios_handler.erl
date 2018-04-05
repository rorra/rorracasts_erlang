-module(usuarios_handler).
-include("registros.hrl").

%% Cowboy Rest
-export([init/2]).
-export([known_methods/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([delete_resource/2]).
-export([allow_missing_post/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

init(Req, State) ->
  {cowboy_rest, Req, State}.

known_methods(Req, State) ->
  {[<<"DELETE">>, <<"GET">>, <<"POST">>, <<"PUT">>], Req, State}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

resource_exists(Req, State) ->
  {#subasta{}, Req, State}.

delete_resource(Req, State) ->
  {"error", Req, State}.

allow_missing_post(Req, State) ->
  {false, Req, State}.

content_types_provided(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  CTA = case Method of
             <<"GET">>    -> [{{<<"application">>, <<"json">>, '*'}, display}];
             <<"DELETE">> -> [{{<<"application">>, <<"json">>, '*'}, undef}];
             <<"POST">>   -> [{{<<"application">>, <<"json">>, '*'}, undef}];
             <<"PUT">>    -> [{{<<"application">>, <<"json">>, '*'}, undef}];
             _ -> []
  end,
  {CTA, Req1, State}.

content_types_accepted(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  CTA = case Method of
             <<"PUT">>  -> [{{<<"application">>, <<"json">>, '*'}, store}];
             <<"POST">> -> [{{<<"application">>, <<"json">>, '*'}, store}];
             _ -> []
  end,
  {CTA, Req1, State}.
