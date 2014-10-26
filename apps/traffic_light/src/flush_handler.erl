-module(flush_handler).

-export([init/2, allowed_methods/2, content_types_provided/2]).
-export([flush_sequences/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, flush_sequences}
  ], Req, State}.

flush_sequences(Req, State) ->
  sequence_manager:flush_all(),
  Json = jsx:encode([{status, <<"ok">>}, {response, <<"ok">>}]),
  {Json, Req, State}.
