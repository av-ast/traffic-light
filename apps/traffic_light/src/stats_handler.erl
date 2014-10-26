-module(stats_handler).

-export([init/2, allowed_methods/2, content_types_provided/2]).
-export([stats/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, stats}
  ], Req, State}.

stats(Req, State) ->
  Json = jsx:encode([{status, <<"ok">>}, {response, sequence_manager:ets_stats()}]),
  {Json, Req, State}.
