-module(fallback_handler).

-export([init/2]).

init(Req, Opts) ->
  Json = jsx:encode([{status, <<"error">>}, {msg, <<"Not found">>}]),
  Req2 = cowboy_req:reply(404,
            [
              {<<"content-type">>, <<"application/json">>},
              {<<"connection">>, <<"close">>}
            ], Json, Req),
  {ok, Req2, Opts}.
