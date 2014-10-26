-module(sequence_handler).

-export([init/2, allowed_methods/2, content_types_accepted/2]).
-export([create_sequence/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
  Types = [{{<<"application">>, Type, []}, create_sequence} || Type <- [
               <<"json">>,
               <<"x-www-form-urlencoded">>]],
	{Types, Req, State}.

create_sequence(Req, State) ->
  Resp = case sequence_manager:create() of
    {ok, Sequence} ->
      lager:info("[Sequence was created successfully]: ~p", [Sequence]),
      [{status, <<"ok">>}, {response, [{sequence, Sequence}]}];
    {error, Msg} ->
      lager:info("[Sequence wasn't created]: ~p", [Msg]),
      [{status, <<"error">>}, {msg, Msg}]
  end,

  Req2 = cowboy_req:set_resp_body(jsx:encode(Resp), Req),

  case Resp of
    [{_, <<"ok">>}|_] -> {true, Req2, State};
    [{_, <<"error">>}|_] -> {false, Req2, State}
  end.

