-module(observation_handler).

-export([init/2, allowed_methods/2, content_types_accepted/2]).
-export([add_observation/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
  Types = [{{<<"application">>, Type, []}, add_observation} || Type <- [
               <<"json">>,
               <<"x-www-form-urlencoded">>]],
  {Types, Req, State}.

add_observation(Req, State) ->
  {ok, Json, Req2} = cowboy_req:body(Req),
  Resp = try jsx:decode(Json) of
    Observation ->
      case sequence_manager:add_observation(Observation) of
        {ok, {Start, Missing}} ->
          [{status, <<"ok">>}, {response, [{start, Start}, {missing, Missing}]}];
        {error, Msg} ->
          [{status, <<"error">>}, {msg, Msg}]
      end
  catch
    _:_ -> [{status, <<"error">>}, {msg, <<"Invalid data format">>}]
  end,

  Req3 = cowboy_req:set_resp_body(jsx:encode(Resp), Req2),

  case Resp of
    [{_, <<"ok">>}|_] -> {true, Req3, State};
    [{_, <<"error">>}|_] -> {false, Req3, State}
  end.

