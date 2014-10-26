-module(traffic_light_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0, config/1]).

start() ->
  {ok, _} = application:ensure_all_started(traffic_light).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  start_cowboy(),
  traffic_light_sup:start_link().

stop(_State) ->
  ok.

config(Key) ->
  {ok, Value} = application:get_env(traffic_light, Key),
  Value.

%% ===================================================================
%% Internal API
%% ===================================================================

start_cowboy() ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/stats", stats_handler, []},
      {"/sequence/create", sequence_handler, []},
      {"/observation/add", observation_handler, []},
      {"/clear", flush_handler, []},
      {'_', fallback_handler, []}
    ]}
  ]),

  {ok, _} = cowboy:start_http(http, config(cowboy_acceptors_num),
                              [{port, config(cowboy_port)}],
                              [{env, [{dispatch, Dispatch}]}]).
