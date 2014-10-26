-module(sequence_manager).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([create/0, flush_all/0, add_observation/1]).
-export([ets_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
          tab :: atom()
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create() ->
  gen_server:call(?MODULE, {create}).

add_observation(Observation) ->
  gen_server:call(?MODULE, {add_observation, Observation}).

ets_stats() ->
  gen_server:call(?MODULE, {ets_stats}).

flush_all() ->
  gen_server:cast(?MODULE, {flush_all}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{ tab = ets:new(sequences, [duplicate_bag, private]) } }.

handle_call({create}, _From, #state{tab=Tab} = State) ->
  Sequence = list_to_binary(utils:uuid()),
  true = ets:insert(Tab, {Sequence, empty_sequence}),
  {reply, {ok, Sequence}, State};
handle_call({add_observation, Observation}, _From, #state{tab=Tab} = State) ->
  Reply = case Observation of
    [{<<"observation">>, [ {<<"color">>,<<"green">>}, {<<"numbers">>,Numbers}]},
     {<<"sequence">>,Sequence}] ->
        handle_green_observation(Tab, Sequence, Numbers);
    [{<<"observation">>, [ {<<"color">>,<<"red">>} ]},
     {<<"sequence">>,Sequence}] ->
        handle_red_observation(Tab, Sequence);
    _ -> {error, <<"Invalid data format">>}
  end,
  {reply, Reply, State};
handle_call({ets_stats}, _From, #state{tab=Tab} = State) ->
  Stats = [
            {ets_objects_count, ets:info(Tab, size)},
            {ets_words_count, ets:info(Tab, memory)}
          ],
  {reply, Stats, State};
handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast({flush_all}, #state{tab=Tab} = State) ->
  true = ets:delete_all_objects(Tab),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_last_observation(Tab, Sequence) ->
  Observations = ets:lookup(Tab, Sequence),
  case Observations of
    [] ->
      {error, missing_sequence};
    [{Sequence, empty_sequence}] ->
      empty_sequence;
    _ ->
      lists:last(Observations)
  end.

handle_green_observation(Tab, Sequence, Numbers) ->
  case check_sequence_presence(Tab, Sequence) of
    true  ->
      LastObservation = get_last_observation(Tab, Sequence),
      case LastObservation of
        {Sequence, <<"red">>} ->
          {error, <<"The red observation should be the last">>};
        {Sequence, {<<"green">>, Numbers, _, _}} ->
          {error, <<"No solutions found">>};
        _ ->
          [DigitsH, DigitsL] = lists:map(fun traffic_light_encoding:sections_to_digits/1, Numbers),
          StartValues = lists:map(fun({N,_}) -> N end,
                                  lists:keysort(2, [zip_digits(H,L) || H <- DigitsH, L <- DigitsL])),
          Missings = lists:map(fun(Digits) ->
                                   list_to_binary(traffic_light_encoding:get_missing_bits(Digits))
                               end, [DigitsH, DigitsL]),
          true = ets:insert(Tab, {Sequence, {<<"green">>, Numbers, StartValues, Missings}}),
          {ok, {StartValues, Missings}}
      end;
    false ->
      {error, <<"The sequence isn't found">>}
  end.

handle_red_observation(Tab, Sequence) ->
  case check_sequence_presence(Tab, Sequence) of
    true ->
      LastObservation = get_last_observation(Tab, Sequence),
      case LastObservation of
        {Sequence, {<<"green">>, _, StartValues, Missings}} ->
          true = ets:insert(Tab, {Sequence, <<"red">>}),
          {ok, {StartValues, Missings}};
        empty_sequence ->
          {error, <<"There isn't enough data">>};
        {Sequence, <<"red">>} ->
          {error, <<"The red observation should be the last">>};
        _ ->
          {error, <<"Invalid observation">>}
      end;
    false ->
      {error, <<"The sequence isn't found">>}
  end.

check_sequence_presence(Tab, Sequence) ->
  ets:member(Tab, Sequence).

zip_digits(DigitH, DigitL) ->
  {DecimalH, _SectionsH, _MissingSectionsH, MissingBitsCountH} = DigitH,
  {DecimalL, _SectionsL, _MissingSectionsL, MissingBitsCountL} = DigitL,
  {digits_to_number(DecimalH, DecimalL), MissingBitsCountH + MissingBitsCountL}.

digits_to_number(DigitH, DigitL) ->
  DigitH*10 + DigitL.

