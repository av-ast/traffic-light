-module(utils_tests).

-include_lib("eunit/include/eunit.hrl").

xor_binary_nums_test() ->
  ?assertEqual("11", utils:xor_binary_nums(["01", "10"])),
  ?assertEqual("0", utils:xor_binary_nums(["11", "11"])),
  ?assertEqual("11", utils:xor_binary_nums(["01", "10"], 2)),
  ?assertEqual("0000011", utils:xor_binary_nums(["01", "10"], 7)).

or_binary_nums_test() ->
  ?assertEqual("11", utils:or_binary_nums(["01", "10"])),
  ?assertEqual("0000011", utils:or_binary_nums(["01", "10"], 7)).

and_binary_nums_test() ->
  ?assertEqual("0", utils:and_binary_nums(["01", "10"])),
  ?assertEqual("0000000", utils:and_binary_nums(["01", "10"], 7)).

uuid_test() ->
  Uuid1 = utils:uuid(),
  Uuid2 = utils:uuid(),
  ?assertEqual(36, length(Uuid1)),
  ?assertNotEqual(Uuid1, Uuid2).

ts_test() ->
  [Ts, UnixTs] = [utils:ts(), utils:unix_ts()],
  ?assertEqual(UnixTs, (Ts div 1000000)).
