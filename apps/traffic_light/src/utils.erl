-module(utils).

-export([ts/0, unix_ts/0, uuid/0]).
-export([xor_binary_nums/1, xor_binary_nums/2]).
-export([or_binary_nums/1, or_binary_nums/2]).
-export([and_binary_nums/1, and_binary_nums/2]).

-spec ts() -> integer().
ts() ->
  {Megasecs, Secs, Microsecs} = now(),
  Microsecs + 1000000 * (Secs + 1000000 * Megasecs).

-spec unix_ts() -> integer().
unix_ts() ->
  {Megasecs, Secs, _} = now(),
  Megasecs * 1000000 + Secs.

-spec uuid() -> string().
uuid() ->
  uuid:to_string(uuid:uuid1()).

-spec xor_binary_nums(Nums :: [string()]) -> string() | invalid_num.
xor_binary_nums(Nums) ->
  integer_to_list(lists:foldl(fun(X, Acc) -> list_to_integer(X, 2) bxor Acc end, 0, Nums), 2).

-spec xor_binary_nums(Nums :: [string()], BitsCount :: integer()) -> string() | invalid_num.
xor_binary_nums(Nums, BitsCount) ->
  align_bits(xor_binary_nums(Nums), BitsCount).

-spec or_binary_nums(Nums :: [string()]) -> string() | invalid_num.
or_binary_nums(Nums) ->
  integer_to_list(lists:foldl(fun(X, Acc) -> list_to_integer(X, 2) bor Acc end, 0, Nums), 2).

-spec or_binary_nums(Nums :: [string()], BitsCount :: integer()) -> string() | invalid_num.
or_binary_nums(Nums, BitsCount) ->
  align_bits(or_binary_nums(Nums), BitsCount).

-spec and_binary_nums(Nums :: [string()]) -> string() | invalid_num.
and_binary_nums(Nums) ->
  integer_to_list(lists:foldl(fun(X, Acc) -> list_to_integer(X, 2) band Acc end, 0, Nums), 2).

-spec and_binary_nums(Nums :: [string()], BitsCount :: integer()) -> string() | invalid_num.
and_binary_nums(Nums, BitsCount) ->
  align_bits(and_binary_nums(Nums), BitsCount).

%% INTERNAL FUNCTIONS

align_bits(BinaryNum, BitsCount) ->
  Len = length(BinaryNum),
  if
    BitsCount > Len -> lists:duplicate(BitsCount - Len, $0) ++ BinaryNum;
    true -> BinaryNum
  end.

