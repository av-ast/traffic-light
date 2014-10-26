-module(traffic_light_encoding).

-export([digit_to_sections/1, sections_to_digits/1, get_missing_bits/1]).

-define(BIT_DEPTH, 7).

-define(ENCODING_MAP, [
    {0, "1110111"},
    {1, "0010010"},
    {2, "1011101"},
    {3, "1011011"},
    {4, "0111010"},
    {5, "1101011"},
    {6, "1101111"},
    {7, "1010010"},
    {8, "1111111"},
    {9, "1111011"}
   ]).

-type sections() :: string().
-type digit() :: [sections()] | integer().

%%% EXPORTED FUNCTIONS

-spec digit_to_sections(Digit :: digit()) -> Sections :: sections() | invalid_digit.
digit_to_sections(Digit) ->
  case lists:keyfind(Digit, 1, ?ENCODING_MAP) of
    {_Digit, Section} -> Section;
    false -> invalid_digit
  end.

-spec sections_to_digits(Sections :: binary() | sections()) ->
                            [{digit(), sections(), sections(), integer()}] |
                            invalid_sections_format.
sections_to_digits(Sections) when is_binary(Sections) ->
  sections_to_digits(binary_to_list(Sections));
sections_to_digits(Sections) ->
  case length(Sections) of
    ?BIT_DEPTH ->
      SuitableDigits = case exact_match(Sections) of
        {Digit, Sections} ->
          [{Digit, Sections}];
        _ ->
          lists:filtermap(fun({_Digit, EtalonSections}) ->
                             are_suitable_sections(Sections, EtalonSections)
                           end, ?ENCODING_MAP)
      end,
      lists:keysort(4,
        lists:map(fun({Digit, DigitSections}) ->
                      MissingSections = utils:xor_binary_nums([DigitSections, Sections], ?BIT_DEPTH),
                      {Digit, DigitSections, MissingSections, missed_bits_count(MissingSections)}
                  end, SuitableDigits));
    _ -> {error, invalid_sections_format}
  end.

get_missing_bits(Digits) ->
  utils:or_binary_nums(lists:map(fun({_, _, Missing, _}) -> Missing end, Digits), ?BIT_DEPTH).

%%% INTERNAL FUNCTIONS

-spec exact_match(Sections :: sections()) -> {digit(), sections()} | false.
exact_match(Sections) ->
  lists:keyfind(Sections, 2, ?ENCODING_MAP).

%% Filter digits which are exactly not suitable (have at least one raised bit in position
%% where it must be equal to zero).
-spec are_suitable_sections(CheckSections :: sections(), EtalonSections :: sections()) -> boolean().
are_suitable_sections(CheckSections, EtalonSections) ->
  BitChecks = lists:zipwith(fun(Bit, RealBit) -> Bit > RealBit end, CheckSections, EtalonSections),
  not lists:any(fun(X) -> X end, BitChecks).

missed_bits_count(MissingSections) ->
  lists:foldl(fun(Bit, Acc) ->
                  if Bit == $1 -> Acc + 1; true -> Acc end
              end, 0, MissingSections).
