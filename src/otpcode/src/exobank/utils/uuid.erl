-module(uuid).
-export([generate/0, get_rand_string/0, uuid2/0, v4/0, to_string/1, get_parts/1]).

generate() ->
  <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
  list_to_binary(io_lib:format("~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b",
                  [A, B, C band 16#0fff, D band 16#3fff bor 16#8000, E])).

get_rand_string() ->
  AllowedChars = "qwAbBcCdDe#%&@*EertyQWERTY1234567890",
  Length = 32,
  lists:foldl(fun(_, Acc) ->
    [lists:nth(rand:uniform(length(AllowedChars)),
      AllowedChars)]
  ++ Acc
  end, [], lists:seq(1, Length)).

uuid2() ->
  list_to_binary(uuid:uuid_to_string(uuid:get_v4())).

v4() ->
  v4(rand:uniform(1, round(math:pow(2, 48))) - 1, rand:uniform(1, round(math:pow(2, 12))) - 1, rand:uniform(1, round(math:pow(2, 32))) - 1, rand:uniform(1, round(math:pow(2, 30))) - 1).

v4(R1, R2, R3, R4) ->
    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.

% Returns a string representation of a binary UUID.
to_string(U) ->
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", get_parts(U))).

get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
    [TL, TM, THV, CSR, CSL, N].