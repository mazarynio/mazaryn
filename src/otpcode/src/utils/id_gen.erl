-module(id_gen).
-compile([export_all, nowarn_export_all]).
-define(VERSION, 4).
-define(VARIANT, 2#10).
-type uuid() :: bitstring().

new() ->
  <<U0:32, U1:16, _:4, U2:12, _:2, U3:30, U4:32>> = crypto:strong_rand_bytes(16),
  <<U0:32, U1:16, ?VERSION:4, U2:12, ?VARIANT:2, U3:30, U4:32>>.

  v4() ->
    <<A1:48, _:4, B1:12, _:2, C1:62>> = crypto:strong_rand_bytes(16),
    Variant = 2,  % Indicates RFC 4122
    Version = 4,  % UUID version number
    Uuid4 = <<A1:48, Version:4, B1:12, Variant:2, C1:62>>,

    <<A:4/binary, B:2/binary, C:2/binary, D:2/binary, E:6/binary>> = Uuid4,
    Parts = [{A, 8}, {B, 4}, {C, 4}, {D, 4}, {E, 12}],
    list_to_binary(string:to_lower(string:join(bins_to_strhexs(Parts), "-"))).

bins_to_strhexs(List) ->
    [binary_to_strhex(X) || X <- List].
binary_to_strhex({Bin, Size}) ->
    string:right(integer_to_list(binary:decode_unsigned(Bin, big), 16), Size, $0).


generate() ->
  <<A:32, B:16, C:16, D:16, E:48>> =
    list_to_binary([trunc(rand:uniform() * 255) || _ <- lists:seq(1, 16)]),
  Res = io_lib:format("~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b",
    [A, B, C band 16#0fff, D band 16#3fff bor 16#8000, E]),
  list_to_binary(Res).
    
-spec to_string(uuid()) -> string().
to_string(<<_:288>> = UUID) ->
  binary_to_list(UUID).




