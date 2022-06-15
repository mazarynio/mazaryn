-module(p2p_utils).
-export([generate_id/0, get_id/0]).

-define(ID_LENGTH, 12).
-define(CHAR_LIST, "0123456789abcdefghijklmnopqrstuvwxyz"). %36 chars

generate_id() ->
    crypto:strong_rand_bytes(20).

%% @spec (get_id()-> string())
-spec(get_id()-> string()).

get_id()->
	rand:seed(erlang:system_time()),
	"-PB0000-" ++ get_id(?ID_LENGTH , []).

%% genrate an list of chars at random.

get_id(0, Acc)->
	Acc;
get_id(Num, Acc)->
	Elem = lists:nth(round(rand:uniform()*35)+1, ?CHAR_LIST),
	get_id(Num-1, [Elem | Acc]).

