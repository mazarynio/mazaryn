-module(mynode).
-export([start/1, get/1]).

start(Name) ->
    {ok, Hostname} = inet:gethostname(),
    ResultString = lists:concat([Name, "@", Hostname]),
    ResultAtom = list_to_atom(ResultString),
    ResultAtom.

get(X) ->
    Z = start(X),
    erlang:get_cookie(Z).
