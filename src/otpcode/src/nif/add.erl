-module(add).
-export([hello_nif/1]).
-include("record.hrl").

-on_load(init/0).

init() ->
    erlang:load_nif(filename:join(?PrivDir, "add"), 0).

hello_nif(X) when is_integer(X) ->
    hello_nif:hello_nif(X).
