%% print_string.erl

-module(print_string).
-export([init/0, print_string/1]).
-include("records.hrl").

-on_load(init/0).

init() ->
    erlang:load_nif(filename:join(?PrivDir, "print_string"), 0).

print_string(X) ->
    print_string:print_string(X).

