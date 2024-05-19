%% print_string.erl
-module(print_string).
-export([init/0, print_string/1]).
-on_load(init/0).

-define(LIBNAME, "libprint_string").
-include("records.hrl").

init() ->
    erlang:load_nif(filename:join(?PrivDir, ?LIBNAME), 0).

print_string(_String) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).