%% print_string.erl
-module(print_string2).
-export([init/0, print_string2/1]).
-on_load(init/0).

-define(PrivDirs, "/home/zaryn/mazaryn/src/otpcode/src/nif/tensorflow").
-define(LIBNAME, "libprint_string2").

init() ->
    erlang:load_nif(filename:join(?PrivDirs, ?LIBNAME), 0).

print_string2(_String) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).