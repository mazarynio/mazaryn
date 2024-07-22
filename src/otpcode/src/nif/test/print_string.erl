-module(print_string).
-export([init/0, print_string/1]).
-on_load(init/0).

-define(PrivDirs, "/home/zaryn/mazaryn/src/otpcode/src/nif/test").
-define(LIBNAME, "libprint_string").

init() ->
    erlang:load_nif(filename:join(?PrivDirs, ?LIBNAME), 0).

print_string(_String) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).
