-module(randomness).
-author("Zaryn Technologies").
-export([init/0, generate/1]).
-on_load(init/0).
-define(LIBNAME, "librandomness").
-include("records.hrl").

init() ->
    erlang:load_nif(filename:join(?PrivDir, ?LIBNAME), 0).

%% Example => randomness:generate(10)
generate(_Number) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).
