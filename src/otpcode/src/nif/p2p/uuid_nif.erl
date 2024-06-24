-module(uuid_nif).
-author("Zaryn Technologies").
-export([init/0, generate/1]).
-on_load(init/0).
-define(LIBNAME, "libuuid_nif").
-include("records.hrl").

init() ->
    erlang:load_nif(filename:join(?PrivDirP2P, ?LIBNAME), 0).

%% Example => uuid_nif:generate(15).
generate(_Length) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).


