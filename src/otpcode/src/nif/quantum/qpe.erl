%% print_string.erl
-module(qpe).
-export([init/0, estimation/1]).
-on_load(init/0).
-define(LIBNAME, "libqpe").
-include("records.hrl").
init() ->
    erlang:load_nif(filename:join(?PrivDir, ?LIBNAME), 0).

%% Example => qpe:estimation(0.25).
estimation(_Theta) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).
