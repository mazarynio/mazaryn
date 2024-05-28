-module(teleport_qudit).
-author("Zaryn Technologies").
-export([init/0, qudit_teleport/1]).
-on_load(init/0).
-define(LIBNAME, "libteleport_qudit").
-include("records.hrl").
init() ->
    erlang:load_nif(filename:join(?PrivDir, ?LIBNAME), 0).

%% Example => qpe:estimation(0.25).
qudit_teleport(_Data) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).

