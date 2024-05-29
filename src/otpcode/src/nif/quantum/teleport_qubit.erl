-module(teleport_qubit).
-author("Zaryn Technologies").
-export([init/0, qubit_teleport/4]).
-on_load(init/0).
-define(LIBNAME, "libteleport_qubit").
-include("records.hrl").
init() ->
    erlang:load_nif(filename:join(?PrivDir, ?LIBNAME), 0).

%% Example => teleport_qubit:qubit_teleport(0.7071, 0.0, 0.7071, 0.0).
qubit_teleport(_Data1, _Data2, _Data3, _Data4) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).

