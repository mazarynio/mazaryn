-module(qft).
-export([init/0, qft_nif/1]).
-on_load(init/0).
-define(LIBNAME, "libqft").
-include("records.hrl").
init() ->
    erlang:load_nif(filename:join(?PrivDir, ?LIBNAME), 0).

%% Example => qft:qft_nif([1, 0, 1, 1, 0]).
qft_nif(_Qubits) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).
