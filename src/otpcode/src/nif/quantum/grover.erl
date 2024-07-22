-module(grover).
-author("Zaryn Technologies").
-export([init/0, grover_search/3]).
-on_load(init/0).
-define(LIBNAME, "libgrover").
-include("records.hrl").

init() ->
    erlang:load_nif(filename:join(?PrivDir, ?LIBNAME), 0).

%% Example => grover:grover_search(3, 1, 2).
grover_search(_N, _Marked, _Qubits) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).
