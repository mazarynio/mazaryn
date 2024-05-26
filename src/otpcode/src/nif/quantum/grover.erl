%% print_string.erl
-module(grover).
-export([init/0, grover_search/3]).
-on_load(init/0).
-define(LIBNAME, "libgrover").
-include("records.hrl").

init() ->
    erlang:load_nif(filename:join(?PrivDir, ?LIBNAME), 0).

%% Example => randomness:generate(10)
grover_search(_N, _Marked, _Qubits) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).
