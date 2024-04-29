-module(cpp_nif).
-export([call_cpp_code/0]).
-include("record.hrl").

-on_load(init/0).

init() ->
    erlang:load_nif(filename:join(?PrivDir, "cpp_nif"), 0).

call_cpp_code() ->
    erlang:nif_error({not_loaded, ?LINE}).
