-module(cpp_nif).
-export([call_cpp_code/0]).

-on_load(init/0).

init() ->
    %% make sure to set the environment variable in the shell first via 'export'
    PrivDir = case os:getenv("NIF_DIR") of
        false ->
            %% Handle case when environment variable is not set
            erlang:error("NIF_DIR environment variable is not set");
        Dir ->
            Dir
    end,
    erlang:load_nif(filename:join(PrivDir, "cpp_nif"), 0).

call_cpp_code() ->
    erlang:nif_error({not_loaded, ?LINE}).