-module(hash_nif).
-author("Zaryn Technologies").
-export([init/0, compute_sha256/1]).
-on_load(init/0).
-define(LIBNAME, "libhash_nif").
-include("records.hrl").

init() ->
    erlang:load_nif(filename:join(?PrivDirP2P, ?LIBNAME), 0).

%% Example => hash_nif:compute_sha256("file.txt").
compute_sha256(_File) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).
