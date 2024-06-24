-module(file_nif).
-author("Zaryn Technologies").
-export([init/0, upload_file/2, send_file/3, receive_file/2]).
-on_load(init/0).
-define(LIBNAME, "libfile_nif").
-include("records.hrl").

init() ->
    erlang:load_nif(filename:join(?PrivDirP2P, ?LIBNAME), 0).

upload_file(_FileName, _FilePath) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).

send_file(_IP, _PORT, _FilePath) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).

receive_file(_IP, _PORT) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).