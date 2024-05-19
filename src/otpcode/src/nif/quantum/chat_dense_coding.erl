%% print_string.erl
-module(chat_dense_coding).
-export([init/0, send_msg/1, get_msg/0]).
-on_load(init/0).
-define(LIBNAME, "libchat_dense_coding").
-include("records.hrl").

init() ->
    erlang:load_nif(filename:join(?PrivDir, ?LIBNAME), 0).

send_msg(_String) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).

get_msg() ->
    erlang:nif_error({not_loaded, ?LIBNAME}).