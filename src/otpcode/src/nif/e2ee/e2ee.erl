-module(e2ee).
-author("Zaryn Technologies").
-export([init/0, generate_key/0, derive_secret/2, encrypt_message/2, decrypt_message/2, sign/2, verify/3]).
-on_load(init/0).
-define(LIBNAME, "libe2ee").
-include("records.hrl").

init() ->
    erlang:load_nif(filename:join(?E2EE, ?LIBNAME), 0).

%% Example => uuid_nif:generate(15).
generate_key() ->
    erlang:nif_error({not_loaded, ?LIBNAME}).

derive_secret(_, _) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).

encrypt_message(_, _) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).

decrypt_message(_, _) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).

sign(_, _) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).

verify(_, _, _) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).

