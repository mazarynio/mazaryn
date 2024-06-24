-module(utils_nif).
-author("Zaryn Technologies").
-include("records.hrl").
-export([init/0, get_local_ip_and_port/0, get_public_ip_and_port/2]).
-on_load(init/0).
-define(LIBNAME, "libutils_nif").

init() ->
    erlang:load_nif(filename:join(?PrivDirP2P, ?LIBNAME), 0).

%% Example => utils_nif:get_local_ip_and_port().
get_local_ip_and_port() ->
    erlang:nif_error({not_loaded, ?LIBNAME}). 

%% Example => utils_nif:get_public_ip_and_port("stun.1und1.de", 3478).
get_public_ip_and_port(_StunServer, _StunPort) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).
