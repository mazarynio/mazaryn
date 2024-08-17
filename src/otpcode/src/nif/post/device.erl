-module(device).
-author("Zaryn Technologies").
-export([init/0, nif_device_info/0]).
-on_load(init/0).
-define(LIBNAME, "libdevice").
-include("records.hrl").

init() ->
    erlang:load_nif(filename:join(?PrivDir, ?LIBNAME), 0).

%% device:nif_device_info().
nif_device_info() ->
    erlang:nif_error({not_loaded, ?LIBNAME}). 
