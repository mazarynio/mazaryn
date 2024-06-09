-module(build_pyramid).
-author("Zaryn Technologies").
-export([init/0, apply/3]).
-on_load(init/0).
-define(LIBNAME, "libbuild_pyramid").
-include("../records.hrl").

init() ->
    erlang:load_nif(filename:join(?PrivDirFiltering, ?LIBNAME), 0).

%% Example => build_pyramid:apply("234.jpg", 3, 4).
apply(_ImagePath, _Maxlevel, _BorderType) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).
