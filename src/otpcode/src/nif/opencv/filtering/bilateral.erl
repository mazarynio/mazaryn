-module(bilateral).
-author("Zaryn Technologies").
-export([init/0, apply/5]).
-on_load(init/0).
-define(LIBNAME, "libbilateral").
-include("../records.hrl").

init() ->
    erlang:load_nif(filename:join(?PrivDirFiltering, ?LIBNAME), 0).

%% Example => bilateral:apply("234.jpg", 9, 75.0, 75.0, 4).
apply(_ImagePath, _D, _SigmaColor, _SigmaSpace, _BorderType) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).
