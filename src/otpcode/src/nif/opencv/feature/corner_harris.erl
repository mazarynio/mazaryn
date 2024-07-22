-module(corner_harris).
-author("Zaryn Technologies").
-export([init/0, apply/6]).
-on_load(init/0).
-define(LIBNAME, "libcorner_harris").
-include("../records.hrl").

init() ->
    erlang:load_nif(filename:join(?PrivDirFeature, ?LIBNAME), 0).

%% Example => corner_harris:apply("image.jpg", "output.jpg", 2, 3, 0.04, 4).
apply(_ImagePath, _OutputPath, _BlockSize, _Ksize, _K, _BorderType) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).
