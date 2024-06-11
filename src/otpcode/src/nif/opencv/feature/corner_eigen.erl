-module(corner_eigen).
-author("Zaryn Technologies").
-export([init/0, apply/5]).
-on_load(init/0).
-define(LIBNAME, "libcorner_eigen").
-include("../records.hrl").

init() ->
    erlang:load_nif(filename:join(?PrivDirFeature, ?LIBNAME), 0).

%% Example => corner_eigenvals_and_vecs:apply("image.jpg", "output.jpg", 2, 3, 4).
apply(_ImagePath, _OutputPath, _BlockSize, _Ksize, _BorderType) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).
