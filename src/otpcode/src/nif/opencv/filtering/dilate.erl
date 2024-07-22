-module(dilate).
-author("Zaryn Technologies").
-export([init/0, apply/7]).
-on_load(init/0).
-define(LIBNAME, "libdilate").
-include("../records.hrl").

init() ->
    erlang:load_nif(filename:join(?PrivDirFiltering, ?LIBNAME), 0).

%% Example => dilate:apply("234.jpg", 0, 3, -1, -1, 1, 4).
apply(_ImagePath, _KernelType, _KernelSize, _AnchorX, _AnchorY, _Iterations, _BorderType) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).
