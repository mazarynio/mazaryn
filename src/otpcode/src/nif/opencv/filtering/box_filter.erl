-module(box_filter).
-author("Zaryn Technologies").
-export([init/0, apply/8]).
-on_load(init/0).
-define(LIBNAME, "libbox_filter").
-include("../records.hrl").

init() ->
    erlang:load_nif(filename:join(?PrivDirFiltering, ?LIBNAME), 0).

%% Example => box_filter:apply("234.jpg", -1, 5, 5, -1, -1, 1, 4).
apply(_ImagePath, _Ddepth, _KsizeWidth, _KsizeHeight, _AnchorX, _AnchorY, _Normalize, _BorderType) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).
