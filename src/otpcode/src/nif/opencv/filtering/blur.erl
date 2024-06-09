-module(blur).
-author("Zaryn Technologies").
-export([init/0, apply/5]).
-on_load(init/0).
-define(LIBNAME, "libblur").
-include("../records.hrl").

init() ->
    erlang:load_nif(filename:join(?PrivDirFiltering, ?LIBNAME), 0).

%% Example => blur:apply("234.jpg", 5, 5, -1, -1).
apply(_ImagePath, _KsizeWidth, _KsizeHeight, _AnchorX, _AnchorY) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).
