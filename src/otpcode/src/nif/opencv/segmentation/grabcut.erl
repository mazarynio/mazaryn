-module(grabcut).
-author("Zaryn Technologies").
-export([init/0, grabcut_segment/5]).
-on_load(init/0).
-define(LIBNAME, "libgrabcut").
-include("../records.hrl").

init() ->
    erlang:load_nif(filename:join(?PrivDirSegment, ?LIBNAME), 0).

%% Example => grabcut:grabcut_segment("234.jpg", 50, 50, 200, 200).
grabcut_segment(_ImagePath, _X, _Y, _Width, _Height) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).
