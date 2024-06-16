-module(calc_back_project).
-author("Zaryn Technologies").
-export([init/0, apply/7]).
-on_load(init/0).
-define(LIBNAME, "libcalc_back_project").
-include("../records.hrl").

init() ->
    erlang:load_nif(filename:join(?PrivDirCalcBack, ?LIBNAME), 0).

%% Example => calc_back_project:apply("image.jpg", "hist.xml", 0, 1.0, true, [0.0, 256.0], "output.jpg").
apply(_ImagePath, _HistPath, _Channels, _Scale, _Uniform, _Ranges, _OutputPath) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).
