-module(watershed).
-author("Zaryn Technologies").
-export([init/0, watershed_segment/2]).
-on_load(init/0).
-define(LIBNAME, "libwatershed").
-include("../records.hrl").

init() ->
    erlang:load_nif(filename:join(?PrivDirSegment, ?LIBNAME), 0).

%% Rectangles = [{50, 50, 200, 200}, {300, 300, 100, 100}].
%% watershed:watershed_segment("234.jpg", Rectangles).
watershed_segment(_ImagePath, _Marker) ->
    erlang:nif_error({not_loaded, ?LIBNAME}).
