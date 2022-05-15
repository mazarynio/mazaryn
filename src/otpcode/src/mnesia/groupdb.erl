-module(groupdb).
-compile([export_all, nowarn_export_all]).
-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

insert(Gp_name, Members) ->
    Id = id_gen:generate(),
    Group = #group{id = Id, gp_name = Gp_name, members = Members},
    F = fun() ->
        mnesia:write(Group)
    end,
    mnesia:transaction(F).