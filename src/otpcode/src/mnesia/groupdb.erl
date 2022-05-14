-module(groupdb).
-compile([export_all, nowarn_export_all]).
-include("../include/records.hrl").
-include_lib("stdlib/include/qlc.hrl").

init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(group, [{attributes, record_info(fields, group)},
            {disc_copies, [node()]}]).

reset_db() ->
    mnesia:clear_table(group).

insert(Gp_name, Members) ->
    Id = id_gen:generate(),
    Group = #group{id = Id, gp_name = Gp_name, members = Members},
    F = fun() ->
        mnesia:write(Group)
    end,
    mnesia:transaction(F).