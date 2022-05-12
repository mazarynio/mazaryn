-module(eventdb).
-compile([export_all, nowarn_export_all]).
-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(event, [{attributes, record_info(fields, event)},
            {disc_copies, [node()]}]).

insert(Name, Date, Loc, Desc) ->
    Event = #event{name = Name, date = Date, loc = Loc, desc = Desc},
    F = fun() ->
        mnesia:write(Event)
    end,
    mnesia:transaction(F).

get_all() ->
    Fun = fun() ->
        qlc:eval(qlc:q([X || X <- mnesia:table(event)]))
    end,
    {atomic, Details} = mnesia:transaction(Fun),
    Details.

retrieve_per_date(Date) ->
    Fun = fun() ->
        Result = qlc:eval(qlc:q([X || X <- mnesia:table(event),
                    X#event.date =:= Date])),
        lists:map(fun(Item) -> {Item#event.loc, Item#event.desc} end, Result)
        end,
    {atomic, Details} = mnesia:transaction(Fun),
    Details.

todays_event() ->
    Fun = fun() ->
        qlc:eavl(qlc:q([X || X <- mnesia:table(event),
                X#event.date =:= erlang:date()]))
        end,
    {atomic, Details} = mnesia:transaction(Fun),
    Details.

delete(Date) ->
    Fun = fun() ->
        Result = qlc:eval(qlc:q([X || X <- mnesia:table(event),
                        X#event.date =:= Date])),
            Fun1 = fun() ->
                lists:foreach(fun(Item) ->
                    mnesia:delete_object(Item)
                end, Result)
            end,
        mnesia:transaction(Fun1)
    end,
    mnesia:transaction(Fun).