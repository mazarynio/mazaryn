-module(eventdb).
-author("Zaryn Technologies").
-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([insert/4, get_all/0, delete/1]).

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