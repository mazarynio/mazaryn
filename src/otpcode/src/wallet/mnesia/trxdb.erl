-module(trxdb).
-export([insert/0]).

-include("../../records.hrl").
-import(uuid, [generate/0]).

insert() ->
    Id = uuid:generate(),
    Transaction = #transaction{id = Id},
    F = fun() ->
        mnesia:write(Transaction)
    end,
    mnesia:transaction(F).
