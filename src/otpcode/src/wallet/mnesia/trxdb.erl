-module(trxdb).
-export([init/0, insert/0]).

-include("../wallet.hrl").
-import(uuid, [generate/0]).

init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(wallet, 
                    [{attributes, record_info(fields, transaction)}]).

insert() ->
    Id = uuid:generate(),
    Transaction = #transaction{id = Id},
    F = fun() ->
        mnesia:write(Transaction)
    end,
    mnesia:transaction(F).
