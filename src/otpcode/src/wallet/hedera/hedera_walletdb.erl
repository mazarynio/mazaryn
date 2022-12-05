-module(hedera_walletdb).
-export([insert/1, get_wallets/0]).

-include("../../records.hrl").

insert(Password) ->
  Fun = fun() ->
    Now = calendar:universal_time(),
    Id = id_gen:generate(),
    mnesia:write(#hed_wallet{id = Id,
                             password = erlpass:hash(Password),
                             date_created = Now}),
    Id
  end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

get_wallets() ->
    Fun = fun() ->
            mnesia:all_keys(hed_wallet)
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.
