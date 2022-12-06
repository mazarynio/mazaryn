-module(hedera_walletdb).
-export([insert/1, get_wallet_by_id/1, get_wallets/0]).

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

get_wallet_by_id(Id) ->
  Res = mnesia:transaction(
          fun() ->
              mnesia:match_object(#hed_wallet{id = Id, _= '_'})
          end),
  case Res of
    {atomic, []} -> wallet_not_exist;
    {atomic, [Wallet]} -> Wallet;
    _ -> error
  end.


get_wallets() ->
    Fun = fun() ->
            mnesia:all_keys(hed_wallet)
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.
