-module(hedera_walletdb).
-export([insert/1]).

-include("../../records.hrl").

insert(Password) ->
  Fun = fun() ->
    Now = calendar:universal_time(),
    Id = id_gen:generate(),
    Hed_wallet = #hed_wallet{id = Id,
                        password = erlpass:hash(Password),
                        date_created = Now},
    mnesia:write(Hed_wallet),
    Id
  end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.
