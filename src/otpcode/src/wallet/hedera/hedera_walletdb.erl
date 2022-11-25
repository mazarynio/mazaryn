-module(hedera_walletdb).
-export([insert/1]).

-include("../../records.hrl").

insert(Password) ->
  Fun = fun() ->
    Now = calendar:universal_time(),
    Id = id_gen:generate(),
    mnesia:write(#hed_wallet{id = Id,
                             password = Password,
                             date_created = Now}),
    Id
  end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.
