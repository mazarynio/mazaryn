-module(walletdb).
-export([insert/2, get_wallet/1, get_wallets/0, get_password/1,
         get_address/1,
         deposit/2, withdraw/2, generate_new_address/1]).

-include("../../records.hrl").

insert(Name, Password) ->
    F = fun() ->
          {Pub_key, Priv_key} = crypto_utils:generate_key_pair(),
          Address = base58:binary_to_base58(Pub_key),
          mnesia:write(#wallet{name = Name,
                               password = erlpass:hash(Password),
                               address = [Address],
                               balance = 0,
                               pub_key = Pub_key,
                               priv_key = Priv_key}),
          Pub_key
        end,
    {atomic, Res} = mnesia:transaction(F),
    Res.

generate_new_address(PubKey) ->
  Fun = fun() ->
          [Wallet] = mnesia:read({wallet, PubKey}),
          CurrentAddress = Wallet#wallet.address,
          NewAddress = base58:binary_to_base58(PubKey),
          mnesia:write(Wallet#wallet{address = [NewAddress|CurrentAddress]}),
          NewAddress
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

get_address(PubKey) ->
  Fun = fun() ->
          [Wallet] = mnesia:read({wallet, PubKey}),
          Wallet#wallet.address
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

get_wallet(PubKey) ->
    {atomic, [Wallet]} = mnesia:transaction(fun() ->
                                              mnesia:read({wallet, PubKey})
                                            end),
    Wallet.

get_wallets() ->
    Fun = fun() ->
        mnesia:all_keys(wallet)
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_password(PubKey) ->
    F = fun() ->
        mnesia:read(wallet, PubKey)
        end,
    Res = mnesia:transaction(F),
    case Res of
      {atomic, [Wallet]} -> Wallet#wallet.password;
      {atomic, []} -> wallet_not_existed;
      _ -> error
    end.


deposit(PubKey, Amount) ->
  Fun = fun() ->
          [Wallet] = mnesia:read({wallet, PubKey}),
          mnesia:write(Wallet#wallet{balance = Wallet#wallet.balance + Amount})
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

withdraw(PubKey, Amount) ->
  Fun = fun() ->
          [Wallet] = mnesia:read({wallet, PubKey}),
          CurrentBalance = Wallet#wallet.balance,
          case Amount =< CurrentBalance of
              true ->
                mnesia:write(Wallet#wallet{balance = CurrentBalance - Amount});
              false ->
                ?MSG_INSUFFICIENT_FUNDS
          end
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

