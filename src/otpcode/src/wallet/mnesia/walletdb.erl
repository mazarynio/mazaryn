-module(walletdb).
-export([insert/2, get_wallet/1, get_wallets/0, get_password/1,
         deposit/2]).

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

get_wallet(Address) ->
    {atomic, [Wallet]} = mnesia:transaction(fun() -> mnesia:read({wallet, Address}) end),
    Wallet.

get_wallets() ->
    Fun = fun() ->
        mnesia:all_keys(wallet)
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_password(Name) ->
    F = fun() ->
        mnesia:read(wallet, Name)
        end,
    Res = mnesia:transaction(F),
    case Res of
      {atomic, [Wallet]} -> Wallet#wallet.password;
      {atomic, []} -> wallet_not_existed;
      _ -> error
    end.

deposit(Name, Amount) ->
  Fun = fun() ->
          [Wallet] = mnesia:read({wallet, Name}),
          mnesia:write(Wallet#wallet{balance = Wallet#wallet.balance + Amount})
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.



