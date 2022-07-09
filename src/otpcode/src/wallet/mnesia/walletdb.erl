-module(walletdb).
-export([insert/2, get_wallet/1, get_wallets/0, get_password/1]). 

-include("../wallet.hrl").
-include_lib("stdlib/include/qlc.hrl").


insert(Name, Password) ->
    F = fun() ->
        Address = uuid:generate(), 
        Balance = 0,
        {Pub_key, Priv_key} = crypto_utils:generate_key_pair(),
        mnesia:write(#wallet{name = Name,
                             password = Password,
                             address = Address,
                             balance = Balance,
                             pub_key = Pub_key,
                             priv_key = Priv_key}),
        Address
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





