-module(walletdb).
-include("../wallet.hrl").

-export([init/0, insert/2, get_wallet/1, get_wallets/0, get_password/1]).

-define(CURVE_NAME, eddsa).
-define(CURVE_PARAM, ed25519).  

init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(wallet, 
                    [{attributes, record_info(fields, wallet)}]).

insert(Name, Password) ->
    Address = crypto_utils:generate_key_pair(),
    Balance = 0,
    {Pub_key, Priv_key} = libp2p_crypto:generate_keys(ecc_compact),
    Wallet = #wallet{name = Name,
                     password = Password, 
                     address = Address,
                     balance = Balance,
                     pub_key = Pub_key,
                     priv_key = Priv_key},
    F = fun() ->
        mnesia:write(Wallet)
    end,
    mnesia:transaction(F).

get_wallet(Name) ->
    F = fun() ->
        mnesia:read(wallet, Name)
    end,
    Res = mnesia:transaction(F),
    case Res of 
        {atomic, [Wallet]} -> Wallet;
        {atomic, []} -> not_exist;
        _ -> error 
    end.

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





