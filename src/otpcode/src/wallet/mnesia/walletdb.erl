-module(walletdb).
-export([init/0, insert/2]).

-include("../wallet.hrl").

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





