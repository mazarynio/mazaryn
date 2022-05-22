-module(walletdb).
-export([init/0, insert/2]).

-record(wallet, {name, password, address, balance, pub_key, priv_key}).

init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(wallet, 
                    [{attributes, record_info(fields, wallet)}]).

insert(Name, Password) ->
    Address = crypto_utils:generate_key_pair(),
    Balance = 0,
    Pub_key = uuid:generate(),
    Priv_key = uuid:get_rand_string(),
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





