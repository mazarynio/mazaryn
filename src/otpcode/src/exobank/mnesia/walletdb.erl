-module(walletdb).
-export([insert/1]).

-record(wallet, {name, address, balance, pub_key, priv_key}).

insert(Name) ->
    Address = crypto_utils:generate_key_pair(),
    Balance = 0,
    Pub_key = uuid:generate(),
    Priv_key = uuid:get_rand_string(),
    Wallet = #wallet{name = Name, 
                     address = Address,
                     balance = Balance,
                     pub_key = Pub_key,
                     priv_key = Priv_key},
    F = fun() ->
        mnesia:write(Wallet)
    end,
    mnesia:transaction(F).



