-module(ae_walletdb).
-export([insert/1, generate_address/0]).

-include("../../records.hrl").

-define(WALLET_LENGTH, 40).

insert(Size) ->
    F = fun() ->
            Mnemonic = ebip39:generate_mnemonic(Size),
            {Pub_key, Priv_key} = crypto_utils:generate_key_pair(),
            Address = generate_address(),
            mnesia:write(#ae_wallet{address = [Address],
                            mnemonic = Mnemonic(Size),
                            balance = 0,
                            pub_key = Pub_key,
                            priv_key = Priv_key})
        end,
    {atomic, Res} = mnesia:transaction(F),
    Res.

generate_address() ->
  Rand = crypto:strong_rand_bytes(?WALLET_LENGTH),
  Hash = crypto:hash(sha256, Rand),
  base58:binary_to_base58(Hash).

