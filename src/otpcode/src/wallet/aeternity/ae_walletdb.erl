-module(ae_walletdb).
-export([insert/4, generate_new_address/1, get_address/1,
 get_wallet/1, get_wallets/0, get_password/1, deposit/2, withdraw/2, generate_address/0,
 delete_wallet/1]).

-include("../../records.hrl").

-define(WALLET_LENGTH, 40).

insert(Name, Password, Size, Token) ->
    F = fun() ->
          Mnemonic = ebip39:generate_mnemonic(Size),
          Names = mnesia:all_keys(wallet),
          case lists:member(Name, Names) of
            true ->
              wallet_name_existing;
            false ->
              {Pub_key, Priv_key} = crypto_utils:generate_key_pair(),
              Address = generate_address(),
              mnesia:write(#ae_wallet{name = Name,
                               password = erlpass:hash(Password),
                               token = Token,
                               address = [Address],
                               mnemonic = Mnemonic,
                               balance = 0,
                               pub_key = Pub_key,
                               priv_key = Priv_key})
          end
        end,
    {atomic, Res} = mnesia:transaction(F),
    Res.

generate_new_address(Name) ->
  Fun = fun() ->
          [Wallet] = mnesia:read({wallet, Name}),
          NewAddress = generate_address(),
          mnesia:write(Wallet#wallet{address = [NewAddress|Wallet#wallet.address]}),
          NewAddress
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

get_address(Name) ->
  Fun = fun() ->
          [Wallet] = mnesia:read({wallet, Name}),
          Wallet#wallet.address
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

get_wallet(Name) ->
    {atomic, [Wallet]} = mnesia:transaction(fun() ->
                                              mnesia:read({wallet, Name})
                                            end),
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

withdraw(Name, Amount) ->
  Fun = fun() ->
          [Wallet] = mnesia:read({wallet, Name}),
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

generate_address() ->
  Rand = crypto:strong_rand_bytes(?WALLET_LENGTH),
  Hash = crypto:hash(sha256, Rand),
  base58:binary_to_base58(Hash).

delete_wallet(Name) ->
    F = fun() ->
        [Wallet] = mnesia:match_object(#ae_wallet{name = Name, _= '_'}),
        mnesia:delete_object(Wallet)
    end,
    mnesia:activity(transaction, F).

