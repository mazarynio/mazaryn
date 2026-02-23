-module(near_walletdb).
-author("Zaryn Technologies").
-include("../records.hrl").
-include("../wallet_records.hrl").
-export([
    create_wallet/5,
    create_wallet/6,
    get_wallet/1,
    get_wallet_by_account_id/1,
    get_user_wallets/1,
    get_primary_wallet/1,
    set_primary_wallet/2,
    update_wallet_last_used/1,
    update_wallet_label/2,
    delete_wallet/1,
    wallet_exists/1,
    account_id_exists/1,

    create_transaction/2,
    get_transaction/1,
    get_wallet_transactions/1,
    get_wallet_transactions/3,
    get_user_transactions/1,
    get_transactions_by_type/2,

    create_access_key/2,
    get_access_key/1,
    get_wallet_access_keys/1,
    get_user_access_keys/1,
    delete_access_key/1,

    create_stake/2,
    get_stake/1,
    get_wallet_stakes/1,
    get_user_stakes/1,
    update_stake/2,

    create_implicit_account/2,
    get_implicit_account/1,
    get_implicit_account_by_account_id/1,
    get_wallet_implicit_accounts/1,
    mark_implicit_funded/2,

    create_social_post/2,
    get_social_post/1,
    get_wallet_social_posts/1,
    get_user_social_posts/1
]).

create_wallet(UserId, AccountId, EncryptedPrivateKey, IV, Tag) ->
    create_wallet(UserId, AccountId, EncryptedPrivateKey, IV, Tag, undefined).

create_wallet(UserId, AccountId, EncryptedPrivateKey, IV, Tag, Label) ->
    Fun = fun() ->
        case account_id_exists_internal(AccountId) of
            true ->
                {error, account_id_already_exists};
            false ->
                WalletId = nanoid:gen(),
                Now = calendar:universal_time(),
                IsPrimary = case get_user_wallets_internal(UserId) of
                    [] -> true;
                    _ -> false
                end,
                Wallet = #near_wallet{
                    wallet_id = WalletId,
                    user_id = UserId,
                    label = Label,
                    account_id = AccountId,
                    encrypted_private_key = EncryptedPrivateKey,
                    encryption_iv = IV,
                    encryption_tag = Tag,
                    network = get_network(),
                    created_at = Now,
                    last_used = Now,
                    is_primary = IsPrimary,
                    metadata = #{}
                },
                mnesia:write(Wallet),
                {ok, WalletId}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet(WalletId) ->
    Fun = fun() ->
        case mnesia:read(near_wallet, WalletId) of
            [] -> {error, wallet_not_found};
            [Wallet] -> {ok, Wallet}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet_by_account_id(AccountId) ->
    Fun = fun() ->
        case mnesia:index_read(near_wallet, AccountId, #near_wallet.account_id) of
            [] -> {error, wallet_not_found};
            [Wallet | _] -> {ok, Wallet}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_user_wallets(UserId) ->
    Fun = fun() ->
        Wallets = mnesia:index_read(near_wallet, UserId, #near_wallet.user_id),
        lists:sort(fun(A, B) ->
            if
                A#near_wallet.is_primary -> true;
                B#near_wallet.is_primary -> false;
                true -> A#near_wallet.created_at >= B#near_wallet.created_at
            end
        end, Wallets)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Wallets} -> {ok, Wallets};
        {aborted, Reason} -> {error, Reason}
    end.

get_primary_wallet(UserId) ->
    Fun = fun() ->
        case mnesia:index_read(near_wallet, UserId, #near_wallet.user_id) of
            [] -> {error, no_wallets_found};
            Wallets ->
                case lists:filter(fun(W) -> W#near_wallet.is_primary end, Wallets) of
                    [] ->
                        [First | _] = lists:sort(
                            fun(A, B) -> A#near_wallet.created_at =< B#near_wallet.created_at end,
                            Wallets
                        ),
                        {ok, First};
                    [Primary | _] -> {ok, Primary}
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

set_primary_wallet(UserId, WalletId) ->
    Fun = fun() ->
        case mnesia:read(near_wallet, WalletId) of
            [] -> {error, wallet_not_found};
            [Wallet] ->
                case Wallet#near_wallet.user_id =:= UserId of
                    false -> {error, access_denied};
                    true ->
                        AllWallets = get_user_wallets_internal(UserId),
                        lists:foreach(fun(W) ->
                            if
                                W#near_wallet.wallet_id =:= WalletId ->
                                    mnesia:write(W#near_wallet{is_primary = true});
                                W#near_wallet.is_primary =:= true ->
                                    mnesia:write(W#near_wallet{is_primary = false});
                                true ->
                                    ok
                            end
                        end, AllWallets),
                        ok
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

update_wallet_last_used(WalletId) ->
    Fun = fun() ->
        case mnesia:read(near_wallet, WalletId) of
            [] -> {error, wallet_not_found};
            [Wallet] ->
                mnesia:write(Wallet#near_wallet{last_used = calendar:universal_time()}),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

update_wallet_label(WalletId, NewLabel) ->
    Fun = fun() ->
        case mnesia:read(near_wallet, WalletId) of
            [] -> {error, wallet_not_found};
            [Wallet] ->
                mnesia:write(Wallet#near_wallet{label = NewLabel}),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

delete_wallet(WalletId) ->
    Fun = fun() ->
        case mnesia:read(near_wallet, WalletId) of
            [] -> {error, wallet_not_found};
            [Wallet] ->
                mnesia:delete({near_wallet, WalletId}),
                Remaining = lists:filter(
                    fun(W) -> W#near_wallet.wallet_id =/= WalletId end,
                    get_user_wallets_internal(Wallet#near_wallet.user_id)
                ),
                case Wallet#near_wallet.is_primary of
                    true ->
                        case Remaining of
                            [] -> ok;
                            [Next | _] -> mnesia:write(Next#near_wallet{is_primary = true})
                        end;
                    false -> ok
                end,
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

wallet_exists(WalletId) ->
    Fun = fun() ->
        case mnesia:read(near_wallet, WalletId) of
            [] -> false;
            [_] -> true
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, _} -> false
    end.

account_id_exists(AccountId) ->
    Fun = fun() ->
        account_id_exists_internal(AccountId)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, _} -> false
    end.

create_transaction(WalletId, TxData) ->
    Fun = fun() ->
        case mnesia:read(near_wallet, WalletId) of
            [] -> {error, wallet_not_found};
            [Wallet] ->
                TxId = nanoid:gen(),
                Now = calendar:universal_time(),
                Tx = #near_transaction{
                    tx_id = TxId,
                    wallet_id = WalletId,
                    user_id = Wallet#near_wallet.user_id,
                    transaction_hash = maps:get(transaction_hash, TxData, undefined),
                    tx_type = maps:get(tx_type, TxData),
                    from_account_id = maps:get(from_account_id, TxData, undefined),
                    receiver_id = maps:get(receiver_id, TxData, undefined),
                    amount_near = maps:get(amount_near, TxData, undefined),
                    contract_id = maps:get(contract_id, TxData, undefined),
                    method_name = maps:get(method_name, TxData, undefined),
                    status = maps:get(status, TxData, confirmed),
                    actions_count = maps:get(actions_count, TxData, 1),
                    error_message = maps:get(error_message, TxData, undefined),
                    created_at = Now,
                    metadata = maps:get(metadata, TxData, #{})
                },
                mnesia:write(Tx),
                {ok, TxId}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_transaction(TxId) ->
    Fun = fun() ->
        case mnesia:read(near_transaction, TxId) of
            [] -> {error, transaction_not_found};
            [Tx] -> {ok, Tx}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet_transactions(WalletId) ->
    Fun = fun() ->
        Txs = mnesia:index_read(near_transaction, WalletId, #near_transaction.wallet_id),
        lists:sort(fun(A, B) ->
            A#near_transaction.created_at >= B#near_transaction.created_at
        end, Txs)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Txs} -> {ok, Txs};
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet_transactions(WalletId, Limit, Offset) ->
    Fun = fun() ->
        All = mnesia:index_read(near_transaction, WalletId, #near_transaction.wallet_id),
        Sorted = lists:sort(fun(A, B) ->
            A#near_transaction.created_at >= B#near_transaction.created_at
        end, All),
        Page = lists:sublist(Sorted, Offset + 1, Limit),
        {Page, length(All)}
    end,
    case mnesia:transaction(Fun) of
        {atomic, {Txs, Total}} -> {ok, Txs, Total};
        {aborted, Reason} -> {error, Reason}
    end.

get_user_transactions(UserId) ->
    Fun = fun() ->
        Txs = mnesia:index_read(near_transaction, UserId, #near_transaction.user_id),
        lists:sort(fun(A, B) ->
            A#near_transaction.created_at >= B#near_transaction.created_at
        end, Txs)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Txs} -> {ok, Txs};
        {aborted, Reason} -> {error, Reason}
    end.

get_transactions_by_type(WalletId, TxType) ->
    Fun = fun() ->
        All = mnesia:index_read(near_transaction, WalletId, #near_transaction.wallet_id),
        Filtered = lists:filter(fun(Tx) ->
            Tx#near_transaction.tx_type =:= TxType
        end, All),
        lists:sort(fun(A, B) ->
            A#near_transaction.created_at >= B#near_transaction.created_at
        end, Filtered)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Txs} -> {ok, Txs};
        {aborted, Reason} -> {error, Reason}
    end.

create_access_key(WalletId, KeyData) ->
    Fun = fun() ->
        case mnesia:read(near_wallet, WalletId) of
            [] -> {error, wallet_not_found};
            [Wallet] ->
                KeyId = nanoid:gen(),
                Now = calendar:universal_time(),
                Key = #near_access_key{
                    key_id = KeyId,
                    wallet_id = WalletId,
                    user_id = Wallet#near_wallet.user_id,
                    public_key = maps:get(public_key, KeyData),
                    key_type = maps:get(key_type, KeyData, full_access),
                    contract_id = maps:get(contract_id, KeyData, undefined),
                    method_names = maps:get(method_names, KeyData, []),
                    allowance_near = maps:get(allowance_near, KeyData, undefined),
                    label = maps:get(label, KeyData, undefined),
                    created_at = Now,
                    metadata = maps:get(metadata, KeyData, #{})
                },
                mnesia:write(Key),
                {ok, KeyId}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_access_key(KeyId) ->
    Fun = fun() ->
        case mnesia:read(near_access_key, KeyId) of
            [] -> {error, key_not_found};
            [Key] -> {ok, Key}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet_access_keys(WalletId) ->
    Fun = fun() ->
        Keys = mnesia:index_read(near_access_key, WalletId, #near_access_key.wallet_id),
        lists:sort(fun(A, B) ->
            A#near_access_key.created_at >= B#near_access_key.created_at
        end, Keys)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Keys} -> {ok, Keys};
        {aborted, Reason} -> {error, Reason}
    end.

get_user_access_keys(UserId) ->
    Fun = fun() ->
        Keys = mnesia:index_read(near_access_key, UserId, #near_access_key.user_id),
        lists:sort(fun(A, B) ->
            A#near_access_key.created_at >= B#near_access_key.created_at
        end, Keys)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Keys} -> {ok, Keys};
        {aborted, Reason} -> {error, Reason}
    end.

delete_access_key(KeyId) ->
    Fun = fun() ->
        case mnesia:read(near_access_key, KeyId) of
            [] -> {error, key_not_found};
            [_] ->
                mnesia:delete({near_access_key, KeyId}),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

create_stake(WalletId, StakeData) ->
    Fun = fun() ->
        case mnesia:read(near_wallet, WalletId) of
            [] -> {error, wallet_not_found};
            [Wallet] ->
                StakeId = nanoid:gen(),
                Now = calendar:universal_time(),
                Stake = #near_stake{
                    stake_id = StakeId,
                    wallet_id = WalletId,
                    user_id = Wallet#near_wallet.user_id,
                    validator_account_id = maps:get(validator_account_id, StakeData, undefined),
                    validator_public_key = maps:get(validator_public_key, StakeData, undefined),
                    amount_near = maps:get(amount_near, StakeData),
                    status = maps:get(status, StakeData, staked),
                    transaction_hash = maps:get(transaction_hash, StakeData),
                    created_at = Now,
                    unstaked_at = maps:get(unstaked_at, StakeData, undefined),
                    withdrawn_at = maps:get(withdrawn_at, StakeData, undefined),
                    metadata = maps:get(metadata, StakeData, #{})
                },
                mnesia:write(Stake),
                {ok, StakeId}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_stake(StakeId) ->
    Fun = fun() ->
        case mnesia:read(near_stake, StakeId) of
            [] -> {error, stake_not_found};
            [Stake] -> {ok, Stake}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet_stakes(WalletId) ->
    Fun = fun() ->
        Stakes = mnesia:index_read(near_stake, WalletId, #near_stake.wallet_id),
        lists:sort(fun(A, B) ->
            A#near_stake.created_at >= B#near_stake.created_at
        end, Stakes)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Stakes} -> {ok, Stakes};
        {aborted, Reason} -> {error, Reason}
    end.

get_user_stakes(UserId) ->
    Fun = fun() ->
        Stakes = mnesia:index_read(near_stake, UserId, #near_stake.user_id),
        lists:sort(fun(A, B) ->
            A#near_stake.created_at >= B#near_stake.created_at
        end, Stakes)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Stakes} -> {ok, Stakes};
        {aborted, Reason} -> {error, Reason}
    end.

update_stake(StakeId, Updates) ->
    Fun = fun() ->
        case mnesia:read(near_stake, StakeId) of
            [] -> {error, stake_not_found};
            [Stake] ->
                Updated = Stake#near_stake{
                    status = maps:get(status, Updates, Stake#near_stake.status),
                    unstaked_at = maps:get(unstaked_at, Updates, Stake#near_stake.unstaked_at),
                    withdrawn_at = maps:get(withdrawn_at, Updates, Stake#near_stake.withdrawn_at),
                    metadata = maps:get(metadata, Updates, Stake#near_stake.metadata)
                },
                mnesia:write(Updated),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

create_implicit_account(WalletId, ImplicitData) ->
    Fun = fun() ->
        case mnesia:read(near_wallet, WalletId) of
            [] -> {error, wallet_not_found};
            [Wallet] ->
                ImplicitId = nanoid:gen(),
                Now = calendar:universal_time(),
                Implicit = #near_implicit_account{
                    implicit_id = ImplicitId,
                    wallet_id = WalletId,
                    user_id = Wallet#near_wallet.user_id,
                    account_id = maps:get(account_id, ImplicitData),
                    encrypted_private_key = maps:get(encrypted_private_key, ImplicitData),
                    encryption_iv = maps:get(encryption_iv, ImplicitData),
                    encryption_tag = maps:get(encryption_tag, ImplicitData),
                    funded = maps:get(funded, ImplicitData, false),
                    created_at = Now,
                    funded_at = maps:get(funded_at, ImplicitData, undefined),
                    metadata = maps:get(metadata, ImplicitData, #{})
                },
                mnesia:write(Implicit),
                {ok, ImplicitId}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_implicit_account(ImplicitId) ->
    Fun = fun() ->
        case mnesia:read(near_implicit_account, ImplicitId) of
            [] -> {error, implicit_account_not_found};
            [Implicit] -> {ok, Implicit}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_implicit_account_by_account_id(AccountId) ->
    Fun = fun() ->
        case mnesia:index_read(near_implicit_account, AccountId, #near_implicit_account.account_id) of
            [] -> {error, implicit_account_not_found};
            [Implicit | _] -> {ok, Implicit}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet_implicit_accounts(WalletId) ->
    Fun = fun() ->
        Accounts = mnesia:index_read(near_implicit_account, WalletId, #near_implicit_account.wallet_id),
        lists:sort(fun(A, B) ->
            A#near_implicit_account.created_at >= B#near_implicit_account.created_at
        end, Accounts)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Accounts} -> {ok, Accounts};
        {aborted, Reason} -> {error, Reason}
    end.

mark_implicit_funded(ImplicitId, FundedAt) ->
    Fun = fun() ->
        case mnesia:read(near_implicit_account, ImplicitId) of
            [] -> {error, implicit_account_not_found};
            [Implicit] ->
                mnesia:write(Implicit#near_implicit_account{
                    funded = true,
                    funded_at = FundedAt
                }),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

create_social_post(WalletId, PostData) ->
    Fun = fun() ->
        case mnesia:read(near_wallet, WalletId) of
            [] -> {error, wallet_not_found};
            [Wallet] ->
                PostId = nanoid:gen(),
                Now = calendar:universal_time(),
                Post = #near_social_post{
                    post_id = PostId,
                    wallet_id = WalletId,
                    user_id = Wallet#near_wallet.user_id,
                    account_id = Wallet#near_wallet.account_id,
                    contract = maps:get(contract, PostData),
                    text = maps:get(text, PostData),
                    media_urls = maps:get(media_urls, PostData, []),
                    tags = maps:get(tags, PostData, []),
                    transaction_hash = maps:get(transaction_hash, PostData),
                    block_height = maps:get(block_height, PostData, undefined),
                    created_at = Now,
                    metadata = maps:get(metadata, PostData, #{})
                },
                mnesia:write(Post),
                {ok, PostId}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_social_post(PostId) ->
    Fun = fun() ->
        case mnesia:read(near_social_post, PostId) of
            [] -> {error, post_not_found};
            [Post] -> {ok, Post}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet_social_posts(WalletId) ->
    Fun = fun() ->
        Posts = mnesia:index_read(near_social_post, WalletId, #near_social_post.wallet_id),
        lists:sort(fun(A, B) ->
            A#near_social_post.created_at >= B#near_social_post.created_at
        end, Posts)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Posts} -> {ok, Posts};
        {aborted, Reason} -> {error, Reason}
    end.

get_user_social_posts(UserId) ->
    Fun = fun() ->
        Posts = mnesia:index_read(near_social_post, UserId, #near_social_post.user_id),
        lists:sort(fun(A, B) ->
            A#near_social_post.created_at >= B#near_social_post.created_at
        end, Posts)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Posts} -> {ok, Posts};
        {aborted, Reason} -> {error, Reason}
    end.

get_user_wallets_internal(UserId) ->
    mnesia:index_read(near_wallet, UserId, #near_wallet.user_id).

account_id_exists_internal(AccountId) ->
    case mnesia:index_read(near_wallet, AccountId, #near_wallet.account_id) of
        [] -> false;
        [_ | _] -> true
    end.

get_network() ->
    case application:get_env(mazaryn, near_network, testnet) of
        testnet -> testnet;
        mainnet -> mainnet;
        Other -> Other
    end.
