-module(aptos_walletdb).
-author("Zaryn Technologies").
-include("../records.hrl").
-include("../wallet_records.hrl").
-export([
    create_wallet/5,
    create_wallet/6,
    create_wallet/7,
    create_wallet/8,
    get_wallet/1,
    get_wallet_by_address/1,
    get_user_wallets/1,
    get_primary_wallet/1,
    set_primary_wallet/2,
    update_wallet_last_used/1,
    update_wallet_label/2,
    delete_wallet/1,
    wallet_exists/1,
    address_exists/1,

    create_transaction/2,
    get_transaction/1,
    get_transaction_by_hash/1,
    get_wallet_transactions/1,
    get_wallet_transactions/3,
    get_user_transactions/1,
    get_transactions_by_type/2,

    create_nft_collection/2,
    get_nft_collection/1,
    get_wallet_nft_collections/1,
    get_user_nft_collections/1,
    get_nft_collection_by_name/2,

    create_nft_token/2,
    get_nft_token/1,
    get_wallet_nft_tokens/1,
    get_collection_nft_tokens/1,

    create_fungible_asset/2,
    get_fungible_asset/1,
    get_wallet_fungible_assets/1,
    get_user_fungible_assets/1,
    get_fungible_asset_by_metadata/1,

    create_stake/2,
    get_stake/1,
    get_wallet_stakes/1,
    get_user_stakes/1,
    get_stakes_by_pool/2,
    update_stake/2,
    delete_stake/1,

    upsert_coin_balance/2,
    get_coin_balance/2,
    get_wallet_coin_balances/1,
    get_user_coin_balances/1,
    delete_coin_balance/1,

    upsert_fa_balance/2,
    get_fa_balance/2,
    get_wallet_fa_balances/1,
    get_user_fa_balances/1,
    delete_fa_balance/1
]).

create_wallet(UserId, Address, EncryptedPrivateKey, IV, Tag) ->
    create_wallet(UserId, Address, EncryptedPrivateKey, IV, Tag, undefined, get_network(), ed25519).

create_wallet(UserId, Address, EncryptedPrivateKey, IV, Tag, Label) ->
    create_wallet(UserId, Address, EncryptedPrivateKey, IV, Tag, Label, get_network(), ed25519).

create_wallet(UserId, Address, EncryptedPrivateKey, IV, Tag, Label, Network) ->
    create_wallet(UserId, Address, EncryptedPrivateKey, IV, Tag, Label, Network, ed25519).

create_wallet(UserId, Address, EncryptedPrivateKey, IV, Tag, Label, Network, Scheme) ->
    Fun = fun() ->
        case address_exists_internal(Address) of
            true ->
                {error, address_already_exists};
            false ->
                WalletId = nanoid:gen(),
                Now = calendar:universal_time(),
                IsPrimary = case get_user_wallets_internal(UserId) of
                    [] -> true;
                    _  -> false
                end,
                Wallet = #aptos_wallet{
                    wallet_id             = WalletId,
                    user_id               = UserId,
                    label                 = Label,
                    account_address       = Address,
                    encrypted_private_key = EncryptedPrivateKey,
                    encryption_iv         = IV,
                    encryption_tag        = Tag,
                    network               = Network,
                    signing_scheme        = Scheme,
                    is_legacy             = false,
                    created_at            = Now,
                    last_used             = Now,
                    is_primary            = IsPrimary,
                    metadata              = #{}
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
        case mnesia:read(aptos_wallet, WalletId) of
            []       -> {error, wallet_not_found};
            [Wallet] -> {ok, Wallet}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet_by_address(Address) ->
    Fun = fun() ->
        case mnesia:index_read(aptos_wallet, Address, #aptos_wallet.account_address) of
            []       -> {error, wallet_not_found};
            [Wallet | _] -> {ok, Wallet}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_user_wallets(UserId) ->
    Fun = fun() ->
        Wallets = mnesia:index_read(aptos_wallet, UserId, #aptos_wallet.user_id),
        Sorted = lists:sort(fun(A, B) ->
            if
                A#aptos_wallet.is_primary -> true;
                B#aptos_wallet.is_primary -> false;
                true -> A#aptos_wallet.created_at >= B#aptos_wallet.created_at
            end
        end, Wallets),
        {ok, Sorted}
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_primary_wallet(UserId) ->
    Fun = fun() ->
        case mnesia:index_read(aptos_wallet, UserId, #aptos_wallet.user_id) of
            [] ->
                {error, no_wallets_found};
            Wallets ->
                case lists:filter(fun(W) -> W#aptos_wallet.is_primary end, Wallets) of
                    [] ->
                        [First | _] = lists:sort(
                            fun(A, B) -> A#aptos_wallet.created_at =< B#aptos_wallet.created_at end,
                            Wallets
                        ),
                        {ok, First};
                    [Primary | _] ->
                        {ok, Primary}
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

set_primary_wallet(UserId, WalletId) ->
    Fun = fun() ->
        case mnesia:read(aptos_wallet, WalletId) of
            [] ->
                {error, wallet_not_found};
            [Wallet] ->
                case Wallet#aptos_wallet.user_id =:= UserId of
                    false ->
                        {error, access_denied};
                    true ->
                        AllWallets = get_user_wallets_internal(UserId),
                        lists:foreach(fun(W) ->
                            if
                                W#aptos_wallet.wallet_id =:= WalletId ->
                                    mnesia:write(W#aptos_wallet{is_primary = true});
                                W#aptos_wallet.is_primary =:= true ->
                                    mnesia:write(W#aptos_wallet{is_primary = false});
                                true ->
                                    ok
                            end
                        end, AllWallets),
                        {ok, WalletId}
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

update_wallet_last_used(WalletId) ->
    Fun = fun() ->
        case mnesia:read(aptos_wallet, WalletId) of
            [] ->
                {error, wallet_not_found};
            [Wallet] ->
                mnesia:write(Wallet#aptos_wallet{last_used = calendar:universal_time()}),
                {ok, WalletId}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

update_wallet_label(WalletId, NewLabel) ->
    Fun = fun() ->
        case mnesia:read(aptos_wallet, WalletId) of
            [] ->
                {error, wallet_not_found};
            [Wallet] ->
                mnesia:write(Wallet#aptos_wallet{label = NewLabel}),
                {ok, WalletId}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

delete_wallet(WalletId) ->
    Fun = fun() ->
        case mnesia:read(aptos_wallet, WalletId) of
            [] ->
                {error, wallet_not_found};
            [Wallet] ->
                mnesia:delete({aptos_wallet, WalletId}),
                Remaining = lists:filter(
                    fun(W) -> W#aptos_wallet.wallet_id =/= WalletId end,
                    get_user_wallets_internal(Wallet#aptos_wallet.user_id)
                ),
                case Wallet#aptos_wallet.is_primary of
                    true ->
                        case Remaining of
                            [] -> ok;
                            [Next | _] -> mnesia:write(Next#aptos_wallet{is_primary = true})
                        end;
                    false ->
                        ok
                end,
                {ok, deleted}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

wallet_exists(WalletId) ->
    Fun = fun() ->
        case mnesia:read(aptos_wallet, WalletId) of
            [] -> false;
            _  -> true
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, _}     -> false
    end.

address_exists(Address) ->
    Fun = fun() ->
        address_exists_internal(Address)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, _}     -> false
    end.

create_transaction(WalletId, TxData) ->
    Fun = fun() ->
        case mnesia:read(aptos_wallet, WalletId) of
            [] ->
                {error, wallet_not_found};
            [Wallet] ->
                TxId = nanoid:gen(),
                Now  = calendar:universal_time(),
                Tx = #aptos_transaction{
                    tx_id            = TxId,
                    wallet_id        = WalletId,
                    user_id          = Wallet#aptos_wallet.user_id,
                    transaction_hash = maps:get(transaction_hash, TxData, undefined),
                    tx_type          = maps:get(tx_type, TxData, transfer),
                    sender_address   = maps:get(sender_address, TxData, Wallet#aptos_wallet.account_address),
                    receiver_address = maps:get(receiver_address, TxData, undefined),
                    amount_octas     = maps:get(amount_octas, TxData, 0),
                    gas_used         = maps:get(gas_used, TxData, 0),
                    gas_unit_price   = maps:get(gas_unit_price, TxData, 100),
                    sequence_number  = maps:get(sequence_number, TxData, 0),
                    function_called  = maps:get(function_called, TxData, undefined),
                    coin_type        = maps:get(coin_type, TxData, undefined),
                    metadata_address = maps:get(metadata_address, TxData, undefined),
                    pool_address     = maps:get(pool_address, TxData, undefined),
                    collection_name  = maps:get(collection_name, TxData, undefined),
                    token_address    = maps:get(token_address, TxData, undefined),
                    status           = maps:get(status, TxData, confirmed),
                    vm_status        = maps:get(vm_status, TxData, undefined),
                    version          = maps:get(version, TxData, undefined),
                    error_message    = maps:get(error_message, TxData, undefined),
                    created_at       = Now,
                    metadata         = maps:get(metadata, TxData, #{})
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
        case mnesia:read(aptos_transaction, TxId) of
            []   -> {error, transaction_not_found};
            [Tx] -> {ok, Tx}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_transaction_by_hash(TxHash) ->
    Fun = fun() ->
        case mnesia:index_read(aptos_transaction, TxHash, #aptos_transaction.transaction_hash) of
            []     -> {error, transaction_not_found};
            [Tx|_] -> {ok, Tx}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet_transactions(WalletId) ->
    Fun = fun() ->
        Txs = mnesia:index_read(aptos_transaction, WalletId, #aptos_transaction.wallet_id),
        Sorted = lists:sort(fun(A, B) ->
            A#aptos_transaction.created_at >= B#aptos_transaction.created_at
        end, Txs),
        {ok, Sorted}
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet_transactions(WalletId, Limit, Offset) ->
    Fun = fun() ->
        All = mnesia:index_read(aptos_transaction, WalletId, #aptos_transaction.wallet_id),
        Sorted = lists:sort(fun(A, B) ->
            A#aptos_transaction.created_at >= B#aptos_transaction.created_at
        end, All),
        Page = lists:sublist(Sorted, Offset + 1, Limit),
        {ok, Page, length(All)}
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_user_transactions(UserId) ->
    Fun = fun() ->
        Txs = mnesia:index_read(aptos_transaction, UserId, #aptos_transaction.user_id),
        Sorted = lists:sort(fun(A, B) ->
            A#aptos_transaction.created_at >= B#aptos_transaction.created_at
        end, Txs),
        {ok, Sorted}
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_transactions_by_type(WalletId, TxType) ->
    Fun = fun() ->
        All = mnesia:index_read(aptos_transaction, WalletId, #aptos_transaction.wallet_id),
        Filtered = lists:filter(fun(Tx) ->
            Tx#aptos_transaction.tx_type =:= TxType
        end, All),
        Sorted = lists:sort(fun(A, B) ->
            A#aptos_transaction.created_at >= B#aptos_transaction.created_at
        end, Filtered),
        {ok, Sorted}
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

create_nft_collection(WalletId, CollectionData) ->
    Fun = fun() ->
        case mnesia:read(aptos_wallet, WalletId) of
            [] ->
                {error, wallet_not_found};
            [Wallet] ->
                CollectionId = nanoid:gen(),
                Now = calendar:universal_time(),
                Collection = #aptos_nft_collection{
                    collection_id    = CollectionId,
                    wallet_id        = WalletId,
                    user_id          = Wallet#aptos_wallet.user_id,
                    creator_address  = Wallet#aptos_wallet.account_address,
                    collection_name  = maps:get(collection_name, CollectionData, undefined),
                    description      = maps:get(description, CollectionData, undefined),
                    uri              = maps:get(uri, CollectionData, undefined),
                    max_supply       = maps:get(max_supply, CollectionData, undefined),
                    current_supply   = 0,
                    transaction_hash = maps:get(transaction_hash, CollectionData, undefined),
                    on_chain_id      = maps:get(on_chain_id, CollectionData, undefined),
                    created_at       = Now,
                    metadata         = maps:get(metadata, CollectionData, #{})
                },
                mnesia:write(Collection),
                {ok, CollectionId}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_nft_collection(CollectionId) ->
    Fun = fun() ->
        case mnesia:read(aptos_nft_collection, CollectionId) of
            []     -> {error, collection_not_found};
            [Coll] -> {ok, Coll}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet_nft_collections(WalletId) ->
    Fun = fun() ->
        Colls = mnesia:index_read(aptos_nft_collection, WalletId, #aptos_nft_collection.wallet_id),
        Sorted = lists:sort(fun(A, B) ->
            A#aptos_nft_collection.created_at >= B#aptos_nft_collection.created_at
        end, Colls),
        {ok, Sorted}
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_user_nft_collections(UserId) ->
    Fun = fun() ->
        Colls = mnesia:index_read(aptos_nft_collection, UserId, #aptos_nft_collection.user_id),
        Sorted = lists:sort(fun(A, B) ->
            A#aptos_nft_collection.created_at >= B#aptos_nft_collection.created_at
        end, Colls),
        {ok, Sorted}
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_nft_collection_by_name(CreatorAddress, CollectionName) ->
    Fun = fun() ->
        All = mnesia:index_read(aptos_nft_collection, CreatorAddress, #aptos_nft_collection.creator_address),
        case lists:filter(fun(C) ->
            C#aptos_nft_collection.collection_name =:= CollectionName
        end, All) of
            []      -> {error, collection_not_found};
            [C | _] -> {ok, C}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

create_nft_token(WalletId, TokenData) ->
    Fun = fun() ->
        case mnesia:read(aptos_wallet, WalletId) of
            [] ->
                {error, wallet_not_found};
            [Wallet] ->
                TokenId = nanoid:gen(),
                Now = calendar:universal_time(),
                Token = #aptos_nft_token{
                    token_id         = TokenId,
                    wallet_id        = WalletId,
                    user_id          = Wallet#aptos_wallet.user_id,
                    collection_id    = maps:get(collection_id, TokenData, undefined),
                    collection_name  = maps:get(collection_name, TokenData, undefined),
                    token_name       = maps:get(token_name, TokenData, undefined),
                    description      = maps:get(description, TokenData, undefined),
                    uri              = maps:get(uri, TokenData, undefined),
                    property_keys    = maps:get(property_keys, TokenData, []),
                    property_types   = maps:get(property_types, TokenData, []),
                    property_values  = maps:get(property_values, TokenData, []),
                    transaction_hash = maps:get(transaction_hash, TokenData, undefined),
                    on_chain_id      = maps:get(on_chain_id, TokenData, undefined),
                    created_at       = Now,
                    metadata         = maps:get(metadata, TokenData, #{})
                },
                mnesia:write(Token),
                {ok, TokenId}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_nft_token(TokenId) ->
    Fun = fun() ->
        case mnesia:read(aptos_nft_token, TokenId) of
            []      -> {error, token_not_found};
            [Token] -> {ok, Token}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet_nft_tokens(WalletId) ->
    Fun = fun() ->
        Tokens = mnesia:index_read(aptos_nft_token, WalletId, #aptos_nft_token.wallet_id),
        Sorted = lists:sort(fun(A, B) ->
            A#aptos_nft_token.created_at >= B#aptos_nft_token.created_at
        end, Tokens),
        {ok, Sorted}
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_collection_nft_tokens(CollectionId) ->
    Fun = fun() ->
        Tokens = mnesia:index_read(aptos_nft_token, CollectionId, #aptos_nft_token.collection_id),
        Sorted = lists:sort(fun(A, B) ->
            A#aptos_nft_token.created_at >= B#aptos_nft_token.created_at
        end, Tokens),
        {ok, Sorted}
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

create_fungible_asset(WalletId, FaData) ->
    Fun = fun() ->
        case mnesia:read(aptos_wallet, WalletId) of
            [] ->
                {error, wallet_not_found};
            [Wallet] ->
                FaId = nanoid:gen(),
                Now = calendar:universal_time(),
                Fa = #aptos_fungible_asset{
                    fa_id            = FaId,
                    wallet_id        = WalletId,
                    user_id          = Wallet#aptos_wallet.user_id,
                    creator_address  = Wallet#aptos_wallet.account_address,
                    asset_name       = maps:get(asset_name, FaData, undefined),
                    symbol           = maps:get(symbol, FaData, undefined),
                    decimals         = maps:get(decimals, FaData, 8),
                    metadata_address = maps:get(metadata_address, FaData, undefined),
                    transaction_hash = maps:get(transaction_hash, FaData, undefined),
                    created_at       = Now,
                    metadata         = maps:get(metadata, FaData, #{})
                },
                mnesia:write(Fa),
                {ok, FaId}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_fungible_asset(FaId) ->
    Fun = fun() ->
        case mnesia:read(aptos_fungible_asset, FaId) of
            []   -> {error, fa_not_found};
            [Fa] -> {ok, Fa}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet_fungible_assets(WalletId) ->
    Fun = fun() ->
        Assets = mnesia:index_read(aptos_fungible_asset, WalletId, #aptos_fungible_asset.wallet_id),
        Sorted = lists:sort(fun(A, B) ->
            A#aptos_fungible_asset.created_at >= B#aptos_fungible_asset.created_at
        end, Assets),
        {ok, Sorted}
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_user_fungible_assets(UserId) ->
    Fun = fun() ->
        Assets = mnesia:index_read(aptos_fungible_asset, UserId, #aptos_fungible_asset.user_id),
        Sorted = lists:sort(fun(A, B) ->
            A#aptos_fungible_asset.created_at >= B#aptos_fungible_asset.created_at
        end, Assets),
        {ok, Sorted}
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_fungible_asset_by_metadata(MetadataAddress) ->
    Fun = fun() ->
        case mnesia:index_read(aptos_fungible_asset, MetadataAddress, #aptos_fungible_asset.metadata_address) of
            []     -> {error, fa_not_found};
            [Fa|_] -> {ok, Fa}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

create_stake(WalletId, StakeData) ->
    Fun = fun() ->
        case mnesia:read(aptos_wallet, WalletId) of
            [] ->
                {error, wallet_not_found};
            [Wallet] ->
                StakeId = nanoid:gen(),
                Now = calendar:universal_time(),
                Stake = #aptos_stake{
                    stake_id               = StakeId,
                    wallet_id              = WalletId,
                    user_id                = Wallet#aptos_wallet.user_id,
                    delegator_address      = Wallet#aptos_wallet.account_address,
                    pool_address           = maps:get(pool_address, StakeData),
                    amount_octas           = maps:get(amount_octas, StakeData, 0),
                    active_stake           = maps:get(active_stake, StakeData, undefined),
                    inactive_stake         = maps:get(inactive_stake, StakeData, undefined),
                    pending_inactive_stake = maps:get(pending_inactive_stake, StakeData, undefined),
                    tx_type                = maps:get(tx_type, StakeData, stake_add),
                    transaction_hash       = maps:get(transaction_hash, StakeData, undefined),
                    gas_used               = maps:get(gas_used, StakeData, 0),
                    gas_unit_price         = maps:get(gas_unit_price, StakeData, 0),
                    vm_status              = maps:get(vm_status, StakeData, undefined),
                    version                = maps:get(version, StakeData, undefined),
                    network                = maps:get(network, StakeData, Wallet#aptos_wallet.network),
                    status                 = maps:get(status, StakeData, confirmed),
                    created_at             = Now,
                    metadata               = maps:get(metadata, StakeData, #{})
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
        case mnesia:read(aptos_stake, StakeId) of
            []      -> {error, stake_not_found};
            [Stake] -> {ok, Stake}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet_stakes(WalletId) ->
    Fun = fun() ->
        Stakes = mnesia:index_read(aptos_stake, WalletId, #aptos_stake.wallet_id),
        Sorted = lists:sort(fun(A, B) ->
            A#aptos_stake.created_at >= B#aptos_stake.created_at
        end, Stakes),
        {ok, Sorted}
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_user_stakes(UserId) ->
    Fun = fun() ->
        Stakes = mnesia:index_read(aptos_stake, UserId, #aptos_stake.user_id),
        Sorted = lists:sort(fun(A, B) ->
            A#aptos_stake.created_at >= B#aptos_stake.created_at
        end, Stakes),
        {ok, Sorted}
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_stakes_by_pool(WalletId, PoolAddress) ->
    Fun = fun() ->
        All = mnesia:index_read(aptos_stake, WalletId, #aptos_stake.wallet_id),
        Filtered = lists:filter(fun(S) ->
            S#aptos_stake.pool_address =:= PoolAddress
        end, All),
        Sorted = lists:sort(fun(A, B) ->
            A#aptos_stake.created_at >= B#aptos_stake.created_at
        end, Filtered),
        {ok, Sorted}
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

update_stake(StakeId, Updates) ->
    Fun = fun() ->
        case mnesia:read(aptos_stake, StakeId) of
            [] ->
                {error, stake_not_found};
            [Stake] ->
                Updated = Stake#aptos_stake{
                    active_stake           = maps:get(active_stake, Updates, Stake#aptos_stake.active_stake),
                    inactive_stake         = maps:get(inactive_stake, Updates, Stake#aptos_stake.inactive_stake),
                    pending_inactive_stake = maps:get(pending_inactive_stake, Updates, Stake#aptos_stake.pending_inactive_stake),
                    status                 = maps:get(status, Updates, Stake#aptos_stake.status),
                    metadata               = maps:get(metadata, Updates, Stake#aptos_stake.metadata)
                },
                mnesia:write(Updated),
                {ok, StakeId}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

delete_stake(StakeId) ->
    Fun = fun() ->
        case mnesia:read(aptos_stake, StakeId) of
            [] ->
                {error, stake_not_found};
            [_] ->
                mnesia:delete({aptos_stake, StakeId}),
                {ok, deleted}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

upsert_coin_balance(WalletId, BalanceData) ->
    Fun = fun() ->
        case mnesia:read(aptos_wallet, WalletId) of
            [] ->
                {error, wallet_not_found};
            [Wallet] ->
                AccountAddress = Wallet#aptos_wallet.account_address,
                CoinType = maps:get(coin_type, BalanceData),
                Now = calendar:universal_time(),
                ExistingId = case mnesia:index_read(aptos_coin_balance, WalletId, #aptos_coin_balance.wallet_id) of
                    [] ->
                        nanoid:gen();
                    Existing ->
                        case lists:filter(fun(B) ->
                            B#aptos_coin_balance.coin_type =:= CoinType
                        end, Existing) of
                            []      -> nanoid:gen();
                            [B | _] -> B#aptos_coin_balance.balance_id
                        end
                end,
                Record = #aptos_coin_balance{
                    balance_id      = ExistingId,
                    wallet_id       = WalletId,
                    user_id         = Wallet#aptos_wallet.user_id,
                    account_address = AccountAddress,
                    coin_type       = CoinType,
                    balance         = maps:get(balance, BalanceData, 0),
                    network         = maps:get(network, BalanceData, Wallet#aptos_wallet.network),
                    last_synced     = Now,
                    metadata        = maps:get(metadata, BalanceData, #{})
                },
                mnesia:write(Record),
                {ok, ExistingId}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_coin_balance(WalletId, CoinType) ->
    Fun = fun() ->
        All = mnesia:index_read(aptos_coin_balance, WalletId, #aptos_coin_balance.wallet_id),
        case lists:filter(fun(B) ->
            B#aptos_coin_balance.coin_type =:= CoinType
        end, All) of
            []      -> {error, balance_not_found};
            [B | _] -> {ok, B}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet_coin_balances(WalletId) ->
    Fun = fun() ->
        Balances = mnesia:index_read(aptos_coin_balance, WalletId, #aptos_coin_balance.wallet_id),
        {ok, Balances}
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_user_coin_balances(UserId) ->
    Fun = fun() ->
        Balances = mnesia:index_read(aptos_coin_balance, UserId, #aptos_coin_balance.user_id),
        {ok, Balances}
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

delete_coin_balance(BalanceId) ->
    Fun = fun() ->
        case mnesia:read(aptos_coin_balance, BalanceId) of
            [] ->
                {error, balance_not_found};
            [_] ->
                mnesia:delete({aptos_coin_balance, BalanceId}),
                {ok, deleted}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

upsert_fa_balance(WalletId, BalanceData) ->
    Fun = fun() ->
        case mnesia:read(aptos_wallet, WalletId) of
            [] ->
                {error, wallet_not_found};
            [Wallet] ->
                AccountAddress = Wallet#aptos_wallet.account_address,
                AssetType = maps:get(asset_type, BalanceData),
                Now = calendar:universal_time(),
                ExistingId = case mnesia:index_read(aptos_fa_balance, WalletId, #aptos_fa_balance.wallet_id) of
                    [] ->
                        nanoid:gen();
                    Existing ->
                        case lists:filter(fun(B) ->
                            B#aptos_fa_balance.asset_type =:= AssetType
                        end, Existing) of
                            []      -> nanoid:gen();
                            [B | _] -> B#aptos_fa_balance.fa_balance_id
                        end
                end,
                Record = #aptos_fa_balance{
                    fa_balance_id    = ExistingId,
                    wallet_id        = WalletId,
                    user_id          = Wallet#aptos_wallet.user_id,
                    account_address  = AccountAddress,
                    asset_type       = AssetType,
                    metadata_address = maps:get(metadata_address, BalanceData, undefined),
                    amount           = maps:get(amount, BalanceData, 0),
                    is_primary       = maps:get(is_primary, BalanceData, false),
                    network          = maps:get(network, BalanceData, Wallet#aptos_wallet.network),
                    last_synced      = Now,
                    metadata         = maps:get(metadata, BalanceData, #{})
                },
                mnesia:write(Record),
                {ok, ExistingId}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_fa_balance(WalletId, AssetType) ->
    Fun = fun() ->
        All = mnesia:index_read(aptos_fa_balance, WalletId, #aptos_fa_balance.wallet_id),
        case lists:filter(fun(B) ->
            B#aptos_fa_balance.asset_type =:= AssetType
        end, All) of
            []      -> {error, balance_not_found};
            [B | _] -> {ok, B}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet_fa_balances(WalletId) ->
    Fun = fun() ->
        Balances = mnesia:index_read(aptos_fa_balance, WalletId, #aptos_fa_balance.wallet_id),
        {ok, Balances}
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_user_fa_balances(UserId) ->
    Fun = fun() ->
        Balances = mnesia:index_read(aptos_fa_balance, UserId, #aptos_fa_balance.user_id),
        {ok, Balances}
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

delete_fa_balance(FaBalanceId) ->
    Fun = fun() ->
        case mnesia:read(aptos_fa_balance, FaBalanceId) of
            [] ->
                {error, balance_not_found};
            [_] ->
                mnesia:delete({aptos_fa_balance, FaBalanceId}),
                {ok, deleted}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

address_exists_internal(Address) ->
    case mnesia:index_read(aptos_wallet, Address, #aptos_wallet.account_address) of
        [] -> false;
        _  -> true
    end.

get_user_wallets_internal(UserId) ->
    mnesia:index_read(aptos_wallet, UserId, #aptos_wallet.user_id).

get_network() ->
    case application:get_env(otpcode, aptos_network) of
        {ok, Network} -> Network;
        undefined     -> testnet
    end.
