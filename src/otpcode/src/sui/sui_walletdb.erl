-module(sui_walletdb).
-author("Zaryn Technologies").
-include("../records.hrl").
-include("../wallet_records.hrl").

-export([
    create_wallet/6,
    create_wallet/7,
    get_wallet/1,
    get_wallet_by_address/1,
    get_user_wallets/1,
    get_user_wallets_by_network/2,
    get_primary_wallet/1,
    set_primary_wallet/2,
    update_wallet_label/2,
    update_wallet_last_used/1,
    delete_wallet/1,
    wallet_exists/1,
    address_exists/1,

    create_transaction/2,
    get_transaction/1,
    get_transaction_by_digest/1,
    get_wallet_transactions/1,
    get_wallet_transactions/3,
    get_user_transactions/1,
    get_transactions_by_type/2,
    update_transaction_status/4,

    create_stake/2,
    get_stake/1,
    get_stake_by_staked_sui_id/1,
    get_wallet_stakes/1,
    get_user_stakes/1,
    get_stakes_by_validator/2,
    update_stake/2,
    delete_stake/1,

    upsert_object/2,
    get_object/1,
    get_wallet_objects/1,
    get_wallet_nfts/1,
    get_user_objects/1,
    delete_object/1,

    upsert_coin_balance/2,
    get_coin_balance/2,
    get_wallet_coin_balances/1,
    get_user_coin_balances/1,
    delete_coin_balance/1
]).

create_wallet(UserId, SuiAddress, EncryptedPrivKey, Iv, Tag, Scheme) ->
    create_wallet(UserId, SuiAddress, EncryptedPrivKey, Iv, Tag, Scheme, #{}).

create_wallet(UserId, SuiAddress, EncryptedPrivKey, Iv, Tag, Scheme, Opts) ->
    case address_exists(SuiAddress) of
        true ->
            {error, address_already_exists};
        false ->
            WalletId = generate_id("sui_wallet"),
            Label     = maps:get(label, Opts, undefined),
            Network   = maps:get(network, Opts, mainnet),
            PublicKey = maps:get(public_key, Opts, <<>>),
            Now       = erlang:system_time(millisecond),
            ExistingWallets = get_user_wallets(UserId),
            IsPrimary = ExistingWallets =:= [],
            Wallet = #sui_wallet{
                wallet_id             = WalletId,
                user_id               = UserId,
                label                 = Label,
                sui_address           = SuiAddress,
                public_key            = PublicKey,
                encrypted_private_key = EncryptedPrivKey,
                encryption_iv         = Iv,
                encryption_tag        = Tag,
                scheme                = Scheme,
                network               = Network,
                is_primary            = IsPrimary,
                created_at            = Now
            },
            F = fun() -> mnesia:write(Wallet) end,
            case mnesia:transaction(F) of
                {atomic, ok} -> {ok, WalletId};
                {aborted, Reason} -> {error, Reason}
            end
    end.

get_wallet(WalletId) ->
    F = fun() -> mnesia:read(sui_wallet, WalletId) end,
    case mnesia:transaction(F) of
        {atomic, [W]} -> {ok, W};
        {atomic, []}  -> {error, wallet_not_found};
        {aborted, R}  -> {error, R}
    end.

get_wallet_by_address(SuiAddress) ->
    F = fun() ->
        mnesia:index_read(sui_wallet, SuiAddress, #sui_wallet.sui_address)
    end,
    case mnesia:transaction(F) of
        {atomic, [W]} -> {ok, W};
        {atomic, []}  -> {error, wallet_not_found};
        {aborted, R}  -> {error, R}
    end.

get_user_wallets(UserId) ->
    F = fun() ->
        mnesia:index_read(sui_wallet, UserId, #sui_wallet.user_id)
    end,
    case mnesia:transaction(F) of
        {atomic, Wallets} -> Wallets;
        {aborted, _}      -> []
    end.

get_user_wallets_by_network(UserId, Network) ->
    All = get_user_wallets(UserId),
    [W || W <- All, W#sui_wallet.network =:= Network].

get_primary_wallet(UserId) ->
    Wallets = get_user_wallets(UserId),
    case [W || W <- Wallets, W#sui_wallet.is_primary =:= true] of
        [Primary | _] -> {ok, Primary};
        []            -> {error, no_primary_wallet}
    end.

set_primary_wallet(UserId, WalletId) ->
    F = fun() ->
        Wallets = mnesia:index_read(sui_wallet, UserId, #sui_wallet.user_id),
        lists:foreach(fun(W) ->
            mnesia:write(W#sui_wallet{is_primary = false})
        end, Wallets),
        case mnesia:read(sui_wallet, WalletId) of
            [Target] ->
                mnesia:write(Target#sui_wallet{is_primary = true}),
                ok;
            [] ->
                mnesia:abort(wallet_not_found)
        end
    end,
    case mnesia:transaction(F) of
        {atomic, ok} -> {ok, WalletId};
        {aborted, R} -> {error, R}
    end.

update_wallet_label(WalletId, Label) ->
    F = fun() ->
        case mnesia:read(sui_wallet, WalletId) of
            [W] ->
                mnesia:write(W#sui_wallet{label = Label}),
                ok;
            [] ->
                mnesia:abort(wallet_not_found)
        end
    end,
    case mnesia:transaction(F) of
        {atomic, ok} -> {ok, WalletId};
        {aborted, R} -> {error, R}
    end.

update_wallet_last_used(WalletId) ->
    F = fun() ->
        case mnesia:read(sui_wallet, WalletId) of
            [W] ->
                mnesia:write(W#sui_wallet{last_used = erlang:system_time(millisecond)}),
                ok;
            [] ->
                mnesia:abort(wallet_not_found)
        end
    end,
    case mnesia:transaction(F) of
        {atomic, ok} -> {ok, WalletId};
        {aborted, R} -> {error, R}
    end.

delete_wallet(WalletId) ->
    F = fun() ->
        case mnesia:read(sui_wallet, WalletId) of
            [W] ->
                WasPrimary = W#sui_wallet.is_primary,
                UserId     = W#sui_wallet.user_id,
                mnesia:delete({sui_wallet, WalletId}),
                case WasPrimary of
                    true ->
                        Remaining = mnesia:index_read(sui_wallet, UserId, #sui_wallet.user_id),
                        case Remaining of
                            [First | _] ->
                                mnesia:write(First#sui_wallet{is_primary = true});
                            [] ->
                                ok
                        end;
                    false ->
                        ok
                end;
            [] ->
                mnesia:abort(wallet_not_found)
        end
    end,
    case mnesia:transaction(F) of
        {atomic, ok} -> {ok, deleted};
        {aborted, R} -> {error, R}
    end.

wallet_exists(WalletId) ->
    case get_wallet(WalletId) of
        {ok, _}    -> true;
        {error, _} -> false
    end.

address_exists(SuiAddress) ->
    case get_wallet_by_address(SuiAddress) of
        {ok, _}    -> true;
        {error, _} -> false
    end.

create_transaction(WalletId, Params) ->
    case get_wallet(WalletId) of
        {error, _} ->
            {error, wallet_not_found};
        {ok, Wallet} ->
            TxId     = generate_id("sui_tx"),
            Now      = erlang:system_time(millisecond),
            Network  = maps:get(network, Params, Wallet#sui_wallet.network),
            Tx = #sui_transaction{
                tx_id              = TxId,
                wallet_id          = WalletId,
                user_id            = Wallet#sui_wallet.user_id,
                digest             = maps:get(digest, Params, undefined),
                tx_type            = maps:get(tx_type, Params, sui_transfer),
                sender_address     = maps:get(sender_address, Params, Wallet#sui_wallet.sui_address),
                recipient_address  = maps:get(recipient_address, Params, undefined),
                amount_mist        = maps:get(amount_mist, Params, undefined),
                coin_type          = maps:get(coin_type, Params, undefined),
                object_id          = maps:get(object_id, Params, undefined),
                package_id         = maps:get(package_id, Params, undefined),
                module_name        = maps:get(module_name, Params, undefined),
                function_name      = maps:get(function_name, Params, undefined),
                validator_address  = maps:get(validator_address, Params, undefined),
                staked_sui_id      = maps:get(staked_sui_id, Params, undefined),
                gas_computation    = maps:get(gas_computation, Params, <<"0">>),
                gas_storage        = maps:get(gas_storage, Params, <<"0">>),
                gas_storage_rebate = maps:get(gas_storage_rebate, Params, <<"0">>),
                status             = maps:get(status, Params, pending),
                effects_status     = maps:get(effects_status, Params, <<"">>),
                checkpoint         = maps:get(checkpoint, Params, undefined),
                epoch              = maps:get(epoch, Params, undefined),
                error_message      = maps:get(error_message, Params, undefined),
                network            = Network,
                created_at         = Now
            },
            F = fun() -> mnesia:write(Tx) end,
            case mnesia:transaction(F) of
                {atomic, ok} -> {ok, TxId};
                {aborted, R} -> {error, R}
            end
    end.

get_transaction(TxId) ->
    F = fun() -> mnesia:read(sui_transaction, TxId) end,
    case mnesia:transaction(F) of
        {atomic, [Tx]} -> {ok, Tx};
        {atomic, []}   -> {error, transaction_not_found};
        {aborted, R}   -> {error, R}
    end.

get_transaction_by_digest(Digest) ->
    F = fun() ->
        mnesia:index_read(sui_transaction, Digest, #sui_transaction.digest)
    end,
    case mnesia:transaction(F) of
        {atomic, [Tx]} -> {ok, Tx};
        {atomic, []}   -> {error, transaction_not_found};
        {aborted, R}   -> {error, R}
    end.

get_wallet_transactions(WalletId) ->
    F = fun() ->
        mnesia:index_read(sui_transaction, WalletId, #sui_transaction.wallet_id)
    end,
    case mnesia:transaction(F) of
        {atomic, Txs} ->
            Sorted = lists:sort(fun(A, B) -> A#sui_transaction.created_at >= B#sui_transaction.created_at end, Txs),
            {ok, Sorted};
        {aborted, R} ->
            {error, R}
    end.

get_wallet_transactions(WalletId, Limit, Offset) ->
    case get_wallet_transactions(WalletId) of
        {ok, All} ->
            Total  = length(All),
            Paged  = lists:sublist(lists:nthtail(min(Offset, Total), All), Limit),
            {ok, Paged, Total};
        {error, R} ->
            {error, R}
    end.

get_user_transactions(UserId) ->
    F = fun() ->
        mnesia:index_read(sui_transaction, UserId, #sui_transaction.user_id)
    end,
    case mnesia:transaction(F) of
        {atomic, Txs} ->
            Sorted = lists:sort(fun(A, B) -> A#sui_transaction.created_at >= B#sui_transaction.created_at end, Txs),
            {ok, Sorted};
        {aborted, R} ->
            {error, R}
    end.

get_transactions_by_type(WalletId, TxType) ->
    case get_wallet_transactions(WalletId) of
        {ok, All} ->
            {ok, [T || T <- All, T#sui_transaction.tx_type =:= TxType]};
        {error, R} ->
            {error, R}
    end.

update_transaction_status(Digest, Status, EffectsStatus, Checkpoint) ->
    F = fun() ->
        case mnesia:index_read(sui_transaction, Digest, #sui_transaction.digest) of
            [Tx] ->
                mnesia:write(Tx#sui_transaction{
                    status         = Status,
                    effects_status = EffectsStatus,
                    checkpoint     = Checkpoint
                }),
                ok;
            [] ->
                mnesia:abort(transaction_not_found)
        end
    end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        {aborted, R} -> {error, R}
    end.

create_stake(WalletId, Params) ->
    case get_wallet(WalletId) of
        {error, _} ->
            {error, wallet_not_found};
        {ok, Wallet} ->
            StakeId = generate_id("sui_stake"),
            Now     = erlang:system_time(millisecond),
            Stake = #sui_stake{
                stake_id               = StakeId,
                wallet_id              = WalletId,
                user_id                = Wallet#sui_wallet.user_id,
                owner_address          = maps:get(owner_address, Params, Wallet#sui_wallet.sui_address),
                validator_address      = maps:get(validator_address, Params, undefined),
                staked_sui_id          = maps:get(staked_sui_id, Params, undefined),
                principal_mist         = maps:get(principal_mist, Params, <<"0">>),
                estimated_reward_mist  = maps:get(estimated_reward_mist, Params, <<"0">>),
                stake_activation_epoch = maps:get(stake_activation_epoch, Params, undefined),
                status                 = maps:get(status, Params, pending),
                tx_type                = maps:get(tx_type, Params, stake),
                digest                 = maps:get(digest, Params, undefined),
                gas_computation        = maps:get(gas_computation, Params, <<"0">>),
                gas_storage            = maps:get(gas_storage, Params, <<"0">>),
                gas_storage_rebate     = maps:get(gas_storage_rebate, Params, <<"0">>),
                network                = maps:get(network, Params, Wallet#sui_wallet.network),
                created_at             = Now
            },
            F = fun() -> mnesia:write(Stake) end,
            case mnesia:transaction(F) of
                {atomic, ok} -> {ok, StakeId};
                {aborted, R} -> {error, R}
            end
    end.

get_stake(StakeId) ->
    F = fun() -> mnesia:read(sui_stake, StakeId) end,
    case mnesia:transaction(F) of
        {atomic, [S]} -> {ok, S};
        {atomic, []}  -> {error, stake_not_found};
        {aborted, R}  -> {error, R}
    end.

get_stake_by_staked_sui_id(StakedSuiId) ->
    F = fun() ->
        mnesia:index_read(sui_stake, StakedSuiId, #sui_stake.staked_sui_id)
    end,
    case mnesia:transaction(F) of
        {atomic, [S]} -> {ok, S};
        {atomic, []}  -> {error, stake_not_found};
        {aborted, R}  -> {error, R}
    end.

get_wallet_stakes(WalletId) ->
    F = fun() ->
        mnesia:index_read(sui_stake, WalletId, #sui_stake.wallet_id)
    end,
    case mnesia:transaction(F) of
        {atomic, Stakes} -> {ok, Stakes};
        {aborted, R}     -> {error, R}
    end.

get_user_stakes(UserId) ->
    F = fun() ->
        mnesia:index_read(sui_stake, UserId, #sui_stake.user_id)
    end,
    case mnesia:transaction(F) of
        {atomic, Stakes} -> {ok, Stakes};
        {aborted, R}     -> {error, R}
    end.

get_stakes_by_validator(WalletId, ValidatorAddress) ->
    case get_wallet_stakes(WalletId) of
        {ok, All} ->
            {ok, [S || S <- All, S#sui_stake.validator_address =:= ValidatorAddress]};
        {error, R} ->
            {error, R}
    end.

update_stake(StakeId, Params) ->
    F = fun() ->
        case mnesia:read(sui_stake, StakeId) of
            [S] ->
                Updated = S#sui_stake{
                    principal_mist        = maps:get(principal_mist, Params, S#sui_stake.principal_mist),
                    estimated_reward_mist = maps:get(estimated_reward_mist, Params, S#sui_stake.estimated_reward_mist),
                    status                = maps:get(status, Params, S#sui_stake.status)
                },
                mnesia:write(Updated),
                ok;
            [] ->
                mnesia:abort(stake_not_found)
        end
    end,
    case mnesia:transaction(F) of
        {atomic, ok} -> {ok, StakeId};
        {aborted, R} -> {error, R}
    end.

delete_stake(StakeId) ->
    F = fun() ->
        case mnesia:read(sui_stake, StakeId) of
            [_] ->
                mnesia:delete({sui_stake, StakeId}),
                ok;
            [] ->
                mnesia:abort(stake_not_found)
        end
    end,
    case mnesia:transaction(F) of
        {atomic, ok} -> {ok, deleted};
        {aborted, R} -> {error, R}
    end.

upsert_object(WalletId, Params) ->
    case get_wallet(WalletId) of
        {error, _} ->
            {error, wallet_not_found};
        {ok, Wallet} ->
            ObjectId = maps:get(object_id, Params),
            Now      = erlang:system_time(millisecond),
            Obj = #sui_object{
                object_id            = ObjectId,
                wallet_id            = WalletId,
                user_id              = Wallet#sui_wallet.user_id,
                owner_address        = maps:get(owner_address, Params, Wallet#sui_wallet.sui_address),
                object_type          = maps:get(object_type, Params, undefined),
                version              = maps:get(version, Params, undefined),
                digest               = maps:get(digest, Params, undefined),
                balance              = maps:get(balance, Params, undefined),
                previous_transaction = maps:get(previous_transaction, Params, undefined),
                storage_rebate       = maps:get(storage_rebate, Params, undefined),
                is_nft               = maps:get(is_nft, Params, false),
                nft_name             = maps:get(nft_name, Params, undefined),
                nft_description      = maps:get(nft_description, Params, undefined),
                nft_url              = maps:get(nft_url, Params, undefined),
                network              = maps:get(network, Params, Wallet#sui_wallet.network),
                synced_at            = Now
            },
            F = fun() -> mnesia:write(Obj) end,
            case mnesia:transaction(F) of
                {atomic, ok} -> {ok, ObjectId};
                {aborted, R} -> {error, R}
            end
    end.

get_object(ObjectId) ->
    F = fun() -> mnesia:read(sui_object, ObjectId) end,
    case mnesia:transaction(F) of
        {atomic, [O]} -> {ok, O};
        {atomic, []}  -> {error, object_not_found};
        {aborted, R}  -> {error, R}
    end.

get_wallet_objects(WalletId) ->
    F = fun() ->
        mnesia:index_read(sui_object, WalletId, #sui_object.wallet_id)
    end,
    case mnesia:transaction(F) of
        {atomic, Objs} -> {ok, Objs};
        {aborted, R}   -> {error, R}
    end.

get_wallet_nfts(WalletId) ->
    case get_wallet_objects(WalletId) of
        {ok, All} ->
            {ok, [O || O <- All, O#sui_object.is_nft =:= true]};
        {error, R} ->
            {error, R}
    end.

get_user_objects(UserId) ->
    F = fun() ->
        mnesia:index_read(sui_object, UserId, #sui_object.user_id)
    end,
    case mnesia:transaction(F) of
        {atomic, Objs} -> {ok, Objs};
        {aborted, R}   -> {error, R}
    end.

delete_object(ObjectId) ->
    F = fun() ->
        case mnesia:read(sui_object, ObjectId) of
            [_] ->
                mnesia:delete({sui_object, ObjectId}),
                ok;
            [] ->
                mnesia:abort(object_not_found)
        end
    end,
    case mnesia:transaction(F) of
        {atomic, ok} -> {ok, deleted};
        {aborted, R} -> {error, R}
    end.

upsert_coin_balance(WalletId, Params) ->
    case get_wallet(WalletId) of
        {error, _} ->
            {error, wallet_not_found};
        {ok, Wallet} ->
            CoinType = maps:get(coin_type, Params),
            Now      = erlang:system_time(millisecond),
            BalanceId = generate_id("sui_bal"),
            ExistingId = case get_coin_balance(WalletId, CoinType) of
                {ok, Existing} -> Existing#sui_coin_balance.balance_id;
                {error, _}     -> BalanceId
            end,
            Bal = #sui_coin_balance{
                balance_id        = ExistingId,
                wallet_id         = WalletId,
                user_id           = Wallet#sui_wallet.user_id,
                owner_address     = maps:get(owner_address, Params, Wallet#sui_wallet.sui_address),
                coin_type         = CoinType,
                total_balance     = maps:get(total_balance, Params, <<"0">>),
                coin_object_count = maps:get(coin_object_count, Params, 0),
                network           = maps:get(network, Params, Wallet#sui_wallet.network),
                synced_at         = Now
            },
            F = fun() -> mnesia:write(Bal) end,
            case mnesia:transaction(F) of
                {atomic, ok} -> {ok, ExistingId};
                {aborted, R} -> {error, R}
            end
    end.

get_coin_balance(WalletId, CoinType) ->
    case get_wallet_coin_balances(WalletId) of
        {ok, All} ->
            case [B || B <- All, B#sui_coin_balance.coin_type =:= CoinType] of
                [B | _] -> {ok, B};
                []      -> {error, balance_not_found}
            end;
        {error, R} ->
            {error, R}
    end.

get_wallet_coin_balances(WalletId) ->
    F = fun() ->
        mnesia:index_read(sui_coin_balance, WalletId, #sui_coin_balance.wallet_id)
    end,
    case mnesia:transaction(F) of
        {atomic, Bals} -> {ok, Bals};
        {aborted, R}   -> {error, R}
    end.

get_user_coin_balances(UserId) ->
    F = fun() ->
        mnesia:index_read(sui_coin_balance, UserId, #sui_coin_balance.user_id)
    end,
    case mnesia:transaction(F) of
        {atomic, Bals} -> {ok, Bals};
        {aborted, R}   -> {error, R}
    end.

delete_coin_balance(BalanceId) ->
    F = fun() ->
        case mnesia:read(sui_coin_balance, BalanceId) of
            [_] ->
                mnesia:delete({sui_coin_balance, BalanceId}),
                ok;
            [] ->
                mnesia:abort(balance_not_found)
        end
    end,
    case mnesia:transaction(F) of
        {atomic, ok} -> {ok, deleted};
        {aborted, R} -> {error, R}
    end.

generate_id(Prefix) ->
    Unique = integer_to_list(erlang:unique_integer([positive])),
    Prefix ++ "_" ++ Unique.
