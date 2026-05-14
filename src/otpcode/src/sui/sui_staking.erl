-module(sui_staking).
-author("Zaryn Technologies").
-include("../records.hrl").
-include("../wallet_records.hrl").

-export([
    stake/5,
    stake/6,
    unstake/4,
    unstake/5,
    get_stake_info/4,
    get_user_stakes/3,
    get_wallet_stakes/3,
    get_validators/2,
    get_validator/3,
    get_epoch/2,
    get_staking_rewards/3,
    sync_stake/3,
    sync_all_stakes/3
]).

stake(Token, WalletId, ValidatorAddress, AmountMist, Network) ->
    stake(Token, WalletId, ValidatorAddress, AmountMist, Network, #{}).

stake(Token, WalletId, ValidatorAddress, AmountMist, Network, Opts) ->
    WalletRecord = sui_walletdb:get_wallet(WalletId),
    case WalletRecord of
        {error, _} ->
            {error, wallet_not_found};
        {ok, Wallet} ->
            SenderAddress = Wallet#sui_wallet.sui_address,
            case sui_wallet:stake_with_validator(
                Token, SenderAddress, ValidatorAddress, AmountMist, Network, Opts
            ) of
                {ok, #{<<"digest">> := Digest,
                       <<"staked_sui_id">> := StakedSuiId,
                       <<"amount_mist">> := ReturnedAmount,
                       <<"gas_used">> := GasUsed,
                       <<"epoch">> := Epoch} = Result} ->
                    StakeParams = #{
                        owner_address          => SenderAddress,
                        validator_address      => ValidatorAddress,
                        staked_sui_id          => StakedSuiId,
                        principal_mist         => ReturnedAmount,
                        stake_activation_epoch => Epoch,
                        status                 => pending,
                        tx_type                => stake,
                        digest                 => Digest,
                        gas_computation        => maps:get(<<"computation_cost">>, GasUsed, <<"0">>),
                        gas_storage            => maps:get(<<"storage_cost">>, GasUsed, <<"0">>),
                        gas_storage_rebate     => maps:get(<<"storage_rebate">>, GasUsed, <<"0">>),
                        network                => Network
                    },
                    case sui_walletdb:create_stake(WalletId, StakeParams) of
                        {ok, StakeId} ->
                            TxParams = #{
                                digest            => Digest,
                                tx_type           => stake,
                                sender_address    => SenderAddress,
                                validator_address => ValidatorAddress,
                                staked_sui_id     => StakedSuiId,
                                amount_mist       => ReturnedAmount,
                                gas_computation   => maps:get(<<"computation_cost">>, GasUsed, <<"0">>),
                                gas_storage       => maps:get(<<"storage_cost">>, GasUsed, <<"0">>),
                                gas_storage_rebate => maps:get(<<"storage_rebate">>, GasUsed, <<"0">>),
                                status            => success,
                                effects_status    => <<"success">>,
                                epoch             => Epoch,
                                network           => Network
                            },
                            sui_walletdb:create_transaction(WalletId, TxParams),
                            {ok, maps:put(<<"stake_id">>, list_to_binary(StakeId), Result)};
                        {error, DbErr} ->
                            {ok, maps:put(<<"stake_id">>, <<>>, Result)},
                            {error, DbErr}
                    end;
                {ok, Result} ->
                    {ok, Result};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

unstake(Token, WalletId, StakedSuiId, Network) ->
    unstake(Token, WalletId, StakedSuiId, Network, #{}).

unstake(Token, WalletId, StakedSuiId, Network, Opts) ->
    case sui_walletdb:get_wallet(WalletId) of
        {error, _} ->
            {error, wallet_not_found};
        {ok, Wallet} ->
            SenderAddress = Wallet#sui_wallet.sui_address,
            case sui_wallet:unstake_from_validator(
                Token, SenderAddress, StakedSuiId, Network, Opts
            ) of
                {ok, #{<<"digest">> := Digest,
                       <<"principal_amount">> := Principal,
                       <<"gas_used">> := GasUsed} = Result} ->
                    case sui_walletdb:get_stake_by_staked_sui_id(StakedSuiId) of
                        {ok, Stake} ->
                            sui_walletdb:update_stake(Stake#sui_stake.stake_id, #{
                                status => unstaked
                            });
                        {error, _} ->
                            ok
                    end,
                    TxParams = #{
                        digest            => Digest,
                        tx_type           => unstake,
                        sender_address    => SenderAddress,
                        staked_sui_id     => StakedSuiId,
                        amount_mist       => Principal,
                        gas_computation   => maps:get(<<"computation_cost">>, GasUsed, <<"0">>),
                        gas_storage       => maps:get(<<"storage_cost">>, GasUsed, <<"0">>),
                        gas_storage_rebate => maps:get(<<"storage_rebate">>, GasUsed, <<"0">>),
                        status            => success,
                        effects_status    => <<"success">>,
                        network           => Network
                    },
                    sui_walletdb:create_transaction(WalletId, TxParams),
                    {ok, Result};
                {ok, Result} ->
                    {ok, Result};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

get_stake_info(Token, WalletId, StakedSuiId, Network) ->
    case sui_walletdb:get_wallet(WalletId) of
        {error, _} ->
            {error, wallet_not_found};
        {ok, Wallet} ->
            OwnerAddress = Wallet#sui_wallet.sui_address,
            case sui_wallet:get_staked_sui(Token, OwnerAddress, Network) of
                {ok, #{<<"stakes">> := Stakes}} ->
                    Match = [S || S <- Stakes,
                        maps:get(<<"staked_sui_id">>, S, <<>>) =:= StakedSuiId],
                    case Match of
                        [Info | _] -> {ok, Info};
                        []         -> {error, stake_not_found}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

get_user_stakes(_Token, UserId, Network) ->
    case sui_walletdb:get_user_stakes(UserId) of
        {ok, LocalStakes} ->
            NetworkStakes = [S || S <- LocalStakes,
                S#sui_stake.network =:= Network orelse Network =:= all],
            {ok, #{
                user_id => UserId,
                stakes  => NetworkStakes,
                count   => length(NetworkStakes)
            }};
        {error, Reason} ->
            {error, Reason}
    end.

get_wallet_stakes(_Token, WalletId, Network) ->
    case sui_walletdb:get_wallet_stakes(WalletId) of
        {ok, LocalStakes} ->
            NetworkStakes = [S || S <- LocalStakes,
                S#sui_stake.network =:= Network orelse Network =:= all],
            {ok, #{
                wallet_id => WalletId,
                stakes    => NetworkStakes,
                count     => length(NetworkStakes)
            }};
        {error, Reason} ->
            {error, Reason}
    end.

get_validators(Token, Network) ->
    sui_wallet:get_validators(Token, Network).

get_validator(Token, ValidatorAddress, Network) ->
    sui_wallet:get_validator(Token, ValidatorAddress, Network).

get_epoch(Token, Network) ->
    sui_wallet:get_epoch(Token, Network).

get_staking_rewards(Token, WalletId, Network) ->
    case sui_walletdb:get_wallet(WalletId) of
        {error, _} ->
            {error, wallet_not_found};
        {ok, Wallet} ->
            OwnerAddress = Wallet#sui_wallet.sui_address,
            case sui_wallet:get_staking_rewards(Token, OwnerAddress, Network) of
                {ok, Rewards} ->
                    {ok, Rewards};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

sync_stake(Token, WalletId, StakedSuiId) ->
    case sui_walletdb:get_wallet(WalletId) of
        {error, _} ->
            {error, wallet_not_found};
        {ok, Wallet} ->
            Network      = Wallet#sui_wallet.network,
            OwnerAddress = Wallet#sui_wallet.sui_address,
            case sui_wallet:get_staked_sui(Token, OwnerAddress, Network) of
                {ok, #{<<"stakes">> := Stakes}} ->
                    Match = [S || S <- Stakes,
                        maps:get(<<"staked_sui_id">>, S, <<>>) =:= StakedSuiId],
                    case Match of
                        [OnChain | _] ->
                            StatusStr = maps:get(<<"status">>, OnChain, <<"Pending">>),
                            Status = case StatusStr of
                                <<"Active">>   -> active;
                                <<"Unstaked">> -> unstaked;
                                _              -> pending
                            end,
                            case sui_walletdb:get_stake_by_staked_sui_id(StakedSuiId) of
                                {ok, LocalStake} ->
                                    sui_walletdb:update_stake(LocalStake#sui_stake.stake_id, #{
                                        status                => Status,
                                        estimated_reward_mist => maps:get(<<"estimated_reward">>, OnChain, <<"0">>)
                                    });
                                {error, _} ->
                                    ok
                            end,
                            {ok, synced};
                        [] ->
                            {error, stake_not_found_on_chain}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

sync_all_stakes(Token, WalletId, Network) ->
    case sui_walletdb:get_wallet(WalletId) of
        {error, _} ->
            {error, wallet_not_found};
        {ok, Wallet} ->
            OwnerAddress = Wallet#sui_wallet.sui_address,
            case sui_wallet:get_staked_sui(Token, OwnerAddress, Network) of
                {ok, #{<<"stakes">> := OnChainStakes}} ->
                    lists:foreach(fun(OnChain) ->
                        StakedSuiId = maps:get(<<"staked_sui_id">>, OnChain, <<>>),
                        StatusStr   = maps:get(<<"status">>, OnChain, <<"Pending">>),
                        Status = case StatusStr of
                            <<"Active">>   -> active;
                            <<"Unstaked">> -> unstaked;
                            _              -> pending
                        end,
                        case sui_walletdb:get_stake_by_staked_sui_id(StakedSuiId) of
                            {ok, LocalStake} ->
                                sui_walletdb:update_stake(LocalStake#sui_stake.stake_id, #{
                                    status                => Status,
                                    estimated_reward_mist => maps:get(<<"estimated_reward">>, OnChain, <<"0">>)
                                });
                            {error, _} ->
                                ValidatorAddress = maps:get(<<"validator_address">>, OnChain, <<>>),
                                StakeParams = #{
                                    owner_address          => OwnerAddress,
                                    validator_address      => ValidatorAddress,
                                    staked_sui_id          => StakedSuiId,
                                    principal_mist         => maps:get(<<"principal">>, OnChain, <<"0">>),
                                    estimated_reward_mist  => maps:get(<<"estimated_reward">>, OnChain, <<"0">>),
                                    stake_activation_epoch => maps:get(<<"stake_activation_epoch">>, OnChain, <<>>),
                                    status                 => Status,
                                    tx_type                => stake,
                                    network                => Network
                                },
                                sui_walletdb:create_stake(WalletId, StakeParams)
                        end
                    end, OnChainStakes),
                    {ok, #{synced => length(OnChainStakes)}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.
