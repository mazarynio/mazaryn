-module(sui_wallet).
-author("Zaryn Technologies").
-include("../records.hrl").
-include("../wallet_records.hrl").

-export([
    create_wallet/2,
    create_wallet/3,
    import_wallet/3,
    import_wallet/4,
    import_wallet_from_mnemonic/3,
    import_wallet_from_mnemonic/4,
    delete_wallet/3,
    get_wallet_info/2,
    get_user_wallets/2,
    set_primary_wallet/3,
    update_wallet_label/3,

    get_sui_balance/2,
    get_balance_by_coin_type/3,
    get_all_balances/2,
    get_owned_coins/2,
    get_owned_coins/3,
    get_owned_objects/2,
    get_owned_objects/3,
    get_coin_info/2,
    get_coin_info/3,

    transfer_sui/5,
    transfer_sui/6,
    batch_transfer_sui/4,
    batch_transfer_sui/5,
    split_coin/5,
    split_coin/6,
    merge_coins/5,
    merge_coins/6,

    get_local_transaction_history/2,
    get_local_transaction_history/4,
    get_local_user_history/2,
    get_local_transaction_by_digest/2,

    get_transaction/3,
    execute_transaction/4,
    simulate_transaction/3,
    wait_for_transaction/3,
    build_transaction/4,

    execute_move_call/6,
    execute_move_call/7,
    publish_package/5,
    upgrade_package/6,

    get_validators/2,
    get_validator/3,
    get_epoch/2,
    stake_with_validator/5,
    stake_with_validator/6,
    unstake_from_validator/4,
    unstake_from_validator/5,
    get_staked_sui/3,
    get_staking_rewards/3,

    get_object/3,
    get_objects/3,
    get_dynamic_fields/3,

    mint_nft/6,
    mint_nft/7,
    transfer_nft/5,
    get_nfts/3,

    sign_message/3,
    verify_signature/4,
    create_multisig/4,
    combine_multisig/5,

    lookup_name/3,
    reverse_lookup_name/3,

    get_chain_info/2,
    get_reference_gas_price/2,
    get_checkpoint/3,
    request_faucet/3
]).

-define(HTTP_TIMEOUT, 30000).

get_base_url() ->
    application:get_env(otpcode, sui_api_url, "http://localhost:3001").

sui_post(Token, Path, Body) ->
    Url = get_base_url() ++ Path,
    Headers = [
        {"Content-Type", "application/json"},
        {"Authorization", "Bearer " ++ to_list(Token)}
    ],
    JsonBody = jiffy:encode(Body),
    case httpc:request(post, {Url, Headers, "application/json", JsonBody}, [{timeout, ?HTTP_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, RespBody}} ->
            {ok, jiffy:decode(list_to_binary(RespBody), [return_maps])};
        {ok, {{_, 201, _}, _, RespBody}} ->
            {ok, jiffy:decode(list_to_binary(RespBody), [return_maps])};
        {ok, {{_, StatusCode, _}, _, RespBody}} ->
            {error, {StatusCode, jiffy:decode(list_to_binary(RespBody), [return_maps])}};
        {error, Reason} ->
            {error, {failed_connect, Reason}}
    end.

sui_get(Token, Path) ->
    Url = get_base_url() ++ Path,
    Headers = [
        {"Authorization", "Bearer " ++ to_list(Token)}
    ],
    case httpc:request(get, {Url, Headers}, [{timeout, ?HTTP_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, RespBody}} ->
            {ok, jiffy:decode(list_to_binary(RespBody), [return_maps])};
        {ok, {{_, StatusCode, _}, _, RespBody}} ->
            {error, {StatusCode, jiffy:decode(list_to_binary(RespBody), [return_maps])}};
        {error, Reason} ->
            {error, {failed_connect, Reason}}
    end.

sui_delete(Token, Path) ->
    Url = get_base_url() ++ Path,
    Headers = [
        {"Authorization", "Bearer " ++ to_list(Token)}
    ],
    case httpc:request(delete, {Url, Headers, "", ""}, [{timeout, ?HTTP_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, RespBody}} ->
            {ok, jiffy:decode(list_to_binary(RespBody), [return_maps])};
        {ok, {{_, StatusCode, _}, _, RespBody}} ->
            {error, {StatusCode, jiffy:decode(list_to_binary(RespBody), [return_maps])}};
        {error, Reason} ->
            {error, {failed_connect, Reason}}
    end.

sui_patch(Token, Path, Body) ->
    Url = get_base_url() ++ Path,
    Headers = [
        {"Content-Type", "application/json"},
        {"Authorization", "Bearer " ++ to_list(Token)}
    ],
    JsonBody = jiffy:encode(Body),
    case httpc:request(patch, {Url, Headers, "application/json", JsonBody}, [{timeout, ?HTTP_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, RespBody}} ->
            {ok, jiffy:decode(list_to_binary(RespBody), [return_maps])};
        {ok, {{_, StatusCode, _}, _, RespBody}} ->
            {error, {StatusCode, jiffy:decode(list_to_binary(RespBody), [return_maps])}};
        {error, Reason} ->
            {error, {failed_connect, Reason}}
    end.

to_list(V) when is_binary(V) -> binary_to_list(V);
to_list(V) when is_list(V)   -> V;
to_list(V) when is_atom(V)   -> atom_to_list(V).

create_wallet(Token, UserId) ->
    create_wallet(Token, UserId, #{}).

create_wallet(Token, UserId, Opts) ->
    Body = maps:merge(#{user_id => UserId}, Opts),
    sui_post(Token, "/sui/wallets", Body).

import_wallet(Token, UserId, PrivateKey) ->
    import_wallet(Token, UserId, PrivateKey, #{}).

import_wallet(Token, UserId, PrivateKey, Opts) ->
    Body = maps:merge(#{user_id => UserId, private_key => PrivateKey}, Opts),
    sui_post(Token, "/sui/wallets/import", Body).

import_wallet_from_mnemonic(Token, UserId, Mnemonic) ->
    import_wallet_from_mnemonic(Token, UserId, Mnemonic, #{}).

import_wallet_from_mnemonic(Token, UserId, Mnemonic, Opts) ->
    Body = maps:merge(#{user_id => UserId, mnemonic => Mnemonic}, Opts),
    sui_post(Token, "/sui/wallets/import/mnemonic", Body).

delete_wallet(Token, UserId, WalletId) ->
    sui_delete(Token, "/sui/wallets/" ++ binary_to_list(WalletId) ++ "?user_id=" ++ binary_to_list(UserId)).

get_wallet_info(Token, WalletId) ->
    sui_get(Token, "/sui/wallets/" ++ binary_to_list(WalletId)).

get_user_wallets(Token, UserId) ->
    sui_get(Token, "/sui/wallets/user/" ++ binary_to_list(UserId)).

set_primary_wallet(Token, UserId, WalletId) ->
    sui_post(Token, "/sui/wallets/" ++ binary_to_list(WalletId) ++ "/primary", #{user_id => UserId}).

update_wallet_label(Token, WalletId, Label) ->
    sui_patch(Token, "/sui/wallets/" ++ binary_to_list(WalletId) ++ "/label", #{label => Label}).

get_sui_balance(Token, WalletId) ->
    sui_get(Token, "/sui/wallets/" ++ binary_to_list(WalletId) ++ "/balance").

get_balance_by_coin_type(Token, WalletId, CoinType) ->
    EncodedType = uri_encode(CoinType),
    sui_get(Token, "/sui/wallets/" ++ binary_to_list(WalletId) ++ "/balance/" ++ EncodedType).

get_all_balances(Token, WalletId) ->
    sui_get(Token, "/sui/wallets/" ++ binary_to_list(WalletId) ++ "/balances").

get_owned_coins(Token, WalletId) ->
    get_owned_coins(Token, WalletId, #{}).

get_owned_coins(Token, WalletId, Opts) ->
    QS = build_query_string(Opts),
    sui_get(Token, "/sui/wallets/" ++ binary_to_list(WalletId) ++ "/coins" ++ QS).

get_owned_objects(Token, WalletId) ->
    get_owned_objects(Token, WalletId, #{}).

get_owned_objects(Token, WalletId, Opts) ->
    QS = build_query_string(Opts),
    sui_get(Token, "/sui/wallets/" ++ binary_to_list(WalletId) ++ "/objects" ++ QS).

get_coin_info(Token, CoinType) ->
    get_coin_info(Token, CoinType, #{}).

get_coin_info(Token, CoinType, Opts) ->
    QS = build_query_string(Opts),
    sui_get(Token, "/sui/coins/info/" ++ uri_encode(CoinType) ++ QS).

transfer_sui(Token, SenderAddress, RecipientAddress, AmountMist, Network) ->
    transfer_sui(Token, SenderAddress, RecipientAddress, AmountMist, Network, #{}).

transfer_sui(Token, SenderAddress, RecipientAddress, AmountMist, Network, Opts) ->
    Body = maps:merge(#{
        sender_address    => SenderAddress,
        recipient_address => RecipientAddress,
        amount_mist       => integer_to_binary(AmountMist),
        network           => Network
    }, Opts),
    sui_post(Token, "/sui/transfer", Body).

batch_transfer_sui(Token, SenderAddress, Transfers, Network) ->
    batch_transfer_sui(Token, SenderAddress, Transfers, Network, #{}).

batch_transfer_sui(Token, SenderAddress, Transfers, Network, Opts) ->
    Body = maps:merge(#{
        sender_address => SenderAddress,
        transfers      => Transfers,
        network        => Network
    }, Opts),
    sui_post(Token, "/sui/transfer/batch", Body).

split_coin(Token, SenderAddress, CoinObjectId, SplitAmounts, GasBudget) ->
    split_coin(Token, SenderAddress, CoinObjectId, SplitAmounts, GasBudget, #{}).

split_coin(Token, SenderAddress, CoinObjectId, SplitAmounts, GasBudget, Opts) ->
    Body = maps:merge(#{
        sender_address  => SenderAddress,
        coin_object_id  => CoinObjectId,
        split_amounts   => SplitAmounts,
        gas_budget      => GasBudget
    }, Opts),
    sui_post(Token, "/sui/coins/split", Body).

merge_coins(Token, SenderAddress, PrimaryCoinId, CoinsToMerge, GasBudget) ->
    merge_coins(Token, SenderAddress, PrimaryCoinId, CoinsToMerge, GasBudget, #{}).

merge_coins(Token, SenderAddress, PrimaryCoinId, CoinsToMerge, GasBudget, Opts) ->
    Body = maps:merge(#{
        sender_address  => SenderAddress,
        primary_coin_id => PrimaryCoinId,
        coins_to_merge  => CoinsToMerge,
        gas_budget      => GasBudget
    }, Opts),
    sui_post(Token, "/sui/coins/merge", Body).

get_local_transaction_history(Token, WalletId) ->
    sui_get(Token, "/sui/history/" ++ binary_to_list(WalletId)).

get_local_transaction_history(Token, WalletId, Limit, Offset) ->
    QS = "?limit=" ++ integer_to_list(Limit) ++ "&offset=" ++ integer_to_list(Offset),
    sui_get(Token, "/sui/history/" ++ binary_to_list(WalletId) ++ QS).

get_local_user_history(Token, UserId) ->
    sui_get(Token, "/sui/history/user/" ++ binary_to_list(UserId)).

get_local_transaction_by_digest(Token, Digest) ->
    sui_get(Token, "/sui/history/digest/" ++ binary_to_list(Digest)).

get_transaction(Token, Digest, Network) ->
    QS = "?network=" ++ network_param(Network),
    sui_get(Token, "/sui/transactions/" ++ binary_to_list(Digest) ++ QS).

execute_transaction(Token, TransactionBytes, Signatures, Network) ->
    sui_post(Token, "/sui/transactions/execute", #{
        transaction_bytes => TransactionBytes,
        signatures        => Signatures,
        network           => network_atom(Network)
    }).

simulate_transaction(Token, TransactionBytes, Network) ->
    sui_post(Token, "/sui/transactions/simulate", #{
        transaction_bytes => TransactionBytes,
        network           => network_atom(Network)
    }).

wait_for_transaction(Token, Digest, Network) ->
    sui_post(Token, "/sui/transactions/wait", #{
        digest  => Digest,
        network => network_atom(Network)
    }).

build_transaction(Token, SenderAddress, TxKind, Params) ->
    sui_post(Token, "/sui/transactions/build", #{
        sender_address   => SenderAddress,
        transaction_kind => TxKind,
        params           => Params
    }).

execute_move_call(Token, SenderAddress, PackageId, ModuleName, FunctionName, Arguments) ->
    execute_move_call(Token, SenderAddress, PackageId, ModuleName, FunctionName, Arguments, #{}).

execute_move_call(Token, SenderAddress, PackageId, ModuleName, FunctionName, Arguments, Opts) ->
    Body = maps:merge(#{
        sender_address => SenderAddress,
        package_id     => PackageId,
        module_name    => ModuleName,
        function_name  => FunctionName,
        arguments      => Arguments
    }, Opts),
    sui_post(Token, "/sui/move/call", Body).

publish_package(Token, SenderAddress, CompiledModules, Dependencies, Opts) ->
    Body = maps:merge(#{
        sender_address   => SenderAddress,
        compiled_modules => CompiledModules,
        dependencies     => Dependencies
    }, Opts),
    sui_post(Token, "/sui/move/publish", Body).

upgrade_package(Token, SenderAddress, PackageId, CompiledModules, UpgradeCapId, Opts) ->
    Body = maps:merge(#{
        sender_address   => SenderAddress,
        package_id       => PackageId,
        compiled_modules => CompiledModules,
        upgrade_cap_id   => UpgradeCapId
    }, Opts),
    sui_post(Token, "/sui/move/upgrade", Body).

get_validators(Token, Network) ->
    QS = "?network=" ++ network_param(Network),
    sui_get(Token, "/sui/staking/validators" ++ QS).

get_validator(Token, ValidatorAddress, Network) ->
    QS = "?network=" ++ network_param(Network),
    sui_get(Token, "/sui/staking/validators/" ++ binary_to_list(ValidatorAddress) ++ QS).

get_epoch(Token, Network) ->
    QS = "?network=" ++ network_param(Network),
    sui_get(Token, "/sui/staking/epoch" ++ QS).

stake_with_validator(Token, SenderAddress, ValidatorAddress, AmountMist, Network) ->
    stake_with_validator(Token, SenderAddress, ValidatorAddress, AmountMist, Network, #{}).

stake_with_validator(Token, SenderAddress, ValidatorAddress, AmountMist, Network, Opts) ->
    Body = maps:merge(#{
        sender_address    => SenderAddress,
        validator_address => ValidatorAddress,
        amount_mist       => integer_to_binary(AmountMist),
        network           => network_atom(Network)
    }, Opts),
    sui_post(Token, "/sui/staking/stake", Body).

unstake_from_validator(Token, SenderAddress, StakedSuiId, Network) ->
    unstake_from_validator(Token, SenderAddress, StakedSuiId, Network, #{}).

unstake_from_validator(Token, SenderAddress, StakedSuiId, Network, Opts) ->
    Body = maps:merge(#{
        sender_address => SenderAddress,
        staked_sui_id  => StakedSuiId,
        network        => network_atom(Network)
    }, Opts),
    sui_post(Token, "/sui/staking/unstake", Body).

get_staked_sui(Token, OwnerAddress, Network) ->
    QS = "?network=" ++ network_param(Network),
    sui_get(Token, "/sui/staking/staked/" ++ binary_to_list(OwnerAddress) ++ QS).

get_staking_rewards(Token, OwnerAddress, Network) ->
    QS = "?network=" ++ network_param(Network),
    sui_get(Token, "/sui/staking/rewards/" ++ binary_to_list(OwnerAddress) ++ QS).

get_object(Token, ObjectId, Network) ->
    QS = "?network=" ++ network_param(Network),
    sui_get(Token, "/sui/objects/" ++ binary_to_list(ObjectId) ++ QS).

get_objects(Token, ObjectIds, Network) ->
    sui_post(Token, "/sui/objects/batch", #{
        object_ids => ObjectIds,
        network    => network_atom(Network)
    }).

get_dynamic_fields(Token, ObjectId, Network) ->
    QS = "?network=" ++ network_param(Network),
    sui_get(Token, "/sui/objects/" ++ binary_to_list(ObjectId) ++ "/dynamic-fields" ++ QS).

mint_nft(Token, SenderAddress, Name, Description, Url, Network) ->
    mint_nft(Token, SenderAddress, Name, Description, Url, Network, #{}).

mint_nft(Token, SenderAddress, Name, Description, Url, Network, Opts) ->
    Body = maps:merge(#{
        sender_address => SenderAddress,
        name           => Name,
        description    => Description,
        url            => Url,
        network        => network_atom(Network)
    }, Opts),
    sui_post(Token, "/sui/nft/mint", Body).

transfer_nft(Token, SenderAddress, NftObjectId, RecipientAddress, Network) ->
    sui_post(Token, "/sui/nft/transfer", #{
        sender_address    => SenderAddress,
        nft_object_id     => NftObjectId,
        recipient_address => RecipientAddress,
        network           => network_atom(Network)
    }).

get_nfts(Token, OwnerAddress, Network) ->
    QS = "?network=" ++ network_param(Network),
    sui_get(Token, "/sui/nft/" ++ binary_to_list(OwnerAddress) ++ QS).

sign_message(Token, WalletId, Message) ->
    sui_post(Token, "/sui/wallets/" ++ binary_to_list(WalletId) ++ "/sign", #{message => Message}).

verify_signature(Token, Message, Signature, Address) ->
    sui_post(Token, "/sui/verify/signature", #{
        message   => Message,
        signature => Signature,
        address   => Address
    }).

create_multisig(Token, PublicKeys, Threshold, Network) ->
    sui_post(Token, "/sui/multisig/create", #{
        public_keys => PublicKeys,
        threshold   => Threshold,
        network     => network_atom(Network)
    }).

combine_multisig(Token, PartialSignatures, TransactionBytes, PublicKeys, Threshold) ->
    sui_post(Token, "/sui/multisig/combine", #{
        partial_signatures => PartialSignatures,
        transaction_bytes  => TransactionBytes,
        public_keys        => PublicKeys,
        threshold          => Threshold
    }).

lookup_name(Token, Name, Network) ->
    QS = "?name=" ++ uri_encode(Name) ++ "&network=" ++ network_param(Network),
    sui_get(Token, "/sui/names/lookup" ++ QS).

reverse_lookup_name(Token, Address, Network) ->
    QS = "?network=" ++ network_param(Network),
    sui_get(Token, "/sui/names/reverse/" ++ binary_to_list(Address) ++ QS).

get_chain_info(Token, Network) ->
    QS = "?network=" ++ network_param(Network),
    sui_get(Token, "/sui/chain/info" ++ QS).

get_reference_gas_price(Token, Network) ->
    QS = "?network=" ++ network_param(Network),
    sui_get(Token, "/sui/chain/gas-price" ++ QS).

get_checkpoint(Token, SequenceNumber, Network) ->
    QS = "?sequence_number=" ++ integer_to_list(SequenceNumber) ++ "&network=" ++ network_param(Network),
    sui_get(Token, "/sui/chain/checkpoint" ++ QS).

request_faucet(Token, RecipientAddress, Network) ->
    sui_post(Token, "/sui/faucet", #{
        recipient_address => RecipientAddress,
        network           => network_atom(Network)
    }).

network_param(mainnet)  -> "mainnet";
network_param(testnet)  -> "testnet";
network_param(devnet)   -> "devnet";
network_param(localnet) -> "localnet";
network_param(Bin) when is_binary(Bin) -> binary_to_list(Bin);
network_param(Str) when is_list(Str)   -> Str.

network_atom(mainnet)  -> <<"mainnet">>;
network_atom(testnet)  -> <<"testnet">>;
network_atom(devnet)   -> <<"devnet">>;
network_atom(localnet) -> <<"localnet">>;
network_atom(Bin) when is_binary(Bin) -> Bin;
network_atom(Str) when is_list(Str)   -> list_to_binary(Str).

uri_encode(Bin) when is_binary(Bin) ->
    http_uri:encode(binary_to_list(Bin));
uri_encode(Str) when is_list(Str) ->
    http_uri:encode(Str).

integer_to_binary(N) when is_integer(N) ->
    list_to_binary(integer_to_list(N));
integer_to_binary(B) when is_binary(B) ->
    B.

build_query_string(Opts) when map_size(Opts) =:= 0 ->
    "";
build_query_string(Opts) ->
    Parts = maps:fold(fun(K, V, Acc) ->
        KStr = atom_to_list(K),
        VStr = case V of
            B when is_binary(B) -> binary_to_list(B);
            I when is_integer(I) -> integer_to_list(I);
            A when is_atom(A)   -> atom_to_list(A);
            L when is_list(L)   -> L
        end,
        [KStr ++ "=" ++ VStr | Acc]
    end, [], Opts),
    "?" ++ string:join(Parts, "&").
