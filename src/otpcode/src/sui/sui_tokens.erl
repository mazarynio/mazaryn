-module(sui_tokens).
-author("Zaryn Technologies").
-include("../records.hrl").
-include("../wallet_records.hrl").

-export([
    get_sui_balance/3,
    get_balance_by_coin_type/4,
    get_all_balances/3,
    get_coin_info/3,
    get_coin_info/4,
    sync_coin_balance/4,
    sync_all_balances/3,
    get_local_coin_balance/3,
    get_local_all_coin_balances/2,

    transfer_sui/5,
    transfer_sui/6,
    batch_transfer_sui/4,
    batch_transfer_sui/5,
    split_coin/6,
    merge_coins/6,

    get_object/4,
    get_objects/4,
    get_owned_objects/3,
    get_owned_objects/4,
    get_dynamic_fields/4,
    sync_object/4,
    get_local_objects/2,
    get_local_nfts/2,

    mint_nft/6,
    mint_nft/7,
    transfer_nft/5,
    get_nfts/4,

    execute_move_call/6,
    execute_move_call/7,
    build_transaction/5,

    sign_message/4,
    verify_signature/5,
    lookup_name/4,
    reverse_lookup_name/4,

    get_chain_info/3,
    get_reference_gas_price/3,
    get_checkpoint/4,
    get_transaction/4
]).

get_sui_balance(Token, WalletId, _Network) ->
    sui_wallet:get_sui_balance(Token, WalletId).

get_balance_by_coin_type(Token, WalletId, CoinType, _Network) ->
    sui_wallet:get_balance_by_coin_type(Token, WalletId, CoinType).

get_all_balances(Token, WalletId, _Network) ->
    sui_wallet:get_all_balances(Token, WalletId).

get_coin_info(Token, CoinType, Network) ->
    get_coin_info(Token, CoinType, Network, #{}).

get_coin_info(Token, CoinType, Network, Opts) ->
    sui_wallet:get_coin_info(Token, CoinType, maps:merge(#{network => Network}, Opts)).

sync_coin_balance(Token, WalletId, CoinType, _Network) ->
    case sui_walletdb:get_wallet(WalletId) of
        {error, _} ->
            {error, wallet_not_found};
        {ok, Wallet} ->
            Network = Wallet#sui_wallet.network,
            case sui_wallet:get_balance_by_coin_type(Token, WalletId, CoinType) of
                {ok, #{<<"total_balance">> := Balance,
                       <<"coin_object_count">> := Count}} ->
                    sui_walletdb:upsert_coin_balance(WalletId, #{
                        coin_type         => CoinType,
                        total_balance     => Balance,
                        coin_object_count => Count,
                        owner_address     => Wallet#sui_wallet.sui_address,
                        network           => Network
                    });
                {error, Reason} ->
                    {error, Reason}
            end
    end.

sync_all_balances(Token, WalletId, _Network) ->
    case sui_walletdb:get_wallet(WalletId) of
        {error, _} ->
            {error, wallet_not_found};
        {ok, Wallet} ->
            Network = Wallet#sui_wallet.network,
            case sui_wallet:get_all_balances(Token, WalletId) of
                {ok, #{<<"balances">> := Balances}} ->
                    lists:foreach(fun(B) ->
                        CoinType = maps:get(<<"coin_type">>, B, <<>>),
                        Balance  = maps:get(<<"total_balance">>, B, <<"0">>),
                        Count    = maps:get(<<"coin_object_count">>, B, 0),
                        sui_walletdb:upsert_coin_balance(WalletId, #{
                            coin_type         => CoinType,
                            total_balance     => Balance,
                            coin_object_count => Count,
                            owner_address     => Wallet#sui_wallet.sui_address,
                            network           => Network
                        })
                    end, Balances),
                    {ok, #{synced => length(Balances)}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

get_local_coin_balance(WalletId, _OwnerAddress, CoinType) ->
    sui_walletdb:get_coin_balance(WalletId, CoinType).

get_local_all_coin_balances(WalletId, _OwnerAddress) ->
    sui_walletdb:get_wallet_coin_balances(WalletId).

transfer_sui(Token, WalletId, RecipientAddress, AmountMist, Network) ->
    transfer_sui(Token, WalletId, RecipientAddress, AmountMist, Network, #{}).

transfer_sui(Token, WalletId, RecipientAddress, AmountMist, Network, Opts) ->
    case sui_walletdb:get_wallet(WalletId) of
        {error, _} ->
            {error, wallet_not_found};
        {ok, Wallet} ->
            SenderAddress = Wallet#sui_wallet.sui_address,
            case sui_wallet:transfer_sui(Token, SenderAddress, RecipientAddress, AmountMist, Network, Opts) of
                {ok, #{<<"digest">> := Digest, <<"gas_used">> := GasUsed} = Result} ->
                    TxParams = #{
                        digest             => Digest,
                        tx_type            => sui_transfer,
                        sender_address     => SenderAddress,
                        recipient_address  => RecipientAddress,
                        amount_mist        => integer_to_binary(AmountMist),
                        coin_type          => <<"0x2::sui::SUI">>,
                        gas_computation    => maps:get(<<"computation_cost">>, GasUsed, <<"0">>),
                        gas_storage        => maps:get(<<"storage_cost">>, GasUsed, <<"0">>),
                        gas_storage_rebate => maps:get(<<"storage_rebate">>, GasUsed, <<"0">>),
                        status             => success,
                        effects_status     => <<"success">>,
                        network            => Network
                    },
                    sui_walletdb:create_transaction(WalletId, TxParams),
                    sui_walletdb:update_wallet_last_used(WalletId),
                    {ok, Result};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

batch_transfer_sui(Token, WalletId, Transfers, Network) ->
    batch_transfer_sui(Token, WalletId, Transfers, Network, #{}).

batch_transfer_sui(Token, WalletId, Transfers, Network, Opts) ->
    case sui_walletdb:get_wallet(WalletId) of
        {error, _} ->
            {error, wallet_not_found};
        {ok, Wallet} ->
            SenderAddress = Wallet#sui_wallet.sui_address,
            case sui_wallet:batch_transfer_sui(Token, SenderAddress, Transfers, Network, Opts) of
                {ok, #{<<"digest">> := Digest, <<"gas_used">> := GasUsed} = Result} ->
                    TxParams = #{
                        digest             => Digest,
                        tx_type            => batch_transfer,
                        sender_address     => SenderAddress,
                        gas_computation    => maps:get(<<"computation_cost">>, GasUsed, <<"0">>),
                        gas_storage        => maps:get(<<"storage_cost">>, GasUsed, <<"0">>),
                        gas_storage_rebate => maps:get(<<"storage_rebate">>, GasUsed, <<"0">>),
                        status             => success,
                        effects_status     => <<"success">>,
                        network            => Network
                    },
                    sui_walletdb:create_transaction(WalletId, TxParams),
                    {ok, Result};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

split_coin(Token, WalletId, CoinObjectId, SplitAmounts, GasBudget, Network) ->
    case sui_walletdb:get_wallet(WalletId) of
        {error, _} ->
            {error, wallet_not_found};
        {ok, Wallet} ->
            SenderAddress = Wallet#sui_wallet.sui_address,
            case sui_wallet:split_coin(Token, SenderAddress, CoinObjectId, SplitAmounts, GasBudget) of
                {ok, #{<<"digest">> := Digest, <<"gas_used">> := GasUsed} = Result} ->
                    TxParams = #{
                        digest             => Digest,
                        tx_type            => split_coin,
                        sender_address     => SenderAddress,
                        object_id          => CoinObjectId,
                        gas_computation    => maps:get(<<"computation_cost">>, GasUsed, <<"0">>),
                        gas_storage        => maps:get(<<"storage_cost">>, GasUsed, <<"0">>),
                        gas_storage_rebate => maps:get(<<"storage_rebate">>, GasUsed, <<"0">>),
                        status             => success,
                        effects_status     => <<"success">>,
                        network            => Network
                    },
                    sui_walletdb:create_transaction(WalletId, TxParams),
                    {ok, Result};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

merge_coins(Token, WalletId, PrimaryCoinId, CoinsToMerge, GasBudget, Network) ->
    case sui_walletdb:get_wallet(WalletId) of
        {error, _} ->
            {error, wallet_not_found};
        {ok, Wallet} ->
            SenderAddress = Wallet#sui_wallet.sui_address,
            case sui_wallet:merge_coins(Token, SenderAddress, PrimaryCoinId, CoinsToMerge, GasBudget) of
                {ok, #{<<"digest">> := Digest, <<"gas_used">> := GasUsed} = Result} ->
                    TxParams = #{
                        digest             => Digest,
                        tx_type            => merge_coins,
                        sender_address     => SenderAddress,
                        object_id          => PrimaryCoinId,
                        gas_computation    => maps:get(<<"computation_cost">>, GasUsed, <<"0">>),
                        gas_storage        => maps:get(<<"storage_cost">>, GasUsed, <<"0">>),
                        gas_storage_rebate => maps:get(<<"storage_rebate">>, GasUsed, <<"0">>),
                        status             => success,
                        effects_status     => <<"success">>,
                        network            => Network
                    },
                    sui_walletdb:create_transaction(WalletId, TxParams),
                    {ok, Result};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

get_object(Token, _WalletId, ObjectId, Network) ->
    sui_wallet:get_object(Token, ObjectId, Network).

get_objects(Token, _WalletId, ObjectIds, Network) ->
    sui_wallet:get_objects(Token, ObjectIds, Network).

get_owned_objects(Token, WalletId, Network) ->
    get_owned_objects(Token, WalletId, Network, #{}).

get_owned_objects(Token, WalletId, _Network, Opts) ->
    sui_wallet:get_owned_objects(Token, WalletId, Opts).

get_dynamic_fields(Token, _WalletId, ObjectId, Network) ->
    sui_wallet:get_dynamic_fields(Token, ObjectId, Network).

sync_object(Token, WalletId, ObjectId, Network) ->
    case sui_walletdb:get_wallet(WalletId) of
        {error, _} ->
            {error, wallet_not_found};
        {ok, Wallet} ->
            case sui_wallet:get_object(Token, ObjectId, Network) of
                {ok, #{<<"object_id">> := ObjId,
                       <<"type">> := ObjType,
                       <<"version">> := Version,
                       <<"digest">> := Digest} = ObjData} ->
                    IsNft = not (
                        binary:match(ObjType, <<"0x2::sui::SUI">>) =/= nomatch orelse
                        binary:match(ObjType, <<"staking_pool::StakedSui">>) =/= nomatch orelse
                        ObjType =:= <<"package">>
                    ),
                    ObjParams = #{
                        object_id            => ObjId,
                        owner_address        => Wallet#sui_wallet.sui_address,
                        object_type          => ObjType,
                        version              => Version,
                        digest               => Digest,
                        balance              => maps:get(<<"balance">>, ObjData, undefined),
                        previous_transaction => maps:get(<<"previous_transaction">>, ObjData, undefined),
                        storage_rebate       => maps:get(<<"storage_rebate">>, ObjData, undefined),
                        is_nft               => IsNft,
                        nft_name             => maps:get(<<"name">>, ObjData, undefined),
                        nft_description      => maps:get(<<"description">>, ObjData, undefined),
                        nft_url              => maps:get(<<"url">>, ObjData, undefined),
                        network              => Network
                    },
                    sui_walletdb:upsert_object(WalletId, ObjParams);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

get_local_objects(WalletId, _OwnerAddress) ->
    sui_walletdb:get_wallet_objects(WalletId).

get_local_nfts(WalletId, _OwnerAddress) ->
    sui_walletdb:get_wallet_nfts(WalletId).

mint_nft(Token, WalletId, Name, Description, Url, Network) ->
    mint_nft(Token, WalletId, Name, Description, Url, Network, #{}).

mint_nft(Token, WalletId, Name, Description, Url, Network, Opts) ->
    case sui_walletdb:get_wallet(WalletId) of
        {error, _} ->
            {error, wallet_not_found};
        {ok, Wallet} ->
            SenderAddress = Wallet#sui_wallet.sui_address,
            case sui_wallet:mint_nft(Token, SenderAddress, Name, Description, Url, Network, Opts) of
                {ok, #{<<"digest">> := Digest,
                       <<"nft_object_id">> := NftObjectId,
                       <<"gas_used">> := GasUsed} = Result} ->
                    ObjParams = #{
                        object_id       => NftObjectId,
                        owner_address   => SenderAddress,
                        object_type     => maps:get(package_id, Opts, <<"0x2::devnet_nft::DevNetNFT">>),
                        is_nft          => true,
                        nft_name        => Name,
                        nft_description => Description,
                        nft_url         => Url,
                        network         => Network
                    },
                    sui_walletdb:upsert_object(WalletId, ObjParams),
                    TxParams = #{
                        digest             => Digest,
                        tx_type            => nft_mint,
                        sender_address     => SenderAddress,
                        object_id          => NftObjectId,
                        gas_computation    => maps:get(<<"computation_cost">>, GasUsed, <<"0">>),
                        gas_storage        => maps:get(<<"storage_cost">>, GasUsed, <<"0">>),
                        gas_storage_rebate => maps:get(<<"storage_rebate">>, GasUsed, <<"0">>),
                        status             => success,
                        effects_status     => <<"success">>,
                        network            => Network
                    },
                    sui_walletdb:create_transaction(WalletId, TxParams),
                    sui_walletdb:update_wallet_last_used(WalletId),
                    {ok, Result};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

transfer_nft(Token, WalletId, NftObjectId, RecipientAddress, Network) ->
    case sui_walletdb:get_wallet(WalletId) of
        {error, _} ->
            {error, wallet_not_found};
        {ok, Wallet} ->
            SenderAddress = Wallet#sui_wallet.sui_address,
            case sui_wallet:transfer_nft(Token, SenderAddress, NftObjectId, RecipientAddress, Network) of
                {ok, #{<<"digest">> := Digest, <<"gas_used">> := GasUsed} = Result} ->
                    sui_walletdb:delete_object(NftObjectId),
                    TxParams = #{
                        digest             => Digest,
                        tx_type            => nft_transfer,
                        sender_address     => SenderAddress,
                        recipient_address  => RecipientAddress,
                        object_id          => NftObjectId,
                        gas_computation    => maps:get(<<"computation_cost">>, GasUsed, <<"0">>),
                        gas_storage        => maps:get(<<"storage_cost">>, GasUsed, <<"0">>),
                        gas_storage_rebate => maps:get(<<"storage_rebate">>, GasUsed, <<"0">>),
                        status             => success,
                        effects_status     => <<"success">>,
                        network            => Network
                    },
                    sui_walletdb:create_transaction(WalletId, TxParams),
                    sui_walletdb:update_wallet_last_used(WalletId),
                    {ok, Result};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

get_nfts(Token, _WalletId, OwnerAddress, Network) ->
    sui_wallet:get_nfts(Token, OwnerAddress, Network).

execute_move_call(Token, WalletId, PackageId, ModuleName, FunctionName, Arguments) ->
    execute_move_call(Token, WalletId, PackageId, ModuleName, FunctionName, Arguments, #{}).

execute_move_call(Token, WalletId, PackageId, ModuleName, FunctionName, Arguments, Opts) ->
    case sui_walletdb:get_wallet(WalletId) of
        {error, _} ->
            {error, wallet_not_found};
        {ok, Wallet} ->
            SenderAddress = Wallet#sui_wallet.sui_address,
            Network       = Wallet#sui_wallet.network,
            case sui_wallet:execute_move_call(
                Token, SenderAddress, PackageId, ModuleName, FunctionName, Arguments, Opts
            ) of
                {ok, #{<<"digest">> := Digest, <<"gas_used">> := GasUsed} = Result} ->
                    TxParams = #{
                        digest             => Digest,
                        tx_type            => move_call,
                        sender_address     => SenderAddress,
                        package_id         => PackageId,
                        module_name        => ModuleName,
                        function_name      => FunctionName,
                        gas_computation    => maps:get(<<"computation_cost">>, GasUsed, <<"0">>),
                        gas_storage        => maps:get(<<"storage_cost">>, GasUsed, <<"0">>),
                        gas_storage_rebate => maps:get(<<"storage_rebate">>, GasUsed, <<"0">>),
                        status             => success,
                        effects_status     => <<"success">>,
                        network            => Network
                    },
                    sui_walletdb:create_transaction(WalletId, TxParams),
                    sui_walletdb:update_wallet_last_used(WalletId),
                    {ok, Result};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

build_transaction(Token, WalletId, TxKind, Params, _Network) ->
    case sui_walletdb:get_wallet(WalletId) of
        {error, _} ->
            {error, wallet_not_found};
        {ok, Wallet} ->
            SenderAddress = Wallet#sui_wallet.sui_address,
            sui_wallet:build_transaction(Token, SenderAddress, TxKind, Params)
    end.

sign_message(Token, WalletId, Message, _Network) ->
    sui_wallet:sign_message(Token, WalletId, Message).

verify_signature(Token, Message, Signature, Address, Network) ->
    sui_wallet:verify_signature(Token, Message, Signature, Address).

lookup_name(Token, Name, Network, _Opts) ->
    sui_wallet:lookup_name(Token, Name, Network).

reverse_lookup_name(Token, Address, Network, _Opts) ->
    sui_wallet:reverse_lookup_name(Token, Address, Network).

get_chain_info(Token, _WalletId, Network) ->
    sui_wallet:get_chain_info(Token, Network).

get_reference_gas_price(Token, _WalletId, Network) ->
    sui_wallet:get_reference_gas_price(Token, Network).

get_checkpoint(Token, _WalletId, SequenceNumber, Network) ->
    sui_wallet:get_checkpoint(Token, SequenceNumber, Network).

get_transaction(Token, _WalletId, Digest, Network) ->
    sui_wallet:get_transaction(Token, Digest, Network).

integer_to_binary(N) when is_integer(N) ->
    list_to_binary(integer_to_list(N));
integer_to_binary(B) when is_binary(B) ->
    B.
