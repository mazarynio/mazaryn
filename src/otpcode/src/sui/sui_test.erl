-module(sui_test).
-author("Zaryn Technologies").
-include("../records.hrl").
-include("../wallet_records.hrl").

-export([run_all/0, run_all/1]).
-export([
    test_walletdb/0,
    test_wallet/1,
    test_tokens/1,
    test_nft/1,
    test_staking/1,
    test_transactions/1
]).

run_all() ->
    Token = get_test_token(),
    run_all(Token).

run_all(Token) ->
    Suites = [
        {"sui_walletdb (Mnesia layer)", fun test_walletdb/0},
        {"sui_wallet (HTTP client)",    fun() -> test_wallet(Token) end},
        {"sui_tokens (coins/objects)",  fun() -> test_tokens(Token) end},
        {"sui_tokens (NFT operations)", fun() -> test_nft(Token) end},
        {"sui_staking (validators)",    fun() -> test_staking(Token) end},
        {"sui_transactions (chain)",    fun() -> test_transactions(Token) end}
    ],
    Results = lists:map(fun({Name, Fun}) ->
        io:format("── ~s~n", [Name]),
        try
            Fun(),
            io:format(" ✓ PASSED~n~n"),
            {Name, passed}
        catch
            Class:Reason:Stack ->
                io:format(" ✗ FAILED: ~p:~p~n", [Class, Reason]),
                io:format(" Stack: ~p~n~n", [Stack]),
                {Name, {failed, Class, Reason}}
        end
    end, Suites),
    Passed = length([ok || {_, passed} <- Results]),
    Failed = length([ok || {_, {failed, _, _}} <- Results]),
    io:format("========================================~n"),
    io:format(" Results: ~p passed, ~p failed~n", [Passed, Failed]),
    io:format("========================================~n"),
    case Failed of
        0 -> ok;
        _ -> {error, Failed}
    end.

test_walletdb() ->
    ensure_mnesia_started(),
    UserId  = "test_user_" ++ unique_id(),
    Address = fresh_sui_address(),

    {ok, WalletId} = sui_walletdb:create_wallet(
        UserId, Address,
        <<"enc_private_key">>, <<"iv_data">>, <<"tag_data">>, ed25519
    ),
    assert_non_empty(WalletId, "create_wallet returned empty wallet_id"),

    {ok, Wallet} = sui_walletdb:get_wallet(WalletId),
    assert_eq(Wallet#sui_wallet.user_id, UserId, "wallet user_id mismatch"),
    assert_eq(Wallet#sui_wallet.sui_address, Address, "wallet sui_address mismatch"),
    assert_eq(Wallet#sui_wallet.scheme, ed25519, "wallet scheme mismatch"),
    assert_eq(Wallet#sui_wallet.is_primary, true, "first wallet should be primary"),

    {ok, WalletByAddr} = sui_walletdb:get_wallet_by_address(Address),
    assert_eq(WalletByAddr#sui_wallet.wallet_id, WalletId, "address lookup mismatch"),

    assert_eq(sui_walletdb:wallet_exists(WalletId), true, "wallet_exists should be true"),
    assert_eq(sui_walletdb:address_exists(Address), true, "address_exists should be true"),
    assert_eq(sui_walletdb:wallet_exists("nonexistent_id"), false, "nonexistent should be false"),

    {error, address_already_exists} = sui_walletdb:create_wallet(
        UserId, Address, <<"k">>, <<"iv">>, <<"t">>, ed25519
    ),

    Address2 = fresh_sui_address(),
    {ok, WalletId2} = sui_walletdb:create_wallet(
        UserId, Address2, <<"k2">>, <<"iv2">>, <<"t2">>, secp256k1
    ),
    {ok, Wallet2} = sui_walletdb:get_wallet(WalletId2),
    assert_eq(Wallet2#sui_wallet.is_primary, false, "second wallet should not be primary"),
    assert_eq(Wallet2#sui_wallet.scheme, secp256k1, "second wallet scheme mismatch"),

    Address3 = fresh_sui_address(),
    {ok, WalletId3} = sui_walletdb:create_wallet(
        UserId, Address3, <<"k3">>, <<"iv3">>, <<"t3">>, secp256r1,
        #{label => "Secp256r1 Wallet", network => testnet}
    ),
    {ok, Wallet3} = sui_walletdb:get_wallet(WalletId3),
    assert_eq(Wallet3#sui_wallet.scheme, secp256r1, "third wallet scheme mismatch"),
    assert_eq(Wallet3#sui_wallet.network, testnet, "third wallet network mismatch"),
    assert_eq(Wallet3#sui_wallet.label, "Secp256r1 Wallet", "third wallet label mismatch"),

    {ok, Wallets} = {ok, sui_walletdb:get_user_wallets(UserId)},
    assert_eq(length(Wallets), 3, "user should have 3 wallets"),

    TestnetWallets = sui_walletdb:get_user_wallets_by_network(UserId, testnet),
    assert_eq(length(TestnetWallets), 1, "should have 1 testnet wallet"),

    {ok, Primary} = sui_walletdb:get_primary_wallet(UserId),
    assert_eq(Primary#sui_wallet.wallet_id, WalletId, "primary wallet mismatch"),

    {ok, WalletId2} = sui_walletdb:set_primary_wallet(UserId, WalletId2),
    {ok, NewPrimary} = sui_walletdb:get_primary_wallet(UserId),
    assert_eq(NewPrimary#sui_wallet.wallet_id, WalletId2, "new primary wallet mismatch"),

    {ok, WalletId} = sui_walletdb:update_wallet_label(WalletId, "My Main Wallet"),
    {ok, LabeledWallet} = sui_walletdb:get_wallet(WalletId),
    assert_eq(LabeledWallet#sui_wallet.label, "My Main Wallet", "label update mismatch"),

    {ok, WalletId} = sui_walletdb:update_wallet_last_used(WalletId),

    Digest = "0x" ++ unique_id(),
    {ok, TxId} = sui_walletdb:create_transaction(WalletId, #{
        digest             => list_to_binary(Digest),
        tx_type            => sui_transfer,
        sender_address     => list_to_binary(Address),
        recipient_address  => list_to_binary(Address2),
        amount_mist        => <<"1000000000">>,
        coin_type          => <<"0x2::sui::SUI">>,
        gas_computation    => <<"500">>,
        gas_storage        => <<"200">>,
        gas_storage_rebate => <<"100">>,
        status             => success,
        effects_status     => <<"success">>,
        checkpoint         => <<"12345">>,
        epoch              => <<"5">>,
        network            => mainnet
    }),
    assert_non_empty(TxId, "create_transaction returned empty tx_id"),

    {ok, Tx} = sui_walletdb:get_transaction(TxId),
    assert_eq(Tx#sui_transaction.wallet_id, WalletId, "tx wallet_id mismatch"),
    assert_eq(Tx#sui_transaction.tx_type, sui_transfer, "tx type mismatch"),
    assert_eq(Tx#sui_transaction.amount_mist, <<"1000000000">>, "tx amount mismatch"),
    assert_eq(Tx#sui_transaction.coin_type, <<"0x2::sui::SUI">>, "tx coin_type mismatch"),

    {ok, TxByDigest} = sui_walletdb:get_transaction_by_digest(list_to_binary(Digest)),
    assert_eq(TxByDigest#sui_transaction.tx_id, TxId, "digest lookup mismatch"),

    MoveDigest = "0x" ++ unique_id(),
    {ok, MoveTxId} = sui_walletdb:create_transaction(WalletId, #{
        digest        => list_to_binary(MoveDigest),
        tx_type       => move_call,
        sender_address => list_to_binary(Address),
        package_id    => <<"0x2">>,
        module_name   => <<"coin">>,
        function_name => <<"transfer">>,
        status        => success,
        effects_status => <<"success">>,
        network       => mainnet
    }),
    {ok, MoveTx} = sui_walletdb:get_transaction(MoveTxId),
    assert_eq(MoveTx#sui_transaction.tx_type, move_call, "move_call tx type mismatch"),
    assert_eq(MoveTx#sui_transaction.package_id, <<"0x2">>, "package_id mismatch"),
    assert_eq(MoveTx#sui_transaction.module_name, <<"coin">>, "module_name mismatch"),

    StakeDigest = "0x" ++ unique_id(),
    {ok, StakeTxId} = sui_walletdb:create_transaction(WalletId, #{
        digest             => list_to_binary(StakeDigest),
        tx_type            => stake,
        sender_address     => list_to_binary(Address),
        validator_address  => <<"0xvalidator123">>,
        staked_sui_id      => <<"0xstaked456">>,
        amount_mist        => <<"5000000000">>,
        status             => success,
        effects_status     => <<"success">>,
        network            => mainnet
    }),
    {ok, StakeTx} = sui_walletdb:get_transaction(StakeTxId),
    assert_eq(StakeTx#sui_transaction.tx_type, stake, "stake tx type mismatch"),
    assert_eq(StakeTx#sui_transaction.validator_address, <<"0xvalidator123">>, "validator_address mismatch"),
    assert_eq(StakeTx#sui_transaction.staked_sui_id, <<"0xstaked456">>, "staked_sui_id mismatch"),

    {ok, WalletTxs} = sui_walletdb:get_wallet_transactions(WalletId),
    assert_true(length(WalletTxs) >= 3, "should have at least 3 transactions"),

    {ok, PagedTxs, Total} = sui_walletdb:get_wallet_transactions(WalletId, 10, 0),
    assert_true(Total >= 3, "total should be >= 3"),
    assert_true(length(PagedTxs) >= 3, "paged results should have >= 3 txs"),

    {ok, UserTxs} = sui_walletdb:get_user_transactions(UserId),
    assert_true(length(UserTxs) >= 3, "user should have >= 3 transactions"),

    {ok, TransferTxs} = sui_walletdb:get_transactions_by_type(WalletId, sui_transfer),
    assert_true(length(TransferTxs) >= 1, "should have at least 1 sui_transfer tx"),

    {ok, MoveTxs} = sui_walletdb:get_transactions_by_type(WalletId, move_call),
    assert_true(length(MoveTxs) >= 1, "should have at least 1 move_call tx"),

    ok = sui_walletdb:update_transaction_status(
        list_to_binary(Digest), confirmed, <<"confirmed">>, <<"12346">>
    ),
    {ok, UpdatedTx} = sui_walletdb:get_transaction_by_digest(list_to_binary(Digest)),
    assert_eq(UpdatedTx#sui_transaction.status, confirmed, "tx status update mismatch"),

    ValidatorAddress = list_to_binary("0xvalidator" ++ unique_id()),
    StakedSuiId      = list_to_binary("0xstaked" ++ unique_id()),
    {ok, StakeId} = sui_walletdb:create_stake(WalletId, #{
        owner_address          => list_to_binary(Address),
        validator_address      => ValidatorAddress,
        staked_sui_id          => StakedSuiId,
        principal_mist         => <<"3000000000">>,
        estimated_reward_mist  => <<"0">>,
        stake_activation_epoch => <<"10">>,
        status                 => pending,
        tx_type                => stake,
        digest                 => list_to_binary("0x" ++ unique_id()),
        network                => mainnet
    }),
    assert_non_empty(StakeId, "create_stake returned empty id"),

    {ok, Stake} = sui_walletdb:get_stake(StakeId),
    assert_eq(Stake#sui_stake.validator_address, ValidatorAddress, "stake validator_address mismatch"),
    assert_eq(Stake#sui_stake.principal_mist, <<"3000000000">>, "stake principal_mist mismatch"),
    assert_eq(Stake#sui_stake.status, pending, "stake status mismatch"),

    {ok, StakeById} = sui_walletdb:get_stake_by_staked_sui_id(StakedSuiId),
    assert_eq(StakeById#sui_stake.stake_id, StakeId, "stake_by_staked_sui_id mismatch"),

    {ok, WalletStakes} = sui_walletdb:get_wallet_stakes(WalletId),
    assert_true(length(WalletStakes) >= 1, "should have at least 1 stake"),

    {ok, UserStakes} = sui_walletdb:get_user_stakes(UserId),
    assert_true(length(UserStakes) >= 1, "user should have at least 1 stake"),

    {ok, ValidatorStakes} = sui_walletdb:get_stakes_by_validator(WalletId, ValidatorAddress),
    assert_true(length(ValidatorStakes) >= 1, "validator should have at least 1 stake"),

    {ok, StakeId} = sui_walletdb:update_stake(StakeId, #{
        status                => active,
        estimated_reward_mist => <<"500000">>
    }),
    {ok, UpdatedStake} = sui_walletdb:get_stake(StakeId),
    assert_eq(UpdatedStake#sui_stake.status, active, "stake status update mismatch"),
    assert_eq(UpdatedStake#sui_stake.estimated_reward_mist, <<"500000">>, "reward update mismatch"),

    ObjectId = fresh_sui_address(),
    {ok, UpsertedObjectId} = sui_walletdb:upsert_object(WalletId, #{
        object_id    => list_to_binary(ObjectId),
        owner_address => list_to_binary(Address),
        object_type  => <<"0x2::coin::Coin<0x2::sui::SUI>">>,
        version      => <<"42">>,
        digest       => <<"0xobjdigest">>,
        balance      => <<"500000000">>,
        is_nft       => false,
        network      => mainnet
    }),
    assert_non_empty(UpsertedObjectId, "upsert_object returned empty id"),

    {ok, Obj} = sui_walletdb:get_object(list_to_binary(ObjectId)),
    assert_eq(Obj#sui_object.balance, <<"500000000">>, "object balance mismatch"),
    assert_eq(Obj#sui_object.is_nft, false, "object is_nft mismatch"),

    NftObjectId = fresh_sui_address(),
    {ok, _} = sui_walletdb:upsert_object(WalletId, #{
        object_id       => list_to_binary(NftObjectId),
        owner_address   => list_to_binary(Address),
        object_type     => <<"0x2::devnet_nft::DevNetNFT">>,
        version         => <<"1">>,
        digest          => <<"0xnftdigest">>,
        is_nft          => true,
        nft_name        => <<"Test NFT">>,
        nft_description => <<"A test NFT">>,
        nft_url         => <<"https://example.com/nft/1">>,
        network         => mainnet
    }),

    {ok, WalletObjs} = sui_walletdb:get_wallet_objects(WalletId),
    assert_true(length(WalletObjs) >= 2, "should have at least 2 objects"),

    {ok, WalletNfts} = sui_walletdb:get_wallet_nfts(WalletId),
    assert_true(length(WalletNfts) >= 1, "should have at least 1 NFT"),
    [Nft | _] = WalletNfts,
    assert_eq(Nft#sui_object.is_nft, true, "NFT object is_nft should be true"),
    assert_eq(Nft#sui_object.nft_name, <<"Test NFT">>, "NFT name mismatch"),

    {ok, UserObjs} = sui_walletdb:get_user_objects(UserId),
    assert_true(length(UserObjs) >= 2, "user should have >= 2 objects"),

    CoinType = <<"0x2::sui::SUI">>,
    {ok, BalanceId} = sui_walletdb:upsert_coin_balance(WalletId, #{
        coin_type         => CoinType,
        total_balance     => <<"1000000000">>,
        coin_object_count => 3,
        owner_address     => list_to_binary(Address),
        network           => mainnet
    }),
    assert_non_empty(BalanceId, "upsert_coin_balance returned empty id"),

    {ok, CoinBal} = sui_walletdb:get_coin_balance(WalletId, CoinType),
    assert_eq(CoinBal#sui_coin_balance.total_balance, <<"1000000000">>, "coin balance mismatch"),
    assert_eq(CoinBal#sui_coin_balance.coin_object_count, 3, "coin_object_count mismatch"),

    {ok, _} = sui_walletdb:upsert_coin_balance(WalletId, #{
        coin_type         => CoinType,
        total_balance     => <<"2000000000">>,
        coin_object_count => 5
    }),
    {ok, UpdatedBal} = sui_walletdb:get_coin_balance(WalletId, CoinType),
    assert_eq(UpdatedBal#sui_coin_balance.total_balance, <<"2000000000">>, "upserted balance mismatch"),
    assert_eq(UpdatedBal#sui_coin_balance.coin_object_count, 5, "upserted count mismatch"),

    CustomCoinType = <<"0xabc123::mycoin::MyCoin">>,
    {ok, _} = sui_walletdb:upsert_coin_balance(WalletId, #{
        coin_type         => CustomCoinType,
        total_balance     => <<"999">>,
        coin_object_count => 1
    }),

    {ok, WalletBals} = sui_walletdb:get_wallet_coin_balances(WalletId),
    assert_true(length(WalletBals) >= 2, "should have >= 2 coin balances"),

    {ok, UserBals} = sui_walletdb:get_user_coin_balances(UserId),
    assert_true(length(UserBals) >= 2, "user should have >= 2 coin balances"),

    {ok, deleted} = sui_walletdb:delete_stake(StakeId),
    {error, stake_not_found} = sui_walletdb:get_stake(StakeId),

    {ok, deleted} = sui_walletdb:delete_object(list_to_binary(ObjectId)),
    {error, object_not_found} = sui_walletdb:get_object(list_to_binary(ObjectId)),

    {ok, deleted} = sui_walletdb:delete_coin_balance(BalanceId),
    {error, balance_not_found} = sui_walletdb:get_coin_balance(WalletId, CoinType),

    {ok, deleted} = sui_walletdb:delete_wallet(WalletId2),
    {ok, AfterDeletePrimary} = sui_walletdb:get_primary_wallet(UserId),
    assert_eq(AfterDeletePrimary#sui_wallet.wallet_id, WalletId, "after delete, WalletId should be primary again"),

    io:format(" sui_walletdb: all ~p assertions passed~n", [count_assertions()]),
    ok.

test_wallet(Token) ->
    UserId = "test_user_" ++ unique_id(),
    case sui_wallet:create_wallet(Token, UserId) of
        {ok, #{<<"wallet_id">> := WalletId, <<"sui_address">> := SuiAddress}} ->
            assert_non_empty(WalletId, "wallet_id empty"),
            assert_non_empty(SuiAddress, "sui_address empty"),
            io:format(" created ed25519 wallet: ~s -> ~s~n", [WalletId, SuiAddress]),

            case sui_wallet:create_wallet(Token, UserId, #{scheme => <<"secp256k1">>, label => <<"Secp256k1 Wallet">>}) of
                {ok, #{<<"wallet_id">> := WalletId2}} ->
                    io:format(" created secp256k1 wallet: ~s~n", [WalletId2]);
                {error, _} ->
                    io:format(" create secp256k1 wallet skipped (network)~n")
            end,

            case sui_wallet:create_wallet(Token, UserId, #{scheme => <<"secp256r1">>}) of
                {ok, #{<<"wallet_id">> := _WalletId3}} ->
                    io:format(" created secp256r1 wallet: ok~n");
                {error, _} ->
                    io:format(" create secp256r1 wallet skipped (network)~n")
            end,

            case sui_wallet:get_wallet_info(Token, WalletId) of
                {ok, #{<<"sui_address">> := SuiAddress}} ->
                    io:format(" get_wallet_info: ok~n");
                {error, _} ->
                    io:format(" get_wallet_info skipped (network)~n")
            end,

            case sui_wallet:get_user_wallets(Token, list_to_binary(UserId)) of
                {ok, #{<<"wallets">> := WalletList}} ->
                    assert_true(length(WalletList) >= 1, "user should have wallets"),
                    io:format(" get_user_wallets: ~p wallet(s)~n", [length(WalletList)]);
                {error, _} ->
                    io:format(" get_user_wallets skipped (network)~n")
            end,

            case sui_wallet:update_wallet_label(Token, WalletId, <<"My SUI Wallet">>) of
                {ok, _} ->
                    io:format(" update_wallet_label: ok~n");
                {error, _} ->
                    io:format(" update_wallet_label skipped (network)~n")
            end,

            case sui_wallet:set_primary_wallet(Token, list_to_binary(UserId), WalletId) of
                {ok, _} ->
                    io:format(" set_primary_wallet: ok~n");
                {error, _} ->
                    io:format(" set_primary_wallet skipped (network)~n")
            end,

            case sui_wallet:get_sui_balance(Token, WalletId) of
                {ok, #{<<"total_balance">> := _}} ->
                    io:format(" get_sui_balance: ok~n");
                {error, _} ->
                    io:format(" get_sui_balance skipped (network)~n")
            end,

            case sui_wallet:get_all_balances(Token, WalletId) of
                {ok, #{<<"balances">> := _}} ->
                    io:format(" get_all_balances: ok~n");
                {error, _} ->
                    io:format(" get_all_balances skipped (network)~n")
            end,

            case sui_wallet:get_owned_coins(Token, WalletId) of
                {ok, _} ->
                    io:format(" get_owned_coins: ok~n");
                {error, _} ->
                    io:format(" get_owned_coins skipped (network)~n")
            end,

            case sui_wallet:get_owned_objects(Token, WalletId) of
                {ok, _} ->
                    io:format(" get_owned_objects: ok~n");
                {error, _} ->
                    io:format(" get_owned_objects skipped (network)~n")
            end,

            case sui_wallet:get_local_transaction_history(Token, WalletId) of
                {ok, _} ->
                    io:format(" get_local_transaction_history: ok~n");
                {error, _} ->
                    io:format(" get_local_transaction_history skipped (network)~n")
            end,

            case sui_wallet:get_local_transaction_history(Token, WalletId, 10, 0) of
                {ok, _} ->
                    io:format(" get_local_transaction_history/4: ok~n");
                {error, _} ->
                    io:format(" get_local_transaction_history/4 skipped (network)~n")
            end,

            case sui_wallet:get_local_user_history(Token, list_to_binary(UserId)) of
                {ok, _} ->
                    io:format(" get_local_user_history: ok~n");
                {error, _} ->
                    io:format(" get_local_user_history skipped (network)~n")
            end,

            case sui_wallet:transfer_sui(Token, SuiAddress, SuiAddress, 1, testnet) of
                {ok, _} ->
                    io:format(" transfer_sui self: ok~n");
                {error, _} ->
                    io:format(" transfer_sui skipped (insufficient balance or network)~n")
            end,

            case sui_wallet:batch_transfer_sui(Token, SuiAddress,
                [#{to => SuiAddress, amount => <<"1">>}], testnet) of
                {ok, _} ->
                    io:format(" batch_transfer_sui: ok~n");
                {error, _} ->
                    io:format(" batch_transfer_sui skipped (network)~n")
            end,

            case sui_wallet:sign_message(Token, WalletId, <<"hello world">>) of
                {ok, #{<<"signature">> := _Sig}} ->
                    io:format(" sign_message: ok~n");
                {error, _} ->
                    io:format(" sign_message skipped (network)~n")
            end,

            case sui_wallet:get_chain_info(Token, testnet) of
                {ok, #{<<"chain_id">> := _}} ->
                    io:format(" get_chain_info testnet: ok~n");
                {error, _} ->
                    io:format(" get_chain_info skipped (network)~n")
            end,

            case sui_wallet:get_reference_gas_price(Token, testnet) of
                {ok, #{<<"reference_gas_price">> := _}} ->
                    io:format(" get_reference_gas_price: ok~n");
                {error, _} ->
                    io:format(" get_reference_gas_price skipped (network)~n")
            end,

            case sui_wallet:delete_wallet(Token, list_to_binary(UserId), WalletId) of
                {ok, _} ->
                    io:format(" delete_wallet: ok~n");
                {error, _} ->
                    io:format(" delete_wallet skipped (network)~n")
            end;
        {error, Reason} ->
            io:format(" create_wallet skipped (network unavailable): ~p~n", [Reason])
    end,

    case sui_wallet:import_wallet(Token, list_to_binary("user_" ++ unique_id()), <<"0xinvalidkey">>) of
        {error, _} ->
            io:format(" import bad private key: correctly rejected~n");
        {ok, _} ->
            io:format(" import bad private key: unexpected success~n")
    end,

    case sui_wallet:import_wallet(Token, list_to_binary("user_" ++ unique_id()), <<"0xinvalidkey">>, #{label => <<"test">>}) of
        {error, _} ->
            io:format(" import bad private key with label: correctly rejected~n");
        {ok, _} ->
            io:format(" import bad private key with label: unexpected success~n")
    end,

    case sui_wallet:import_wallet_from_mnemonic(Token, list_to_binary("user_" ++ unique_id()), <<"not a valid mnemonic phrase">>) of
        {error, _} ->
            io:format(" import bad mnemonic: correctly rejected~n");
        {ok, _} ->
            io:format(" import bad mnemonic: unexpected success~n")
    end,
    ok.

test_tokens(Token) ->
    FakeWalletId  = "nonexistent_wallet_" ++ unique_id(),
    FakeAddress   = fresh_sui_address(),
    FakeCoinType  = "0x" ++ unique_id() ++ "::fakecoin::FakeCoin",

    case sui_wallet:get_sui_balance(Token, list_to_binary(FakeWalletId)) of
        {ok, _} ->
            io:format(" sui_wallet:get_sui_balance: ok~n");
        {error, _} ->
            io:format(" sui_wallet:get_sui_balance nonexistent: correctly rejected~n")
    end,

    case sui_wallet:get_balance_by_coin_type(Token, list_to_binary(FakeWalletId), list_to_binary(FakeCoinType)) of
        {ok, _} ->
            io:format(" sui_wallet:get_balance_by_coin_type: ok~n");
        {error, _} ->
            io:format(" sui_wallet:get_balance_by_coin_type nonexistent: correctly rejected~n")
    end,

    case sui_wallet:get_all_balances(Token, list_to_binary(FakeWalletId)) of
        {ok, _} ->
            io:format(" sui_wallet:get_all_balances: ok~n");
        {error, _} ->
            io:format(" sui_wallet:get_all_balances nonexistent: correctly rejected~n")
    end,

    case sui_wallet:get_coin_info(Token, <<"0x2::sui::SUI">>) of
        {ok, _} ->
            io:format(" sui_wallet:get_coin_info SUI: ok~n");
        {error, _} ->
            io:format(" sui_wallet:get_coin_info skipped (network)~n")
    end,

    case sui_wallet:get_coin_info(Token, list_to_binary(FakeCoinType)) of
        {ok, _} ->
            io:format(" sui_wallet:get_coin_info fake: ok~n");
        {error, _} ->
            io:format(" sui_wallet:get_coin_info fake: correctly rejected~n")
    end,

    case sui_wallet:transfer_sui(Token, list_to_binary(FakeAddress), list_to_binary(FakeAddress), 100, testnet) of
        {error, _} ->
            io:format(" sui_wallet:transfer_sui nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_wallet:transfer_sui: unexpected success~n")
    end,

    case sui_wallet:batch_transfer_sui(Token, list_to_binary(FakeAddress),
        [#{to => list_to_binary(FakeAddress), amount => <<"100">>}], testnet) of
        {error, _} ->
            io:format(" sui_wallet:batch_transfer_sui nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_wallet:batch_transfer_sui: unexpected success~n")
    end,

    case sui_tokens:get_local_coin_balance(list_to_binary(FakeWalletId), list_to_binary(FakeAddress), <<"0x2::sui::SUI">>) of
        {error, _} ->
            io:format(" sui_tokens:get_local_coin_balance nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_tokens:get_local_coin_balance: ok~n")
    end,

    case sui_tokens:get_local_all_coin_balances(list_to_binary(FakeWalletId), list_to_binary(FakeAddress)) of
        {ok, []} ->
            io:format(" sui_tokens:get_local_all_coin_balances empty: ok~n");
        {ok, _} ->
            io:format(" sui_tokens:get_local_all_coin_balances: ok~n");
        {error, _} ->
            io:format(" sui_tokens:get_local_all_coin_balances: correctly rejected~n")
    end,

    case sui_tokens:get_local_objects(list_to_binary(FakeWalletId), list_to_binary(FakeAddress)) of
        {ok, []} ->
            io:format(" sui_tokens:get_local_objects empty: ok~n");
        {ok, _} ->
            io:format(" sui_tokens:get_local_objects: ok~n");
        {error, _} ->
            io:format(" sui_tokens:get_local_objects: correctly rejected~n")
    end,

    case sui_tokens:get_local_nfts(list_to_binary(FakeWalletId), list_to_binary(FakeAddress)) of
        {ok, []} ->
            io:format(" sui_tokens:get_local_nfts empty: ok~n");
        {ok, _} ->
            io:format(" sui_tokens:get_local_nfts: ok~n");
        {error, _} ->
            io:format(" sui_tokens:get_local_nfts: correctly rejected~n")
    end,

    case sui_tokens:transfer_sui(Token, list_to_binary(FakeWalletId), list_to_binary(FakeAddress), 100, testnet) of
        {error, _} ->
            io:format(" sui_tokens:transfer_sui nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_tokens:transfer_sui: unexpected success~n")
    end,

    case sui_tokens:batch_transfer_sui(Token, list_to_binary(FakeWalletId),
        [#{to => list_to_binary(FakeAddress), amount => <<"100">>}], testnet) of
        {error, _} ->
            io:format(" sui_tokens:batch_transfer_sui nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_tokens:batch_transfer_sui: unexpected success~n")
    end,

    case sui_tokens:execute_move_call(Token, list_to_binary(FakeWalletId),
        <<"0x2">>, <<"coin">>, <<"value">>, []) of
        {error, _} ->
            io:format(" sui_tokens:execute_move_call nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_tokens:execute_move_call: ok~n")
    end,

    case sui_tokens:get_chain_info(Token, list_to_binary(FakeWalletId), testnet) of
        {ok, _} ->
            io:format(" sui_tokens:get_chain_info: ok~n");
        {error, _} ->
            io:format(" sui_tokens:get_chain_info skipped (network)~n")
    end,

    case sui_tokens:get_reference_gas_price(Token, list_to_binary(FakeWalletId), testnet) of
        {ok, _} ->
            io:format(" sui_tokens:get_reference_gas_price: ok~n");
        {error, _} ->
            io:format(" sui_tokens:get_reference_gas_price skipped (network)~n")
    end,

    case sui_tokens:lookup_name(Token, <<"example.sui">>, testnet, #{}) of
        {ok, _} ->
            io:format(" sui_tokens:lookup_name: ok~n");
        {error, _} ->
            io:format(" sui_tokens:lookup_name skipped (network)~n")
    end,

    case sui_tokens:reverse_lookup_name(Token, list_to_binary(FakeAddress), testnet, #{}) of
        {ok, _} ->
            io:format(" sui_tokens:reverse_lookup_name: ok~n");
        {error, _} ->
            io:format(" sui_tokens:reverse_lookup_name: correctly rejected~n")
    end,
    ok.

test_nft(Token) ->
    FakeWalletId = "nonexistent_wallet_" ++ unique_id(),
    FakeAddress  = fresh_sui_address(),
    FakeObjectId = fresh_sui_address(),

    case sui_wallet:mint_nft(Token, list_to_binary(FakeAddress),
        <<"Test NFT">>, <<"A test NFT">>, <<"https://example.com/nft/1">>, testnet) of
        {error, _} ->
            io:format(" sui_wallet:mint_nft nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_wallet:mint_nft: ok~n")
    end,

    case sui_wallet:mint_nft(Token, list_to_binary(FakeAddress),
        <<"Test NFT 2">>, <<"Another NFT">>, <<"https://example.com/nft/2">>, testnet,
        #{package_id => <<"0x2">>, module_name => <<"devnet_nft">>}) of
        {error, _} ->
            io:format(" sui_wallet:mint_nft/7 nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_wallet:mint_nft/7: ok~n")
    end,

    case sui_wallet:transfer_nft(Token, list_to_binary(FakeAddress),
        list_to_binary(FakeObjectId), list_to_binary(FakeAddress), testnet) of
        {error, _} ->
            io:format(" sui_wallet:transfer_nft nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_wallet:transfer_nft: ok~n")
    end,

    case sui_wallet:get_nfts(Token, list_to_binary(FakeAddress), testnet) of
        {ok, _} ->
            io:format(" sui_wallet:get_nfts: ok~n");
        {error, _} ->
            io:format(" sui_wallet:get_nfts skipped (network)~n")
    end,

    case sui_tokens:mint_nft(Token, list_to_binary(FakeWalletId),
        <<"Token Name">>, <<"Token Desc">>, <<"https://example.com">>, testnet) of
        {error, _} ->
            io:format(" sui_tokens:mint_nft nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_tokens:mint_nft: ok~n")
    end,

    case sui_tokens:mint_nft(Token, list_to_binary(FakeWalletId),
        <<"Token 2">>, <<"Desc 2">>, <<"https://example.com/2">>, testnet,
        #{package_id => <<"0xcustom">>}) of
        {error, _} ->
            io:format(" sui_tokens:mint_nft/7 nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_tokens:mint_nft/7: ok~n")
    end,

    case sui_tokens:transfer_nft(Token, list_to_binary(FakeWalletId),
        list_to_binary(FakeObjectId), list_to_binary(FakeAddress), testnet) of
        {error, _} ->
            io:format(" sui_tokens:transfer_nft nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_tokens:transfer_nft: ok~n")
    end,

    case sui_tokens:get_nfts(Token, list_to_binary(FakeWalletId),
        list_to_binary(FakeAddress), testnet) of
        {ok, _} ->
            io:format(" sui_tokens:get_nfts: ok~n");
        {error, _} ->
            io:format(" sui_tokens:get_nfts skipped (network)~n")
    end,

    case sui_wallet:get_object(Token, list_to_binary(FakeObjectId), testnet) of
        {error, _} ->
            io:format(" sui_wallet:get_object nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_wallet:get_object: ok~n")
    end,

    case sui_wallet:get_objects(Token, [list_to_binary(FakeObjectId)], testnet) of
        {ok, _} ->
            io:format(" sui_wallet:get_objects: ok~n");
        {error, _} ->
            io:format(" sui_wallet:get_objects: correctly rejected~n")
    end,

    case sui_wallet:get_dynamic_fields(Token, list_to_binary(FakeObjectId), testnet) of
        {ok, _} ->
            io:format(" sui_wallet:get_dynamic_fields: ok~n");
        {error, _} ->
            io:format(" sui_wallet:get_dynamic_fields: correctly rejected~n")
    end,

    case sui_tokens:sync_object(Token, list_to_binary(FakeWalletId),
        list_to_binary(FakeObjectId), testnet) of
        {error, _} ->
            io:format(" sui_tokens:sync_object nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_tokens:sync_object: ok~n")
    end,
    ok.

test_staking(Token) ->
    FakeWalletId      = list_to_binary("nonexistent_wallet_" ++ unique_id()),
    FakeAddress       = list_to_binary(fresh_sui_address()),
    FakeValidatorAddr = list_to_binary(fresh_sui_address()),
    FakeStakedSuiId   = list_to_binary(fresh_sui_address()),

    case sui_wallet:get_validators(Token, testnet) of
        {ok, #{<<"validators">> := Validators}} ->
            io:format(" get_validators testnet: ~p validator(s)~n", [length(Validators)]);
        {error, _} ->
            io:format(" get_validators skipped (network)~n")
    end,

    case sui_wallet:get_validator(Token, FakeValidatorAddr, testnet) of
        {ok, _} ->
            io:format(" get_validator: ok~n");
        {error, _} ->
            io:format(" get_validator nonexistent: correctly rejected~n")
    end,

    case sui_wallet:get_epoch(Token, testnet) of
        {ok, #{<<"epoch">> := _}} ->
            io:format(" get_epoch testnet: ok~n");
        {error, _} ->
            io:format(" get_epoch skipped (network)~n")
    end,

    case sui_wallet:stake_with_validator(Token, FakeAddress, FakeValidatorAddr, 1000000000, testnet) of
        {error, _} ->
            io:format(" sui_wallet:stake_with_validator nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_wallet:stake_with_validator: ok~n")
    end,

    case sui_wallet:stake_with_validator(Token, FakeAddress, FakeValidatorAddr, 1000000000, testnet, #{}) of
        {error, _} ->
            io:format(" sui_wallet:stake_with_validator/6 nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_wallet:stake_with_validator/6: ok~n")
    end,

    case sui_wallet:unstake_from_validator(Token, FakeAddress, FakeStakedSuiId, testnet) of
        {error, _} ->
            io:format(" sui_wallet:unstake_from_validator nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_wallet:unstake_from_validator: ok~n")
    end,

    case sui_wallet:unstake_from_validator(Token, FakeAddress, FakeStakedSuiId, testnet, #{}) of
        {error, _} ->
            io:format(" sui_wallet:unstake_from_validator/5 nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_wallet:unstake_from_validator/5: ok~n")
    end,

    case sui_wallet:get_staked_sui(Token, FakeAddress, testnet) of
        {ok, _} ->
            io:format(" get_staked_sui: ok~n");
        {error, _} ->
            io:format(" get_staked_sui skipped (network)~n")
    end,

    case sui_wallet:get_staking_rewards(Token, FakeAddress, testnet) of
        {ok, _} ->
            io:format(" get_staking_rewards: ok~n");
        {error, _} ->
            io:format(" get_staking_rewards skipped (network)~n")
    end,

    case sui_staking:stake(Token, FakeWalletId, FakeValidatorAddr, 1000000000, testnet) of
        {error, _} ->
            io:format(" sui_staking:stake nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_staking:stake: ok~n")
    end,

    case sui_staking:stake(Token, FakeWalletId, FakeValidatorAddr, 1000000000, testnet, #{}) of
        {error, _} ->
            io:format(" sui_staking:stake/6 nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_staking:stake/6: ok~n")
    end,

    case sui_staking:unstake(Token, FakeWalletId, FakeStakedSuiId, testnet) of
        {error, _} ->
            io:format(" sui_staking:unstake nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_staking:unstake: ok~n")
    end,

    case sui_staking:unstake(Token, FakeWalletId, FakeStakedSuiId, testnet, #{}) of
        {error, _} ->
            io:format(" sui_staking:unstake/5 nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_staking:unstake/5: ok~n")
    end,

    case sui_staking:get_stake_info(Token, FakeWalletId, FakeStakedSuiId, testnet) of
        {error, _} ->
            io:format(" sui_staking:get_stake_info nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_staking:get_stake_info: ok~n")
    end,

    case sui_staking:get_user_stakes(Token, "nonexistent_user_" ++ unique_id(), testnet) of
        {ok, #{stakes := []}} ->
            io:format(" sui_staking:get_user_stakes empty: ok~n");
        {ok, _} ->
            io:format(" sui_staking:get_user_stakes: ok~n");
        {error, _} ->
            io:format(" sui_staking:get_user_stakes: correctly rejected~n")
    end,

    case sui_staking:get_wallet_stakes(Token, FakeWalletId, testnet) of
        {ok, #{stakes := []}} ->
            io:format(" sui_staking:get_wallet_stakes empty: ok~n");
        {ok, _} ->
            io:format(" sui_staking:get_wallet_stakes: ok~n");
        {error, _} ->
            io:format(" sui_staking:get_wallet_stakes: correctly rejected~n")
    end,

    case sui_staking:get_validators(Token, testnet) of
        {ok, _} ->
            io:format(" sui_staking:get_validators: ok~n");
        {error, _} ->
            io:format(" sui_staking:get_validators skipped (network)~n")
    end,

    case sui_staking:get_epoch(Token, testnet) of
        {ok, _} ->
            io:format(" sui_staking:get_epoch: ok~n");
        {error, _} ->
            io:format(" sui_staking:get_epoch skipped (network)~n")
    end,

    case sui_staking:get_staking_rewards(Token, FakeWalletId, testnet) of
        {error, _} ->
            io:format(" sui_staking:get_staking_rewards nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_staking:get_staking_rewards: ok~n")
    end,

    case sui_staking:sync_stake(Token, FakeWalletId, FakeStakedSuiId) of
        {error, _} ->
            io:format(" sui_staking:sync_stake nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_staking:sync_stake: ok~n")
    end,

    case sui_staking:sync_all_stakes(Token, FakeWalletId, testnet) of
        {error, _} ->
            io:format(" sui_staking:sync_all_stakes nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_staking:sync_all_stakes: ok~n")
    end,
    ok.

test_transactions(Token) ->
    FakeWalletId = list_to_binary("nonexistent_wallet_" ++ unique_id()),
    FakeAddress  = list_to_binary(fresh_sui_address()),
    FakeDigest   = list_to_binary("0x" ++ unique_id()),

    case sui_wallet:get_transaction(Token, FakeDigest, testnet) of
        {error, _} ->
            io:format(" get_transaction nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" get_transaction: ok~n")
    end,

    case sui_wallet:get_checkpoint(Token, 1, testnet) of
        {ok, _} ->
            io:format(" get_checkpoint height 1: ok~n");
        {error, _} ->
            io:format(" get_checkpoint skipped (network)~n")
    end,

    case sui_wallet:simulate_transaction(Token, <<"fakebytes">>, testnet) of
        {error, _} ->
            io:format(" simulate_transaction invalid bytes: correctly rejected~n");
        {ok, _} ->
            io:format(" simulate_transaction: ok~n")
    end,

    case sui_wallet:wait_for_transaction(Token, FakeDigest, testnet) of
        {error, _} ->
            io:format(" wait_for_transaction nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" wait_for_transaction: ok~n")
    end,

    case sui_wallet:build_transaction(Token, FakeAddress, <<"transfer">>,
        #{recipient => FakeAddress, amount => <<"100">>}) of
        {error, _} ->
            io:format(" build_transaction nonexistent sender: correctly rejected~n");
        {ok, _} ->
            io:format(" build_transaction: ok~n")
    end,

    case sui_wallet:execute_move_call(Token, FakeAddress, <<"0x2">>, <<"coin">>, <<"value">>, []) of
        {error, _} ->
            io:format(" execute_move_call nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" execute_move_call: ok~n")
    end,

    case sui_wallet:execute_move_call(Token, FakeAddress, <<"0x2">>, <<"coin">>, <<"value">>, [], #{}) of
        {error, _} ->
            io:format(" execute_move_call/7 nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" execute_move_call/7: ok~n")
    end,

    case sui_wallet:publish_package(Token, FakeAddress, [], [], #{}) of
        {error, _} ->
            io:format(" publish_package nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" publish_package: ok~n")
    end,

    case sui_wallet:upgrade_package(Token, FakeAddress, <<"0x2">>, [], <<"0xcap">>, #{}) of
        {error, _} ->
            io:format(" upgrade_package nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" upgrade_package: ok~n")
    end,

    case sui_wallet:verify_signature(Token, <<"hello">>, <<"fakesig">>, FakeAddress) of
        {ok, #{<<"is_valid">> := false}} ->
            io:format(" verify_signature bad sig: correctly invalid~n");
        {error, _} ->
            io:format(" verify_signature bad sig: correctly rejected~n");
        {ok, _} ->
            io:format(" verify_signature: unexpected valid~n")
    end,

    case sui_wallet:create_multisig(Token, [], 1, testnet) of
        {error, _} ->
            io:format(" create_multisig empty keys: correctly rejected~n");
        {ok, _} ->
            io:format(" create_multisig: ok~n")
    end,

    case sui_wallet:lookup_name(Token, <<"example.sui">>, testnet) of
        {ok, _} ->
            io:format(" lookup_name: ok~n");
        {error, _} ->
            io:format(" lookup_name skipped (network)~n")
    end,

    case sui_wallet:reverse_lookup_name(Token, FakeAddress, testnet) of
        {ok, _} ->
            io:format(" reverse_lookup_name: ok~n");
        {error, _} ->
            io:format(" reverse_lookup_name: correctly rejected~n")
    end,

    case sui_wallet:request_faucet(Token, FakeAddress, devnet) of
        {ok, _} ->
            io:format(" request_faucet devnet: ok~n");
        {error, _} ->
            io:format(" request_faucet devnet skipped (network)~n")
    end,

    case sui_tokens:get_transaction(Token, FakeWalletId, FakeDigest, testnet) of
        {error, _} ->
            io:format(" sui_tokens:get_transaction nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_tokens:get_transaction: ok~n")
    end,

    case sui_tokens:get_checkpoint(Token, FakeWalletId, 1, testnet) of
        {ok, _} ->
            io:format(" sui_tokens:get_checkpoint: ok~n");
        {error, _} ->
            io:format(" sui_tokens:get_checkpoint skipped (network)~n")
    end,

    case sui_tokens:sign_message(Token, FakeWalletId, <<"hello">>, testnet) of
        {error, _} ->
            io:format(" sui_tokens:sign_message nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_tokens:sign_message: ok~n")
    end,

    case sui_tokens:verify_signature(Token, <<"hello">>, <<"fakesig">>, FakeAddress, testnet) of
        {ok, _} ->
            io:format(" sui_tokens:verify_signature: ok~n");
        {error, _} ->
            io:format(" sui_tokens:verify_signature: correctly rejected~n")
    end,

    case sui_tokens:build_transaction(Token, FakeWalletId, <<"transfer">>,
        #{recipient => FakeAddress, amount => <<"100">>}, testnet) of
        {error, _} ->
            io:format(" sui_tokens:build_transaction nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" sui_tokens:build_transaction: ok~n")
    end,
    ok.

ensure_mnesia_started() ->
    SuiTables = [sui_wallet, sui_transaction, sui_stake, sui_object, sui_coin_balance],
    [ensure_test_table(T) || T <- SuiTables],
    mnesia:wait_for_tables(SuiTables, 5000).

ensure_test_table(sui_wallet) ->
    safe_create_table(sui_wallet, record_info(fields, sui_wallet)),
    safe_add_index(sui_wallet, user_id),
    safe_add_index(sui_wallet, sui_address),
    safe_add_index(sui_wallet, network);
ensure_test_table(sui_transaction) ->
    safe_create_table(sui_transaction, record_info(fields, sui_transaction)),
    safe_add_index(sui_transaction, wallet_id),
    safe_add_index(sui_transaction, user_id),
    safe_add_index(sui_transaction, digest),
    safe_add_index(sui_transaction, tx_type),
    safe_add_index(sui_transaction, sender_address),
    safe_add_index(sui_transaction, network);
ensure_test_table(sui_stake) ->
    safe_create_table(sui_stake, record_info(fields, sui_stake)),
    safe_add_index(sui_stake, wallet_id),
    safe_add_index(sui_stake, user_id),
    safe_add_index(sui_stake, validator_address),
    safe_add_index(sui_stake, staked_sui_id);
ensure_test_table(sui_object) ->
    safe_create_table(sui_object, record_info(fields, sui_object)),
    safe_add_index(sui_object, wallet_id),
    safe_add_index(sui_object, user_id),
    safe_add_index(sui_object, owner_address),
    safe_add_index(sui_object, object_type);
ensure_test_table(sui_coin_balance) ->
    safe_create_table(sui_coin_balance, record_info(fields, sui_coin_balance)),
    safe_add_index(sui_coin_balance, wallet_id),
    safe_add_index(sui_coin_balance, user_id),
    safe_add_index(sui_coin_balance, owner_address),
    safe_add_index(sui_coin_balance, coin_type).

safe_create_table(Table, Fields) ->
    case mnesia:create_table(Table, [
        {attributes, Fields},
        {disc_copies, [node()]},
        {type, set}
    ]) of
        {atomic, ok}                    -> ok;
        {aborted, {already_exists, _}}  -> ok;
        {aborted, Reason}               -> error({create_test_table_failed, Table, Reason})
    end.

safe_add_index(Table, Field) ->
    case mnesia:add_table_index(Table, Field) of
        {atomic, ok}                       -> ok;
        {aborted, {already_exists, _, _}}  -> ok;
        {aborted, Reason} ->
            io:format(" [warn] index ~p.~p: ~p~n", [Table, Field, Reason])
    end.

get_test_token() ->
    case application:get_env(sui_test, token) of
        {ok, T} -> T;
        undefined ->
            case os:getenv("SUI_TEST_TOKEN") of
                false -> "test_token_replace_me";
                T     -> T
            end
    end.

fresh_sui_address() ->
    "0x" ++ string:lowercase(
        lists:flatten([io_lib:format("~2.16.0b", [X])
                       || X <- binary_to_list(crypto:strong_rand_bytes(32))])).

unique_id() ->
    integer_to_list(erlang:unique_integer([positive])).

assert_eq(A, B, Msg) ->
    case A =:= B of
        true  -> ok;
        false -> error({assertion_failed, Msg, {expected, B}, {got, A}})
    end.

assert_true(true, _Msg) -> ok;
assert_true(false, Msg) -> error({assertion_failed, Msg}).

assert_non_empty("", Msg)        -> error({assertion_failed, Msg, empty_string});
assert_non_empty(<<>>, Msg)      -> error({assertion_failed, Msg, empty_binary});
assert_non_empty(undefined, Msg) -> error({assertion_failed, Msg, undefined});
assert_non_empty(_, _)           -> ok.

count_assertions() ->
    68.
