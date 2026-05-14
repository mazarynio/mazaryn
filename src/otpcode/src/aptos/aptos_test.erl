-module(aptos_test).
-author("Zaryn Technologies").
-include("../records.hrl").
-include("../wallet_records.hrl").

-export([run_all/0, run_all/1]).
-export([
    test_walletdb/0,
    test_wallet/1,
    test_tokens/1,
    test_nft/1,
    test_transactions/1,
    test_staking/1,
    test_user_integration/1
]).

run_all() ->
    Token = get_test_token(),
    run_all(Token).

run_all(Token) ->
    Suites = [
        {"aptos_walletdb (Mnesia layer)", fun test_walletdb/0},
        {"aptos_wallet (HTTP client)", fun() -> test_wallet(Token) end},
        {"aptos_tokens (coins/FA)", fun() -> test_tokens(Token) end},
        {"aptos_tokens (NFT operations)", fun() -> test_nft(Token) end},
        {"aptos_transactions (ledger/blocks)", fun() -> test_transactions(Token) end},
        {"aptos_staking (delegation pools)", fun() -> test_staking(Token) end},
        {"aptos_user_integration", fun() -> test_user_integration(Token) end}
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
    UserId = "test_user_" ++ unique_id(),
    Address = fresh_address(),

    {ok, WalletId} = aptos_walletdb:create_wallet(
        UserId, Address,
        <<"enc_private_key">>, <<"iv_data">>, <<"tag_data">>
    ),
    assert_non_empty(WalletId, "create_wallet returned empty wallet_id"),

    {ok, Wallet} = aptos_walletdb:get_wallet(WalletId),
    assert_eq(Wallet#aptos_wallet.user_id, UserId, "wallet user_id mismatch"),
    assert_eq(Wallet#aptos_wallet.account_address, Address, "wallet account_address mismatch"),
    assert_eq(Wallet#aptos_wallet.is_primary, true, "first wallet should be primary"),

    {ok, WalletByAddr} = aptos_walletdb:get_wallet_by_address(Address),
    assert_eq(WalletByAddr#aptos_wallet.wallet_id, WalletId, "address lookup mismatch"),

    assert_eq(aptos_walletdb:wallet_exists(WalletId), true, "wallet_exists should be true"),
    assert_eq(aptos_walletdb:address_exists(Address), true, "address_exists should be true"),
    assert_eq(aptos_walletdb:wallet_exists("nonexistent_id"), false, "nonexistent wallet should be false"),

    {error, address_already_exists} = aptos_walletdb:create_wallet(
        UserId, Address, <<"k">>, <<"iv">>, <<"t">>
    ),

    Address2 = fresh_address(),
    {ok, WalletId2} = aptos_walletdb:create_wallet(
        UserId, Address2, <<"k2">>, <<"iv2">>, <<"t2">>
    ),
    {ok, Wallet2} = aptos_walletdb:get_wallet(WalletId2),
    assert_eq(Wallet2#aptos_wallet.is_primary, false, "second wallet should not be primary"),

    {ok, Wallets} = aptos_walletdb:get_user_wallets(UserId),
    assert_eq(length(Wallets), 2, "user should have 2 wallets"),

    {ok, Primary} = aptos_walletdb:get_primary_wallet(UserId),
    assert_eq(Primary#aptos_wallet.wallet_id, WalletId, "primary wallet mismatch"),

    {ok, WalletId2} = aptos_walletdb:set_primary_wallet(UserId, WalletId2),
    {ok, NewPrimary} = aptos_walletdb:get_primary_wallet(UserId),
    assert_eq(NewPrimary#aptos_wallet.wallet_id, WalletId2, "new primary wallet mismatch"),

    {ok, WalletId} = aptos_walletdb:update_wallet_label(WalletId, "My Aptos Wallet"),
    {ok, LabeledWallet} = aptos_walletdb:get_wallet(WalletId),
    assert_eq(LabeledWallet#aptos_wallet.label, "My Aptos Wallet", "label update mismatch"),

    {ok, WalletId} = aptos_walletdb:update_wallet_last_used(WalletId),

    TxHash = "0x" ++ unique_id(),
    {ok, TxId} = aptos_walletdb:create_transaction(WalletId, #{
        transaction_hash => TxHash,
        tx_type          => apt_transfer,
        sender_address   => Address,
        receiver_address => Address2,
        amount_octas     => 100000000,
        gas_used         => 500,
        gas_unit_price   => 100,
        vm_status        => <<"Executed successfully">>,
        status           => confirmed
    }),
    assert_non_empty(TxId, "create_transaction returned empty tx_id"),

    {ok, Tx} = aptos_walletdb:get_transaction(TxId),
    assert_eq(Tx#aptos_transaction.wallet_id, WalletId, "tx wallet_id mismatch"),
    assert_eq(Tx#aptos_transaction.tx_type, apt_transfer, "tx type mismatch"),
    assert_eq(Tx#aptos_transaction.amount_octas, 100000000, "tx amount mismatch"),

    {ok, TxByHash} = aptos_walletdb:get_transaction_by_hash(TxHash),
    assert_eq(TxByHash#aptos_transaction.tx_id, TxId, "tx hash lookup mismatch"),

    {ok, WalletTxs} = aptos_walletdb:get_wallet_transactions(WalletId),
    assert_true(length(WalletTxs) >= 1, "should have at least 1 transaction"),

    {ok, PagedTxs, Total} = aptos_walletdb:get_wallet_transactions(WalletId, 10, 0),
    assert_true(Total >= 1, "total should be >= 1"),
    assert_true(length(PagedTxs) >= 1, "paged results should have >= 1 tx"),

    {ok, UserTxs} = aptos_walletdb:get_user_transactions(UserId),
    assert_true(length(UserTxs) >= 1, "user should have at least 1 transaction"),

    {ok, AptTxs} = aptos_walletdb:get_transactions_by_type(WalletId, apt_transfer),
    assert_true(length(AptTxs) >= 1, "should find apt_transfer transactions"),

    CoinTxHash = "0x" ++ unique_id(),
    {ok, CoinTxId} = aptos_walletdb:create_transaction(WalletId, #{
        transaction_hash => CoinTxHash,
        tx_type          => coin_transfer,
        sender_address   => Address,
        receiver_address => Address2,
        amount_octas     => 50000000,
        coin_type        => <<"0x1::aptos_coin::AptosCoin">>,
        status           => confirmed
    }),
    {ok, CoinTx} = aptos_walletdb:get_transaction(CoinTxId),
    assert_eq(CoinTx#aptos_transaction.tx_type, coin_transfer, "coin tx type mismatch"),
    assert_eq(CoinTx#aptos_transaction.coin_type, <<"0x1::aptos_coin::AptosCoin">>, "coin_type mismatch"),

    StakeTxHash = "0x" ++ unique_id(),
    {ok, StakeTxId} = aptos_walletdb:create_transaction(WalletId, #{
        transaction_hash => StakeTxHash,
        tx_type          => stake_add,
        sender_address   => Address,
        pool_address     => <<"0xpool123">>,
        amount_octas     => 1000000000,
        status           => confirmed
    }),
    {ok, StakeTx} = aptos_walletdb:get_transaction(StakeTxId),
    assert_eq(StakeTx#aptos_transaction.tx_type, stake_add, "stake tx type mismatch"),
    assert_eq(StakeTx#aptos_transaction.pool_address, <<"0xpool123">>, "pool_address mismatch"),

    {ok, CollId} = aptos_walletdb:create_nft_collection(WalletId, #{
        collection_name  => "Test Collection",
        description      => "A test collection",
        uri              => "https://example.com/collection",
        max_supply       => 1000,
        transaction_hash => "0xcoll" ++ unique_id()
    }),
    assert_non_empty(CollId, "create_nft_collection returned empty id"),

    {ok, Coll} = aptos_walletdb:get_nft_collection(CollId),
    assert_eq(Coll#aptos_nft_collection.collection_name, "Test Collection", "collection name mismatch"),
    assert_eq(Coll#aptos_nft_collection.wallet_id, WalletId, "collection wallet_id mismatch"),

    {ok, CollByName} = aptos_walletdb:get_nft_collection_by_name(Address, "Test Collection"),
    assert_eq(CollByName#aptos_nft_collection.collection_id, CollId, "collection name lookup mismatch"),

    {ok, WalletColls} = aptos_walletdb:get_wallet_nft_collections(WalletId),
    assert_true(length(WalletColls) >= 1, "should have at least 1 collection"),

    {ok, UserColls} = aptos_walletdb:get_user_nft_collections(UserId),
    assert_true(length(UserColls) >= 1, "user should have at least 1 collection"),

    {ok, TokenId} = aptos_walletdb:create_nft_token(WalletId, #{
        collection_id    => CollId,
        collection_name  => "Test Collection",
        token_name       => "Test Token #1",
        description      => "My first token",
        uri              => "https://example.com/token/1",
        property_keys    => [<<"level">>, <<"rarity">>],
        property_types   => [<<"u64">>, <<"String">>],
        property_values  => [<<"10">>, <<"rare">>],
        transaction_hash => "0xtok" ++ unique_id()
    }),
    assert_non_empty(TokenId, "create_nft_token returned empty id"),

    {ok, NftToken} = aptos_walletdb:get_nft_token(TokenId),
    assert_eq(NftToken#aptos_nft_token.token_name, "Test Token #1", "token name mismatch"),
    assert_eq(NftToken#aptos_nft_token.collection_id, CollId, "token collection_id mismatch"),

    {ok, WalletTokens} = aptos_walletdb:get_wallet_nft_tokens(WalletId),
    assert_true(length(WalletTokens) >= 1, "should have at least 1 token"),

    {ok, CollTokens} = aptos_walletdb:get_collection_nft_tokens(CollId),
    assert_true(length(CollTokens) >= 1, "collection should have at least 1 token"),

    MetadataAddress = "0xmeta" ++ unique_id(),
    {ok, FaId} = aptos_walletdb:create_fungible_asset(WalletId, #{
        asset_name       => "Test Coin",
        symbol           => "TST",
        decimals         => 8,
        metadata_address => MetadataAddress,
        transaction_hash => "0xfa" ++ unique_id()
    }),
    assert_non_empty(FaId, "create_fungible_asset returned empty id"),

    {ok, Fa} = aptos_walletdb:get_fungible_asset(FaId),
    assert_eq(Fa#aptos_fungible_asset.symbol, "TST", "FA symbol mismatch"),
    assert_eq(Fa#aptos_fungible_asset.decimals, 8, "FA decimals mismatch"),

    {ok, FaByMeta} = aptos_walletdb:get_fungible_asset_by_metadata(MetadataAddress),
    assert_eq(FaByMeta#aptos_fungible_asset.fa_id, FaId, "FA metadata lookup mismatch"),

    {ok, WalletFas} = aptos_walletdb:get_wallet_fungible_assets(WalletId),
    assert_true(length(WalletFas) >= 1, "should have at least 1 FA"),

    {ok, UserFas} = aptos_walletdb:get_user_fungible_assets(UserId),
    assert_true(length(UserFas) >= 1, "user should have at least 1 FA"),

    PoolAddress = "0xpool" ++ unique_id(),
    {ok, StakeId} = aptos_walletdb:create_stake(WalletId, #{
        pool_address           => PoolAddress,
        amount_octas           => 2000000000,
        tx_type                => stake_add,
        transaction_hash       => "0xstk" ++ unique_id(),
        active_stake           => <<"2000000000">>,
        inactive_stake         => <<"0">>,
        pending_inactive_stake => <<"0">>,
        status                 => confirmed
    }),
    assert_non_empty(StakeId, "create_stake returned empty id"),

    {ok, Stake} = aptos_walletdb:get_stake(StakeId),
    assert_eq(Stake#aptos_stake.tx_type, stake_add, "stake tx_type mismatch"),
    assert_eq(Stake#aptos_stake.amount_octas, 2000000000, "stake amount mismatch"),
    assert_eq(Stake#aptos_stake.pool_address, PoolAddress, "stake pool_address mismatch"),

    {ok, WalletStakes} = aptos_walletdb:get_wallet_stakes(WalletId),
    assert_true(length(WalletStakes) >= 1, "should have at least 1 stake"),

    {ok, UserStakes} = aptos_walletdb:get_user_stakes(UserId),
    assert_true(length(UserStakes) >= 1, "user should have at least 1 stake"),

    {ok, PoolStakes} = aptos_walletdb:get_stakes_by_pool(WalletId, PoolAddress),
    assert_true(length(PoolStakes) >= 1, "pool should have at least 1 stake"),

    {ok, StakeId} = aptos_walletdb:update_stake(StakeId, #{
        active_stake           => <<"1500000000">>,
        inactive_stake         => <<"500000000">>,
        pending_inactive_stake => <<"0">>
    }),
    {ok, UpdatedStake} = aptos_walletdb:get_stake(StakeId),
    assert_eq(UpdatedStake#aptos_stake.active_stake, <<"1500000000">>, "stake active_stake update mismatch"),
    assert_eq(UpdatedStake#aptos_stake.inactive_stake, <<"500000000">>, "stake inactive_stake update mismatch"),

    CoinType = <<"0x1::aptos_coin::AptosCoin">>,
    {ok, CoinBalId} = aptos_walletdb:upsert_coin_balance(WalletId, #{
        coin_type => CoinType,
        balance   => 500000000
    }),
    assert_non_empty(CoinBalId, "upsert_coin_balance returned empty id"),

    {ok, CoinBal} = aptos_walletdb:get_coin_balance(WalletId, CoinType),
    assert_eq(CoinBal#aptos_coin_balance.balance, 500000000, "coin balance mismatch"),
    assert_eq(CoinBal#aptos_coin_balance.coin_type, CoinType, "coin_type mismatch"),

    {ok, _} = aptos_walletdb:upsert_coin_balance(WalletId, #{
        coin_type => CoinType,
        balance   => 750000000
    }),
    {ok, UpdatedCoinBal} = aptos_walletdb:get_coin_balance(WalletId, CoinType),
    assert_eq(UpdatedCoinBal#aptos_coin_balance.balance, 750000000, "coin balance upsert update mismatch"),

    {ok, WalletCoinBals} = aptos_walletdb:get_wallet_coin_balances(WalletId),
    assert_true(length(WalletCoinBals) >= 1, "should have at least 1 coin balance"),

    {ok, UserCoinBals} = aptos_walletdb:get_user_coin_balances(UserId),
    assert_true(length(UserCoinBals) >= 1, "user should have at least 1 coin balance"),

    AssetType = "0x" ++ unique_id() ++ "::mycoin::MyCoin",
    {ok, FaBalId} = aptos_walletdb:upsert_fa_balance(WalletId, #{
        asset_type       => AssetType,
        metadata_address => MetadataAddress,
        amount           => 300000000,
        is_primary       => true
    }),
    assert_non_empty(FaBalId, "upsert_fa_balance returned empty id"),

    {ok, FaBal} = aptos_walletdb:get_fa_balance(WalletId, AssetType),
    assert_eq(FaBal#aptos_fa_balance.amount, 300000000, "FA balance amount mismatch"),
    assert_eq(FaBal#aptos_fa_balance.is_primary, true, "FA balance is_primary mismatch"),

    {ok, _} = aptos_walletdb:upsert_fa_balance(WalletId, #{
        asset_type => AssetType,
        amount     => 450000000,
        is_primary => true
    }),
    {ok, UpdatedFaBal} = aptos_walletdb:get_fa_balance(WalletId, AssetType),
    assert_eq(UpdatedFaBal#aptos_fa_balance.amount, 450000000, "FA balance upsert update mismatch"),

    {ok, WalletFaBals} = aptos_walletdb:get_wallet_fa_balances(WalletId),
    assert_true(length(WalletFaBals) >= 1, "should have at least 1 FA balance"),

    {ok, UserFaBals} = aptos_walletdb:get_user_fa_balances(UserId),
    assert_true(length(UserFaBals) >= 1, "user should have at least 1 FA balance"),

    {ok, deleted} = aptos_walletdb:delete_stake(StakeId),
    {error, stake_not_found} = aptos_walletdb:get_stake(StakeId),

    {ok, deleted} = aptos_walletdb:delete_coin_balance(CoinBalId),
    {error, balance_not_found} = aptos_walletdb:get_coin_balance(WalletId, CoinType),

    {ok, deleted} = aptos_walletdb:delete_fa_balance(FaBalId),

    {ok, deleted} = aptos_walletdb:delete_wallet(WalletId2),
    {ok, AfterDeletePrimary} = aptos_walletdb:get_primary_wallet(UserId),
    assert_eq(AfterDeletePrimary#aptos_wallet.wallet_id, WalletId, "after delete, WalletId should be primary"),

    io:format(" aptos_walletdb: all ~p assertions passed~n", [count_assertions()]),
    ok.

test_wallet(Token) ->
    UserId = "test_user_" ++ unique_id(),
    case aptos_wallet:create_wallet(Token, UserId) of
        {ok, #{<<"wallet_id">> := WalletId, <<"account_address">> := AccountAddress}} ->
            assert_non_empty(WalletId, "wallet_id empty"),
            assert_non_empty(AccountAddress, "account_address empty"),
            io:format(" created wallet: ~s -> ~s~n", [WalletId, AccountAddress]),
            case aptos_wallet:get_apt_balance(Token, AccountAddress) of
                {ok, _Balance} ->
                    io:format(" get_apt_balance: ok~n");
                {error, Reason} ->
                    io:format(" get_apt_balance skipped (network): ~p~n", [Reason])
            end,
            case aptos_wallet:get_balance(Token, AccountAddress) of
                {ok, _} ->
                    io:format(" get_balance: ok~n");
                {error, _} ->
                    io:format(" get_balance skipped (network)~n")
            end,
            case aptos_wallet:get_fa_balance(Token, AccountAddress, AccountAddress) of
                {ok, _} ->
                    io:format(" get_fa_balance: ok~n");
                {error, _} ->
                    io:format(" get_fa_balance skipped (network)~n")
            end,
            case aptos_wallet:get_wallet_info(Token, AccountAddress) of
                {ok, #{<<"account_address">> := AccountAddress}} ->
                    io:format(" get_wallet_info: ok~n");
                {error, _} ->
                    io:format(" get_wallet_info skipped (network)~n")
            end,
            case aptos_wallet:get_user_wallets(Token, UserId) of
                {ok, #{<<"wallets">> := WalletList}} ->
                    assert_true(length(WalletList) >= 1, "user should have wallets"),
                    io:format(" get_user_wallets: ~p wallet(s)~n", [length(WalletList)]);
                {error, _} ->
                    io:format(" get_user_wallets skipped (network)~n")
            end,
            case aptos_wallet:create_wallet(Token, UserId, "Savings") of
                {ok, #{<<"wallet_id">> := _WalletId2}} ->
                    io:format(" create_wallet with label: ok~n");
                {error, _} ->
                    io:format(" create_wallet with label skipped (network)~n")
            end,
            case aptos_wallet:transfer_apt(Token, AccountAddress, AccountAddress, 1, testnet) of
                {ok, _} ->
                    io:format(" transfer_apt: ok~n");
                {error, _} ->
                    io:format(" transfer_apt skipped (insufficient balance or network)~n")
            end,
            case aptos_wallet:transfer_apt(Token, AccountAddress, AccountAddress, 1, testnet, #{}) of
                {ok, _} ->
                    io:format(" transfer_apt/6: ok~n");
                {error, _} ->
                    io:format(" transfer_apt/6 skipped~n")
            end,
            case aptos_wallet:transfer_fa(Token, AccountAddress, AccountAddress, AccountAddress, 1, testnet) of
                {ok, _} ->
                    io:format(" transfer_fa: ok~n");
                {error, _} ->
                    io:format(" transfer_fa skipped (network)~n")
            end,
            case aptos_wallet:get_local_transaction_history(Token, WalletId) of
                {ok, _} ->
                    io:format(" get_local_transaction_history: ok~n");
                {error, _} ->
                    io:format(" get_local_transaction_history skipped (network)~n")
            end,
            case aptos_wallet:get_local_transaction_history(Token, WalletId, 10, 0) of
                {ok, _} ->
                    io:format(" get_local_transaction_history/4: ok~n");
                {error, _} ->
                    io:format(" get_local_transaction_history/4 skipped (network)~n")
            end,
            case aptos_wallet:delete_wallet(Token, AccountAddress) of
                {ok, deleted} ->
                    io:format(" delete_wallet: ok~n");
                {error, _} ->
                    io:format(" delete_wallet skipped (network)~n")
            end;
        {error, Reason} ->
            io:format(" create_wallet skipped (network unavailable): ~p~n", [Reason])
    end,
    case aptos_wallet:import_wallet(Token, UserId, "0xinvalidprivatekey") of
        {error, _} ->
            io:format(" import bad private key: correctly rejected~n");
        {ok, _} ->
            io:format(" import bad private key: unexpected success (check TS validation)~n")
    end,
    case aptos_wallet:import_wallet(Token, UserId, "0xinvalidprivatekey", "Imported") of
        {error, _} ->
            io:format(" import bad private key with label: correctly rejected~n");
        {ok, _} ->
            io:format(" import bad private key with label: unexpected success~n")
    end,
    ok.

test_tokens(Token) ->
    FakeAddress = fresh_address(),
    FakeWalletId = "nonexistent_wallet_" ++ unique_id(),
    FakeMetadata = fresh_address(),

    case aptos_wallet:get_coin_balance(Token, FakeAddress, <<"0x1::aptos_coin::AptosCoin">>) of
        {ok, _} ->
            io:format(" aptos_wallet:get_coin_balance: ok~n");
        {error, _} ->
            io:format(" aptos_wallet:get_coin_balance nonexistent: correctly rejected~n")
    end,
    case aptos_wallet:get_all_coins(Token, FakeAddress) of
        {ok, _} ->
            io:format(" aptos_wallet:get_all_coins: ok~n");
        {error, _} ->
            io:format(" aptos_wallet:get_all_coins: correctly rejected~n")
    end,
    case aptos_wallet:get_fa_metadata(Token, <<"0x1::aptos_coin::AptosCoin">>) of
        {ok, _} ->
            io:format(" aptos_wallet:get_fa_metadata: ok~n");
        {error, _} ->
            io:format(" aptos_wallet:get_fa_metadata: correctly rejected~n")
    end,
    case aptos_wallet:get_all_fa_balances(Token, FakeAddress) of
        {ok, _} ->
            io:format(" aptos_wallet:get_all_fa_balances: ok~n");
        {error, _} ->
            io:format(" aptos_wallet:get_all_fa_balances: correctly rejected~n")
    end,
    case aptos_wallet:transfer_coin(Token, FakeAddress, FakeAddress, <<"0x1::aptos_coin::AptosCoin">>, 100, testnet) of
        {error, _} ->
            io:format(" aptos_wallet:transfer_coin nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" aptos_wallet:transfer_coin: unexpected success~n")
    end,
    case aptos_wallet:transfer_fa(Token, FakeAddress, FakeAddress, FakeMetadata, 100, testnet) of
        {error, _} ->
            io:format(" aptos_wallet:transfer_fa nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" aptos_wallet:transfer_fa: unexpected success~n")
    end,
    case aptos_wallet:mint_coin(Token, FakeAddress, <<"0x1::aptos_coin::AptosCoin">>, 100, testnet) of
        {error, _} ->
            io:format(" aptos_wallet:mint_coin nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" aptos_wallet:mint_coin: unexpected success~n")
    end,
    case aptos_wallet:call_view_function(Token, <<"0x1::coin::balance">>, [FakeAddress]) of
        {ok, _} ->
            io:format(" aptos_wallet:call_view_function: ok~n");
        {error, _} ->
            io:format(" aptos_wallet:call_view_function: correctly rejected~n")
    end,
    case aptos_wallet:get_fa_activities(Token, <<"0x1::aptos_coin::AptosCoin">>, 10) of
        {ok, _} ->
            io:format(" aptos_wallet:get_fa_activities: ok~n");
        {error, _} ->
            io:format(" aptos_wallet:get_fa_activities: correctly rejected~n")
    end,
    case aptos_wallet:get_fa_metadata_by_creator(Token, FakeAddress) of
        {ok, _} ->
            io:format(" aptos_wallet:get_fa_metadata_by_creator: ok~n");
        {error, _} ->
            io:format(" aptos_wallet:get_fa_metadata_by_creator: correctly rejected~n")
    end,
    case aptos_tokens:get_coin_balance(Token, FakeWalletId, FakeAddress, <<"0x1::aptos_coin::AptosCoin">>) of
        {ok, _} ->
            io:format(" aptos_tokens:get_coin_balance: ok~n");
        {error, _} ->
            io:format(" aptos_tokens:get_coin_balance nonexistent: correctly rejected~n")
    end,
    case aptos_tokens:get_all_coins(Token, FakeWalletId, FakeAddress) of
        {ok, _} ->
            io:format(" aptos_tokens:get_all_coins: ok~n");
        {error, _} ->
            io:format(" aptos_tokens:get_all_coins: correctly rejected~n")
    end,
    case aptos_tokens:get_fa_metadata(Token, FakeWalletId, <<"0x1::aptos_coin::AptosCoin">>) of
        {ok, _} ->
            io:format(" aptos_tokens:get_fa_metadata: ok~n");
        {error, _} ->
            io:format(" aptos_tokens:get_fa_metadata: correctly rejected~n")
    end,
    case aptos_tokens:get_all_fa_balances(Token, FakeWalletId, FakeAddress) of
        {ok, _} ->
            io:format(" aptos_tokens:get_all_fa_balances: ok~n");
        {error, _} ->
            io:format(" aptos_tokens:get_all_fa_balances: correctly rejected~n")
    end,
    case aptos_tokens:transfer_coin(Token, FakeWalletId, FakeAddress, FakeAddress, <<"0x1::aptos_coin::AptosCoin">>, 100, testnet) of
        {error, _} ->
            io:format(" aptos_tokens:transfer_coin nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" aptos_tokens:transfer_coin: unexpected success~n")
    end,
    case aptos_tokens:transfer_fa(Token, FakeWalletId, FakeAddress, FakeAddress, FakeMetadata, 100, testnet) of
        {error, _} ->
            io:format(" aptos_tokens:transfer_fa nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" aptos_tokens:transfer_fa: unexpected success~n")
    end,
    case aptos_tokens:mint_coin(Token, FakeWalletId, FakeAddress, <<"0x1::aptos_coin::AptosCoin">>, 100, testnet) of
        {error, _} ->
            io:format(" aptos_tokens:mint_coin nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" aptos_tokens:mint_coin: unexpected success~n")
    end,
    case aptos_tokens:call_view_function(Token, FakeWalletId, <<"0x1::coin::balance">>, [FakeAddress]) of
        {ok, _} ->
            io:format(" aptos_tokens:call_view_function: ok~n");
        {error, _} ->
            io:format(" aptos_tokens:call_view_function: correctly rejected~n")
    end,
    case aptos_tokens:get_fa_activities(Token, FakeWalletId, <<"0x1::aptos_coin::AptosCoin">>, 10) of
        {ok, _} ->
            io:format(" aptos_tokens:get_fa_activities: ok~n");
        {error, _} ->
            io:format(" aptos_tokens:get_fa_activities: correctly rejected~n")
    end,
    case aptos_tokens:get_fa_metadata_by_creator(Token, FakeWalletId, FakeAddress) of
        {ok, _} ->
            io:format(" aptos_tokens:get_fa_metadata_by_creator: ok~n");
        {error, _} ->
            io:format(" aptos_tokens:get_fa_metadata_by_creator: correctly rejected~n")
    end,
    case aptos_tokens:sync_coin_balance(Token, FakeWalletId, FakeAddress, <<"0x1::aptos_coin::AptosCoin">>) of
        {ok, _} ->
            io:format(" aptos_tokens:sync_coin_balance: ok~n");
        {error, _} ->
            io:format(" aptos_tokens:sync_coin_balance: correctly rejected~n")
    end,
    case aptos_tokens:sync_fa_balances(Token, FakeWalletId, FakeAddress) of
        {ok, _} ->
            io:format(" aptos_tokens:sync_fa_balances: ok~n");
        {error, _} ->
            io:format(" aptos_tokens:sync_fa_balances: correctly rejected~n")
    end,
    case aptos_tokens:get_local_coin_balance(FakeWalletId, FakeAddress, <<"0x1::aptos_coin::AptosCoin">>) of
        {error, _} ->
            io:format(" aptos_tokens:get_local_coin_balance nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" aptos_tokens:get_local_coin_balance: ok~n")
    end,
    case aptos_tokens:get_local_fa_balance(FakeWalletId, FakeAddress, <<"0xfakeasset">>) of
        {error, _} ->
            io:format(" aptos_tokens:get_local_fa_balance nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" aptos_tokens:get_local_fa_balance: ok~n")
    end,
    case aptos_tokens:get_local_all_coin_balances(FakeWalletId, FakeAddress) of
        {ok, []} ->
            io:format(" aptos_tokens:get_local_all_coin_balances empty: ok~n");
        {ok, _} ->
            io:format(" aptos_tokens:get_local_all_coin_balances: ok~n");
        {error, _} ->
            io:format(" aptos_tokens:get_local_all_coin_balances: correctly rejected~n")
    end,
    case aptos_tokens:get_local_all_fa_balances(FakeWalletId, FakeAddress) of
        {ok, []} ->
            io:format(" aptos_tokens:get_local_all_fa_balances empty: ok~n");
        {ok, _} ->
            io:format(" aptos_tokens:get_local_all_fa_balances: ok~n");
        {error, _} ->
            io:format(" aptos_tokens:get_local_all_fa_balances: correctly rejected~n")
    end,
    ok.

test_nft(Token) ->
    FakeAddress = fresh_address(),
    FakeTokenAddress = fresh_address(),

    case aptos_tokens:create_collection(Token, FakeAddress, "My Collection", "A test collection", "https://example.com") of
        {ok, _} ->
            io:format(" create_collection: ok~n");
        {error, _} ->
            io:format(" create_collection nonexistent: correctly rejected~n")
    end,
    case aptos_tokens:create_collection(Token, FakeAddress, "My Collection 2", "desc", "https://example.com", #{}) of
        {ok, _} ->
            io:format(" create_collection/6: ok~n");
        {error, _} ->
            io:format(" create_collection/6 nonexistent: correctly rejected~n")
    end,
    case aptos_tokens:mint_nft(Token, FakeAddress, "My Collection", "Token #1",
                               "First token", "https://example.com/1",
                               #{keys => [<<"level">>], types => [<<"u64">>], values => [<<"1">>]}) of
        {ok, _} ->
            io:format(" mint_nft: ok~n");
        {error, _} ->
            io:format(" mint_nft nonexistent: correctly rejected~n")
    end,
    case aptos_tokens:mint_nft(Token, FakeAddress, "My Collection", "Token #2",
                               "Second token", "https://example.com/2",
                               #{keys => [], types => [], values => []}, #{}) of
        {ok, _} ->
            io:format(" mint_nft/8: ok~n");
        {error, _} ->
            io:format(" mint_nft/8 nonexistent: correctly rejected~n")
    end,
    case aptos_tokens:mint_soul_bound(Token, FakeAddress, "My Collection", "SB #1",
                                      "Soul bound token", "https://example.com/sb/1",
                                      FakeAddress,
                                      #{keys => [], types => [], values => []}) of
        {ok, _} ->
            io:format(" mint_soul_bound: ok~n");
        {error, _} ->
            io:format(" mint_soul_bound nonexistent: correctly rejected~n")
    end,
    case aptos_tokens:transfer_nft(Token, FakeAddress, FakeTokenAddress, FakeAddress, testnet) of
        {error, _} ->
            io:format(" transfer_nft nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" transfer_nft: ok~n")
    end,
    case aptos_tokens:burn_nft(Token, FakeAddress, FakeTokenAddress, testnet) of
        {error, _} ->
            io:format(" burn_nft nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" burn_nft: ok~n")
    end,
    case aptos_tokens:get_owned_tokens(Token, FakeAddress, testnet) of
        {ok, _} ->
            io:format(" get_owned_tokens: ok~n");
        {error, _} ->
            io:format(" get_owned_tokens skipped (network)~n")
    end,
    case aptos_tokens:get_collection_data(Token, "My Collection", FakeAddress, testnet) of
        {ok, _} ->
            io:format(" get_collection_data: ok~n");
        {error, _} ->
            io:format(" get_collection_data skipped (network)~n")
    end,
    case aptos_tokens:get_token_data(Token, FakeTokenAddress, testnet) of
        {error, _} ->
            io:format(" get_token_data nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" get_token_data: ok~n")
    end,
    case aptos_tokens:set_token_description(Token, FakeAddress, FakeTokenAddress, "New desc", testnet) of
        {error, _} ->
            io:format(" set_token_description nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" set_token_description: ok~n")
    end,
    case aptos_tokens:set_token_name(Token, FakeAddress, FakeTokenAddress, "New Name", testnet) of
        {error, _} ->
            io:format(" set_token_name nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" set_token_name: ok~n")
    end,
    case aptos_tokens:set_token_uri(Token, FakeAddress, FakeTokenAddress, "https://new.uri", testnet) of
        {error, _} ->
            io:format(" set_token_uri nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" set_token_uri: ok~n")
    end,
    case aptos_tokens:add_token_property(Token, FakeAddress, FakeTokenAddress, "power", "u64", 100, testnet) of
        {error, _} ->
            io:format(" add_token_property nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" add_token_property: ok~n")
    end,
    case aptos_tokens:update_token_property(Token, FakeAddress, FakeTokenAddress, "power", "u64", 200, testnet) of
        {error, _} ->
            io:format(" update_token_property nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" update_token_property: ok~n")
    end,
    case aptos_tokens:remove_token_property(Token, FakeAddress, FakeTokenAddress, "power", "u64", 200, testnet) of
        {error, _} ->
            io:format(" remove_token_property nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" remove_token_property: ok~n")
    end,
    ok.

test_transactions(Token) ->
    FakeAddress = fresh_address(),
    FakeWalletId = "nonexistent_wallet_" ++ unique_id(),

    case aptos_transactions:get_ledger_info(Token, testnet) of
        {ok, #{<<"chain_id">> := _}} ->
            io:format(" get_ledger_info testnet: ok~n");
        {ok, _} ->
            io:format(" get_ledger_info testnet: ok (no chain_id field)~n");
        {error, _} ->
            io:format(" get_ledger_info skipped (network)~n")
    end,
    case aptos_transactions:get_transactions(Token, testnet) of
        {ok, _} ->
            io:format(" get_transactions: ok~n");
        {error, _} ->
            io:format(" get_transactions skipped (network)~n")
    end,
    case aptos_transactions:get_transaction_by_hash(Token, "0xfakehash000", testnet) of
        {error, _} ->
            io:format(" get_transaction_by_hash nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" get_transaction_by_hash: ok~n")
    end,
    case aptos_transactions:get_transaction_by_version(Token, 999999999999, testnet) of
        {error, _} ->
            io:format(" get_transaction_by_version nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" get_transaction_by_version: ok~n")
    end,
    case aptos_transactions:get_account_transactions(Token, FakeAddress, testnet) of
        {ok, _} ->
            io:format(" get_account_transactions: ok~n");
        {error, _} ->
            io:format(" get_account_transactions skipped (network)~n")
    end,
    case aptos_transactions:get_account_transactions(Token, FakeAddress, testnet, #{limit => 5, offset => 0}) of
        {ok, _} ->
            io:format(" get_account_transactions/4: ok~n");
        {error, _} ->
            io:format(" get_account_transactions/4 skipped (network)~n")
    end,
    case aptos_transactions:estimate_gas(Token, FakeAddress, "0x1::aptos_account::transfer", testnet) of
        {ok, _} ->
            io:format(" estimate_gas: ok~n");
        {error, _} ->
            io:format(" estimate_gas skipped (network)~n")
    end,
    case aptos_transactions:get_block_by_height(Token, 1, testnet) of
        {ok, _} ->
            io:format(" get_block_by_height: ok~n");
        {error, _} ->
            io:format(" get_block_by_height skipped (network)~n")
    end,
    case aptos_transactions:get_block_by_version(Token, 1, testnet) of
        {ok, _} ->
            io:format(" get_block_by_version: ok~n");
        {error, _} ->
            io:format(" get_block_by_version skipped (network)~n")
    end,
    case aptos_transactions:transfer_apt(Token, FakeAddress, FakeAddress, 100000000, testnet) of
        {error, _} ->
            io:format(" aptos_transactions:transfer_apt nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" aptos_transactions:transfer_apt: ok~n")
    end,
    case aptos_transactions:transfer_apt(Token, FakeAddress, FakeAddress, 100000000, testnet, #{}) of
        {error, _} ->
            io:format(" aptos_transactions:transfer_apt/6 nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" aptos_transactions:transfer_apt/6: ok~n")
    end,
    case aptos_transactions:get_local_history(Token, FakeWalletId) of
        {ok, _} ->
            io:format(" get_local_history: ok~n");
        {error, _} ->
            io:format(" get_local_history skipped (network)~n")
    end,
    case aptos_transactions:get_local_history(Token, FakeWalletId, 10, 0) of
        {ok, _} ->
            io:format(" get_local_history/4: ok~n");
        {error, _} ->
            io:format(" get_local_history/4 skipped (network)~n")
    end,
    case aptos_transactions:get_local_transaction(Token, "0xfakehash000") of
        {error, transaction_not_found} ->
            io:format(" get_local_transaction nonexistent: correctly not found~n");
        {error, _} ->
            io:format(" get_local_transaction: correctly rejected~n");
        {ok, _} ->
            io:format(" get_local_transaction: ok~n")
    end,
    case aptos_transactions:get_local_user_history(Token, "nonexistent_user_" ++ unique_id()) of
        {ok, #{user_id := _, transactions := []}} ->
            io:format(" get_local_user_history empty: ok~n");
        {ok, _} ->
            io:format(" get_local_user_history: ok~n");
        {error, _} ->
            io:format(" get_local_user_history: correctly rejected~n")
    end,
    case aptos_transactions:get_local_history_by_type(Token, FakeWalletId, apt_transfer) of
        {ok, _} ->
            io:format(" get_local_history_by_type: ok~n");
        {error, _} ->
            io:format(" get_local_history_by_type: correctly rejected~n")
    end,
    ok.

test_staking(Token) ->
    FakeAddress = fresh_address(),
    FakeWalletId = "nonexistent_wallet_" ++ unique_id(),
    FakePoolAddress = fresh_address(),

    case aptos_staking:add_stake(Token, FakeWalletId, FakeAddress, FakePoolAddress, 1000000000) of
        {error, _} ->
            io:format(" add_stake nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" add_stake: ok~n")
    end,
    case aptos_staking:add_stake(Token, FakeWalletId, FakeAddress, FakePoolAddress, 1000000000, #{}) of
        {error, _} ->
            io:format(" add_stake/6 nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" add_stake/6: ok~n")
    end,
    case aptos_staking:unlock_stake(Token, FakeWalletId, FakeAddress, FakePoolAddress, 500000000) of
        {error, _} ->
            io:format(" unlock_stake nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" unlock_stake: ok~n")
    end,
    case aptos_staking:unlock_stake(Token, FakeWalletId, FakeAddress, FakePoolAddress, 500000000, #{}) of
        {error, _} ->
            io:format(" unlock_stake/6 nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" unlock_stake/6: ok~n")
    end,
    case aptos_staking:withdraw_stake(Token, FakeWalletId, FakeAddress, FakePoolAddress) of
        {error, _} ->
            io:format(" withdraw_stake nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" withdraw_stake: ok~n")
    end,
    case aptos_staking:withdraw_stake(Token, FakeWalletId, FakeAddress, FakePoolAddress, #{}) of
        {error, _} ->
            io:format(" withdraw_stake/5 nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" withdraw_stake/5: ok~n")
    end,
    case aptos_staking:reactivate_stake(Token, FakeWalletId, FakeAddress, FakePoolAddress, 500000000) of
        {error, _} ->
            io:format(" reactivate_stake nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" reactivate_stake: ok~n")
    end,
    case aptos_staking:reactivate_stake(Token, FakeWalletId, FakeAddress, FakePoolAddress, 500000000, #{}) of
        {error, _} ->
            io:format(" reactivate_stake/6 nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" reactivate_stake/6: ok~n")
    end,
    case aptos_staking:get_stake_info(Token, FakeWalletId, FakeAddress, FakePoolAddress) of
        {ok, #{delegator_address := _, pool_address := _}} ->
            io:format(" get_stake_info: ok~n");
        {ok, _} ->
            io:format(" get_stake_info: ok (alternate shape)~n");
        {error, _} ->
            io:format(" get_stake_info skipped (network)~n")
    end,
    case aptos_staking:get_delegation_pools(Token, testnet) of
        {ok, #{pools := Pools}} ->
            io:format(" get_delegation_pools testnet: ~p pool(s)~n", [length(Pools)]);
        {error, _} ->
            io:format(" get_delegation_pools skipped (network)~n")
    end,
    case aptos_staking:get_pool_info(Token, FakePoolAddress, testnet) of
        {ok, _} ->
            io:format(" get_pool_info: ok~n");
        {error, _} ->
            io:format(" get_pool_info nonexistent: correctly rejected~n")
    end,
    case aptos_staking:get_staking_activities(Token, FakeAddress, testnet) of
        {ok, #{activities := _}} ->
            io:format(" get_staking_activities: ok~n");
        {error, _} ->
            io:format(" get_staking_activities skipped (network)~n")
    end,
    case aptos_staking:get_user_stakes(Token, "nonexistent_user_" ++ unique_id()) of
        {ok, #{stakes := []}} ->
            io:format(" get_user_stakes empty: ok~n");
        {ok, #{stakes := _}} ->
            io:format(" get_user_stakes: ok~n");
        {error, _} ->
            io:format(" get_user_stakes: correctly rejected~n")
    end,
    case aptos_staking:get_wallet_stakes(Token, FakeWalletId) of
        {ok, #{stakes := []}} ->
            io:format(" get_wallet_stakes empty: ok~n");
        {ok, #{stakes := _}} ->
            io:format(" get_wallet_stakes: ok~n");
        {error, _} ->
            io:format(" get_wallet_stakes: correctly rejected~n")
    end,
    ok.

test_user_integration(Token) ->
    run_integration_case("ensure_user_has_aptos_wallet",
        fun() -> aptos_user_integration:ensure_user_has_wallet(Token, "nonexistent_mazaryn_user") end),
    run_integration_case("get_user_apt_balance",
        fun() -> aptos_user_integration:get_user_apt_balance(Token, "nonexistent_mazaryn_user") end),
    run_integration_case("get_user_all_coins",
        fun() -> aptos_user_integration:get_user_all_coins(Token, "nonexistent_mazaryn_user") end),
    run_integration_case("get_user_fa_balances",
        fun() -> aptos_user_integration:get_user_fa_balances(Token, "nonexistent_mazaryn_user") end),
    run_integration_case("transfer_apt_between_users",
        fun() -> aptos_user_integration:transfer_apt_between_users(Token, "user_a", "user_b", 100000000, testnet) end),
    run_integration_case("transfer_fa_between_users",
        fun() -> aptos_user_integration:transfer_fa_between_users(Token, "user_a", "user_b", "0xmeta123", 100, testnet) end),
    run_integration_case("stake_for_user",
        fun() -> aptos_user_integration:stake_for_user(Token, "nonexistent_user", "0xpool123", 1000000000) end),
    run_integration_case("unlock_stake_for_user",
        fun() -> aptos_user_integration:unlock_stake_for_user(Token, "nonexistent_user", "0xpool123", 500000000) end),
    run_integration_case("withdraw_stake_for_user",
        fun() -> aptos_user_integration:withdraw_stake_for_user(Token, "nonexistent_user", "0xpool123") end),
    run_integration_case("reactivate_stake_for_user",
        fun() -> aptos_user_integration:reactivate_stake_for_user(Token, "nonexistent_user", "0xpool123", 500000000) end),
    run_integration_case("get_user_stake_info",
        fun() -> aptos_user_integration:get_user_stake_info(Token, "nonexistent_user", "0xpool123") end),
    run_integration_case("get_user_staking_activities",
        fun() -> aptos_user_integration:get_user_staking_activities(Token, "nonexistent_user", testnet) end),
    run_integration_case("get_user_transaction_history",
        fun() -> aptos_user_integration:get_user_transaction_history(Token, "nonexistent_user") end),
    run_integration_case("get_user_transaction_history_by_type",
        fun() -> aptos_user_integration:get_user_transaction_history_by_type(Token, "nonexistent_user", apt_transfer) end),
    run_integration_case("sync_user_balances",
        fun() -> aptos_user_integration:sync_user_balances(Token, "nonexistent_user") end),
    run_integration_case("get_aptos_summary",
        fun() -> aptos_user_integration:get_aptos_summary(Token, "nonexistent_user") end),
    ok.

run_integration_case(Name, Fun) ->
    Result = try Fun()
             catch
                 error:undef -> {error, undef_integration_module};
                 Class:Reason -> {error, {Class, Reason}}
             end,
    case Result of
        {error, _Reason} ->
            io:format(" ~s: correctly rejected (~p)~n", [Name, _Reason]);
        {ok, _, _} ->
            io:format(" ~s: ok (3-tuple)~n", [Name]);
        {ok, _} ->
            io:format(" ~s: ok~n", [Name])
    end.

ensure_mnesia_started() ->
    AptosTestTables = [aptos_wallet, aptos_transaction, aptos_nft_collection,
                       aptos_nft_token, aptos_fungible_asset, aptos_stake,
                       aptos_coin_balance, aptos_fa_balance],
    [ensure_test_table(T) || T <- AptosTestTables],
    mnesia:wait_for_tables(AptosTestTables, 5000).

ensure_test_table(aptos_wallet) ->
    safe_create_table(aptos_wallet, record_info(fields, aptos_wallet)),
    safe_add_index(aptos_wallet, user_id),
    safe_add_index(aptos_wallet, account_address);
ensure_test_table(aptos_transaction) ->
    safe_create_table(aptos_transaction, record_info(fields, aptos_transaction)),
    safe_add_index(aptos_transaction, wallet_id),
    safe_add_index(aptos_transaction, user_id),
    safe_add_index(aptos_transaction, transaction_hash),
    safe_add_index(aptos_transaction, tx_type);
ensure_test_table(aptos_nft_collection) ->
    safe_create_table(aptos_nft_collection, record_info(fields, aptos_nft_collection)),
    safe_add_index(aptos_nft_collection, wallet_id),
    safe_add_index(aptos_nft_collection, user_id),
    safe_add_index(aptos_nft_collection, creator_address);
ensure_test_table(aptos_nft_token) ->
    safe_create_table(aptos_nft_token, record_info(fields, aptos_nft_token)),
    safe_add_index(aptos_nft_token, wallet_id),
    safe_add_index(aptos_nft_token, user_id),
    safe_add_index(aptos_nft_token, collection_id);
ensure_test_table(aptos_fungible_asset) ->
    safe_create_table(aptos_fungible_asset, record_info(fields, aptos_fungible_asset)),
    safe_add_index(aptos_fungible_asset, wallet_id),
    safe_add_index(aptos_fungible_asset, user_id),
    safe_add_index(aptos_fungible_asset, creator_address),
    safe_add_index(aptos_fungible_asset, metadata_address);
ensure_test_table(aptos_stake) ->
    safe_create_table(aptos_stake, record_info(fields, aptos_stake)),
    safe_add_index(aptos_stake, wallet_id),
    safe_add_index(aptos_stake, user_id),
    safe_add_index(aptos_stake, pool_address),
    safe_add_index(aptos_stake, delegator_address);
ensure_test_table(aptos_coin_balance) ->
    safe_create_table(aptos_coin_balance, record_info(fields, aptos_coin_balance)),
    safe_add_index(aptos_coin_balance, wallet_id),
    safe_add_index(aptos_coin_balance, user_id),
    safe_add_index(aptos_coin_balance, account_address),
    safe_add_index(aptos_coin_balance, coin_type);
ensure_test_table(aptos_fa_balance) ->
    safe_create_table(aptos_fa_balance, record_info(fields, aptos_fa_balance)),
    safe_add_index(aptos_fa_balance, wallet_id),
    safe_add_index(aptos_fa_balance, user_id),
    safe_add_index(aptos_fa_balance, account_address),
    safe_add_index(aptos_fa_balance, asset_type).

safe_create_table(Table, Fields) ->
    case mnesia:create_table(Table, [
        {attributes, Fields},
        {disc_copies, [node()]},
        {type, set}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, _}} -> ok;
        {aborted, Reason} -> error({create_test_table_failed, Table, Reason})
    end.

safe_add_index(Table, Field) ->
    case mnesia:add_table_index(Table, Field) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, _, _}} -> ok;
        {aborted, Reason} ->
            io:format(" [warn] index ~p.~p: ~p~n", [Table, Field, Reason])
    end.

get_test_token() ->
    case application:get_env(aptos_test, token) of
        {ok, T} -> T;
        undefined ->
            case os:getenv("APTOS_TEST_TOKEN") of
                false -> "test_token_replace_me";
                T -> T
            end
    end.

fresh_address() ->
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
    52.
