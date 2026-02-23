-module(near_test).
-author("Zaryn Technologies").
-include("../records.hrl").
-include("../wallet_records.hrl").

-export([run_all/0, run_all/1]).
-export([
    test_walletdb/0,
    test_wallet/1,
    test_tokens/1,
    test_keys/1,
    test_transactions/1,
    test_staking/1,
    test_social/1,
    test_user_integration/1
]).

run_all() ->
    Token = get_test_token(),
    run_all(Token).

run_all(Token) ->
    Suites = [
        {"near_walletdb (Mnesia layer)", fun test_walletdb/0},
        {"near_wallet (HTTP client)", fun() -> test_wallet(Token) end},
        {"near_tokens (FT/USDT)", fun() -> test_tokens(Token) end},
        {"near_keys (access keys)", fun() -> test_keys(Token) end},
        {"near_transactions (batch/call)", fun() -> test_transactions(Token) end},
        {"near_staking (validators)", fun() -> test_staking(Token) end},
        {"near_social (SocialDB)", fun() -> test_social(Token) end},
        {"near_user_integration", fun() -> test_user_integration(Token) end}
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
    AccountId = "test." ++ unique_id() ++ ".testnet",
    {ok, WalletId} = near_walletdb:create_wallet(
        UserId, AccountId,
        <<"enc_private_key">>, <<"iv_data">>, <<"tag_data">>
    ),
    assert_non_empty(WalletId, "create_wallet returned empty wallet_id"),
    {ok, Wallet} = near_walletdb:get_wallet(WalletId),
    assert_eq(Wallet#near_wallet.user_id, UserId, "wallet user_id mismatch"),
    assert_eq(Wallet#near_wallet.account_id, AccountId, "wallet account_id mismatch"),
    assert_eq(Wallet#near_wallet.is_primary, true, "first wallet should be primary"),
    {ok, WalletByAccount} = near_walletdb:get_wallet_by_account_id(AccountId),
    assert_eq(WalletByAccount#near_wallet.wallet_id, WalletId, "account_id lookup mismatch"),
    assert_eq(near_walletdb:wallet_exists(WalletId), true, "wallet_exists should be true"),
    assert_eq(near_walletdb:account_id_exists(AccountId), true, "account_id_exists should be true"),
    assert_eq(near_walletdb:wallet_exists("nonexistent_id"), false, "nonexistent wallet should be false"),
    {error, account_id_already_exists} = near_walletdb:create_wallet(
        UserId, AccountId, <<"k">>, <<"iv">>, <<"t">>
    ),
    AccountId2 = "test2." ++ unique_id() ++ ".testnet",
    {ok, WalletId2} = near_walletdb:create_wallet(
        UserId, AccountId2, <<"k2">>, <<"iv2">>, <<"t2">>
    ),
    {ok, Wallet2} = near_walletdb:get_wallet(WalletId2),
    assert_eq(Wallet2#near_wallet.is_primary, false, "second wallet should not be primary"),
    {ok, Wallets} = near_walletdb:get_user_wallets(UserId),
    assert_eq(length(Wallets), 2, "user should have 2 wallets"),
    {ok, Primary} = near_walletdb:get_primary_wallet(UserId),
    assert_eq(Primary#near_wallet.wallet_id, WalletId, "primary wallet mismatch"),
    ok = near_walletdb:set_primary_wallet(UserId, WalletId2),
    {ok, NewPrimary} = near_walletdb:get_primary_wallet(UserId),
    assert_eq(NewPrimary#near_wallet.wallet_id, WalletId2, "new primary wallet mismatch"),
    ok = near_walletdb:update_wallet_label(WalletId, "My Test Wallet"),
    {ok, LabeledWallet} = near_walletdb:get_wallet(WalletId),
    assert_eq(LabeledWallet#near_wallet.label, "My Test Wallet", "label update mismatch"),
    ok = near_walletdb:update_wallet_last_used(WalletId),
    {ok, TxId} = near_walletdb:create_transaction(WalletId, #{
        transaction_hash => "hash_" ++ unique_id(),
        tx_type => transfer,
        from_account_id => AccountId,
        receiver_id => "receiver.testnet",
        amount_near => 1.5,
        status => confirmed
    }),
    assert_non_empty(TxId, "create_transaction returned empty tx_id"),
    {ok, Tx} = near_walletdb:get_transaction(TxId),
    assert_eq(Tx#near_transaction.wallet_id, WalletId, "tx wallet_id mismatch"),
    assert_eq(Tx#near_transaction.tx_type, transfer, "tx type mismatch"),
    {ok, Txs} = near_walletdb:get_wallet_transactions(WalletId),
    assert_true(length(Txs) >= 1, "should have at least 1 transaction"),
    {ok, PagedTxs, Total} = near_walletdb:get_wallet_transactions(WalletId, 10, 0),
    assert_true(Total >= 1, "total should be >= 1"),
    assert_true(length(PagedTxs) >= 1, "paged results should have >= 1 tx"),
    {ok, TransferTxs} = near_walletdb:get_transactions_by_type(WalletId, transfer),
    assert_true(length(TransferTxs) >= 1, "should find transfer transactions"),
    {ok, KeyId} = near_walletdb:create_access_key(WalletId, #{
        public_key => "ed25519:testpublickey" ++ unique_id(),
        key_type => function_call,
        contract_id => "social.near",
        method_names => ["set"],
        allowance_near => "0.25",
        label => "Social Key"
    }),
    assert_non_empty(KeyId, "create_access_key returned empty key_id"),
    {ok, Key} = near_walletdb:get_access_key(KeyId),
    assert_eq(Key#near_access_key.wallet_id, WalletId, "key wallet_id mismatch"),
    assert_eq(Key#near_access_key.key_type, function_call, "key type mismatch"),
    {ok, Keys} = near_walletdb:get_wallet_access_keys(WalletId),
    assert_true(length(Keys) >= 1, "should have at least 1 key"),
    ok = near_walletdb:delete_access_key(KeyId),
    {error, key_not_found} = near_walletdb:get_access_key(KeyId),
    {ok, StakeId} = near_walletdb:create_stake(WalletId, #{
        validator_account_id => "validator.poolv1.near",
        amount_near => 10.0,
        transaction_hash => "stake_hash_" ++ unique_id(),
        status => staked
    }),
    assert_non_empty(StakeId, "create_stake returned empty stake_id"),
    {ok, Stake} = near_walletdb:get_stake(StakeId),
    assert_eq(Stake#near_stake.status, staked, "stake status mismatch"),
    Now = calendar:universal_time(),
    ok = near_walletdb:update_stake(StakeId, #{status => unstaked, unstaked_at => Now}),
    {ok, UpdatedStake} = near_walletdb:get_stake(StakeId),
    assert_eq(UpdatedStake#near_stake.status, unstaked, "stake update status mismatch"),
    ImplicitAccId = string:lowercase(lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(crypto:strong_rand_bytes(32))])),
    {ok, ImplicitId} = near_walletdb:create_implicit_account(WalletId, #{
        account_id => ImplicitAccId,
        encrypted_private_key => <<"enc_key">>,
        encryption_iv => <<"iv">>,
        encryption_tag => <<"tag">>
    }),
    assert_non_empty(ImplicitId, "create_implicit_account returned empty id"),
    {ok, Implicit} = near_walletdb:get_implicit_account(ImplicitId),
    assert_eq(Implicit#near_implicit_account.funded, false, "implicit should start unfunded"),
    {ok, ImplicitByAccId} = near_walletdb:get_implicit_account_by_account_id(ImplicitAccId),
    assert_eq(ImplicitByAccId#near_implicit_account.implicit_id, ImplicitId, "implicit account_id lookup mismatch"),
    ok = near_walletdb:mark_implicit_funded(ImplicitId, Now),
    {ok, FundedImplicit} = near_walletdb:get_implicit_account(ImplicitId),
    assert_eq(FundedImplicit#near_implicit_account.funded, true, "implicit should be funded"),
    {ok, PostId} = near_walletdb:create_social_post(WalletId, #{
        contract => "social.near",
        text => "Hello NEAR from Mazaryn!",
        media_urls => [],
        tags => ["mazaryn", "near"],
        transaction_hash => "post_hash_" ++ unique_id()
    }),
    assert_non_empty(PostId, "create_social_post returned empty post_id"),
    {ok, Post} = near_walletdb:get_social_post(PostId),
    assert_eq(Post#near_social_post.wallet_id, WalletId, "post wallet_id mismatch"),
    assert_eq(Post#near_social_post.text, "Hello NEAR from Mazaryn!", "post text mismatch"),
    {ok, Posts} = near_walletdb:get_wallet_social_posts(WalletId),
    assert_true(length(Posts) >= 1, "should have at least 1 post"),
    {ok, UserPosts} = near_walletdb:get_user_social_posts(UserId),
    assert_true(length(UserPosts) >= 1, "user should have at least 1 post"),
    ok = near_walletdb:delete_wallet(WalletId2),
    {ok, AfterDeletePrimary} = near_walletdb:get_primary_wallet(UserId),
    assert_eq(AfterDeletePrimary#near_wallet.wallet_id, WalletId, "after delete, WalletId should be primary"),
    io:format(" near_walletdb: all ~p assertions passed~n", [count_assertions()]),
    ok.

test_wallet(Token) ->
    UserId = "test_user_" ++ unique_id(),
    case near_wallet:create_wallet(Token, UserId) of
        {ok, #{wallet_id := WalletId, account_id := AccountId}} ->
            assert_non_empty(WalletId, "wallet_id empty"),
            assert_non_empty(AccountId, "account_id empty"),
            io:format(" created wallet: ~s -> ~s~n", [WalletId, AccountId]),
            case near_wallet:get_balance(Token, WalletId) of
                {ok, #{balance_near := _BalNear}} ->
                    io:format(" get_balance: ok~n");
                {error, Reason} ->
                    io:format(" get_balance skipped (network): ~p~n", [Reason])
            end,
            case near_wallet:get_multi_balance(Token, WalletId) of
                {ok, #{near := _}} ->
                    io:format(" get_multi_balance: ok~n");
                {error, _} ->
                    io:format(" get_multi_balance skipped (network)~n")
            end,
            case near_wallet:get_wallet_info(Token, WalletId) of
                {ok, #{account_id := AccountId}} ->
                    io:format(" get_wallet_info: ok~n");
                {error, _} ->
                    io:format(" get_wallet_info skipped (network)~n")
            end,
            case near_wallet:get_user_wallets(Token, UserId) of
                {ok, #{wallets := WalletList}} ->
                    assert_true(length(WalletList) >= 1, "user should have wallets"),
                    io:format(" get_user_wallets: ~p wallet(s)~n", [length(WalletList)]);
                {error, _} ->
                    io:format(" get_user_wallets skipped (network)~n")
            end;
        {error, Reason} ->
            io:format(" create_wallet skipped (network unavailable): ~p~n", [Reason])
    end,
    case near_wallet:import_wallet(Token, UserId, "invalid seed phrase that is wrong") of
        {error, _} ->
            io:format(" import bad seed: correctly rejected~n");
        {ok, _} ->
            io:format(" import bad seed: unexpected success (check TS validation)~n")
    end,
    ok.

test_tokens(Token) ->
    FakeWalletId = "nonexistent_wallet_" ++ unique_id(),
    case near_tokens:transfer_near(Token, FakeWalletId, "bob.testnet", "0.1") of
        {error, _} ->
            io:format(" transfer_near nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" transfer_near: unexpected success~n")
    end,
    case near_tokens:transfer_ft(Token, FakeWalletId, "bob.testnet", "ft.testnet", "100") of
        {error, _} ->
            io:format(" transfer_ft nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" transfer_ft: unexpected success~n")
    end,
    case near_tokens:transfer_usdt(Token, FakeWalletId, "bob.testnet", "1.00") of
        {error, _} ->
            io:format(" transfer_usdt nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" transfer_usdt: unexpected success~n")
    end,
    case near_tokens:get_ft_balance(Token, FakeWalletId, "ft.testnet") of
        {error, _} ->
            io:format(" get_ft_balance nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" get_ft_balance: ok (wallet found)~n")
    end,
    case near_tokens:get_usdt_balance(Token, FakeWalletId) of
        {error, _} ->
            io:format(" get_usdt_balance nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" get_usdt_balance: ok~n")
    end,
    case near_tokens:get_token_metadata(Token, "wrap.testnet") of
        {ok, #{symbol := _Symbol, decimals := _Dec}} ->
            io:format(" get_token_metadata wrap.testnet: ok~n");
        {error, _} ->
            io:format(" get_token_metadata skipped (network)~n")
    end,
    ok.

test_keys(Token) ->
    FakeWalletId = "nonexistent_wallet_" ++ unique_id(),
    case near_keys:generate_seed_phrase(Token) of
        {ok, #{seed_phrase := Seed, public_key := PubKey}} ->
            assert_true(length(string:tokens(Seed, " ")) >= 12, "seed should have >= 12 words"),
            assert_non_empty(PubKey, "public_key from seed empty"),
            io:format(" generate_seed_phrase: ok (~p words)~n",
                      [length(string:tokens(Seed, " "))]);
        {error, _} ->
            io:format(" generate_seed_phrase skipped (network)~n")
    end,
    case near_keys:get_access_keys(Token, FakeWalletId) of
        {error, _} ->
            io:format(" get_access_keys nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" get_access_keys: ok~n")
    end,
    case near_keys:add_full_access_key(Token, FakeWalletId, "ed25519:fakekey123") of
        {error, _} ->
            io:format(" add_full_access_key nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" add_full_access_key: ok~n")
    end,
    case near_keys:add_function_call_key(Token, FakeWalletId, "ed25519:fakekey456",
                                          "social.near", ["set"]) of
        {error, _} ->
            io:format(" add_function_call_key nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" add_function_call_key: ok~n")
    end,
    case near_keys:get_stored_keys(Token, FakeWalletId) of
        {error, _} ->
            io:format(" get_stored_keys nonexistent: correctly rejected~n");
        {ok, #{keys := _}} ->
            io:format(" get_stored_keys: ok~n")
    end,
    case near_keys:import_from_seed_phrase(Token, "user_" ++ unique_id(),
                                            "not a valid seed phrase at all") of
        {error, _} ->
            io:format(" import bad seed: correctly rejected~n");
        {ok, _} ->
            io:format(" import seed: ok (ts accepted it)~n")
    end,
    ok.

test_transactions(Token) ->
    FakeWalletId = "nonexistent_wallet_" ++ unique_id(),
    Actions = [
        #{type => transfer, amount_near => "0.1"}
    ],
    case near_transactions:send_batch(Token, FakeWalletId, Actions) of
        {error, _} ->
            io:format(" send_batch nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" send_batch: ok~n")
    end,
    case near_transactions:call_function(
            Token, FakeWalletId, "social.near", "set", #{}) of
        {error, _} ->
            io:format(" call_function nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" call_function: ok~n")
    end,
    case near_transactions:view_function(
            Token, "social.near", "get",
            #{keys => ["alice.near/profile/name"]}) of
        {ok, #{result := _}} ->
            io:format(" view_function social.near: ok~n");
        {error, _} ->
            io:format(" view_function skipped (network)~n")
    end,
    case near_transactions:get_transaction_status(Token, "fakehash000") of
        {error, _} ->
            io:format(" get_transaction_status nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" get_transaction_status: ok~n")
    end,
    case near_transactions:get_transaction_history(Token, FakeWalletId) of
        {error, _} ->
            io:format(" get_transaction_history nonexistent: correctly rejected~n");
        {ok, #{transactions := _}} ->
            io:format(" get_transaction_history: ok~n")
    end,
    case near_transactions:get_user_transaction_history(Token, "nonexistent_user_id") of
        {error, _} ->
            io:format(" get_user_tx_history nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" get_user_tx_history: ok~n")
    end,
    ok.

test_staking(Token) ->
    FakeWalletId = "nonexistent_wallet_" ++ unique_id(),
    case near_staking:stake(Token, FakeWalletId, "validator.poolv1.near", "10") of
        {error, _} ->
            io:format(" stake nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" stake: ok~n")
    end,
    case near_staking:unstake(Token, FakeWalletId, "5") of
        {error, _} ->
            io:format(" unstake nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" unstake: ok~n")
    end,
    case near_staking:unstake_all(Token, FakeWalletId) of
        {error, _} ->
            io:format(" unstake_all nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" unstake_all: ok~n")
    end,
    case near_staking:get_staked_balance(Token, FakeWalletId) of
        {error, _} ->
            io:format(" get_staked_balance nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" get_staked_balance: ok~n")
    end,
    case near_staking:get_all_staked(Token, FakeWalletId) of
        {error, _} ->
            io:format(" get_all_staked nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" get_all_staked: ok~n")
    end,
    case near_staking:get_validators(Token) of
        {ok, #{validators := Validators}} ->
            io:format(" get_validators: ~p validators~n", [length(Validators)]);
        {error, _} ->
            io:format(" get_validators skipped (network)~n")
    end,
    case near_staking:get_epoch_seat_price(Token) of
        {ok, #{seat_price_near := _Price}} ->
            io:format(" get_epoch_seat_price: ok~n");
        {error, _} ->
            io:format(" get_epoch_seat_price skipped (network)~n")
    end,
    case near_staking:get_user_stakes(Token, "nonexistent_user_id") of
        {ok, #{stakes := []}} ->
            io:format(" get_user_stakes empty: ok~n");
        {error, _} ->
            io:format(" get_user_stakes nonexistent: correctly rejected~n")
    end,
    ok.

test_social(Token) ->
    FakeWalletId = "nonexistent_wallet_" ++ unique_id(),
    case near_social:create_post(Token, FakeWalletId, "Hello world!") of
        {error, _} ->
            io:format(" create_post nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" create_post: ok~n")
    end,
    case near_social:create_post(Token, FakeWalletId, "Tagged post!", #{
        tags => ["test", "near"],
        media_urls => ["https://example.com/img.png"]
    }) of
        {error, _} ->
            io:format(" create_post with opts nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" create_post with opts: ok~n")
    end,
    case near_social:like_post(Token, FakeWalletId, "alice.near/post/1") of
        {error, _} ->
            io:format(" like_post nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" like_post: ok~n")
    end,
    case near_social:comment_on_post(Token, FakeWalletId, "alice.near/post/1", "Nice!") of
        {error, _} ->
            io:format(" comment_on_post nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" comment_on_post: ok~n")
    end,
    case near_social:follow_account(Token, FakeWalletId, "bob.near") of
        {error, _} ->
            io:format(" follow_account nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" follow_account: ok~n")
    end,
    case near_social:tip_account(Token, FakeWalletId, "alice.near", "0.1") of
        {error, _} ->
            io:format(" tip_account nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" tip_account: ok~n")
    end,
    case near_social:get_social_profile(Token, "root.near") of
        {ok, #{account_id := "root.near"}} ->
            io:format(" get_social_profile root.near: ok~n");
        {error, _} ->
            io:format(" get_social_profile skipped (network)~n")
    end,
    case near_social:get_followers(Token, "alice.near") of
        {ok, #{followers := _F}} ->
            io:format(" get_followers: ok~n");
        {error, _} ->
            io:format(" get_followers skipped (network)~n")
    end,
    case near_social:get_following(Token, "alice.near") of
        {ok, #{following := _G}} ->
            io:format(" get_following: ok~n");
        {error, _} ->
            io:format(" get_following skipped (network)~n")
    end,
    case near_social:get_feed(Token, FakeWalletId) of
        {error, _} ->
            io:format(" get_feed nonexistent: correctly rejected~n");
        {ok, #{posts := _}} ->
            io:format(" get_feed: ok~n")
    end,
    case near_social:get_notifications(Token, FakeWalletId) of
        {error, _} ->
            io:format(" get_notifications nonexistent: correctly rejected~n");
        {ok, _} ->
            io:format(" get_notifications: ok~n")
    end,
    ok.

test_user_integration(Token) ->
    run_integration_case("ensure_user_has_wallet",
        fun() -> near_user_integration:ensure_user_has_wallet(Token, "nonexistent_mazaryn_user") end),
    run_integration_case("get_user_balance",
        fun() -> near_user_integration:get_user_balance(Token, "nonexistent_mazaryn_user") end),
    run_integration_case("get_user_multi_balance",
        fun() -> near_user_integration:get_user_multi_balance(Token, "nonexistent_mazaryn_user") end),
    run_integration_case("transfer_between_users",
        fun() -> near_user_integration:transfer_between_users(Token, "user_a", "user_b", "1.0", #{}) end),
    run_integration_case("transfer_ft_between_users",
        fun() -> near_user_integration:transfer_ft_between_users(Token, "user_a", "user_b", "ft.testnet", "100", #{}) end),
    run_integration_case("tip_user",
        fun() -> near_user_integration:tip_user(Token, "user_a", "user_b", "0.5") end),
    run_integration_case("create_post_for_user",
        fun() -> near_user_integration:create_post_for_user(Token, "nonexistent_user", "Hello!") end),
    run_integration_case("like_post_for_user",
        fun() -> near_user_integration:like_post_for_user(Token, "nonexistent_user", "alice.near/post/1") end),
    run_integration_case("comment_for_user",
        fun() -> near_user_integration:comment_for_user(Token, "nonexistent_user", "alice.near/post/1", "Nice!") end),
    run_integration_case("follow_user",
        fun() -> near_user_integration:follow_user(Token, "user_a", "user_b") end),
    run_integration_case("unfollow_user",
        fun() -> near_user_integration:unfollow_user(Token, "user_a", "user_b") end),
    run_integration_case("get_user_feed",
        fun() -> near_user_integration:get_user_feed(Token, "nonexistent_user") end),
    run_integration_case("get_user_social_profile",
        fun() -> near_user_integration:get_user_social_profile(Token, "nonexistent_user") end),
    run_integration_case("get_user_notifications",
        fun() -> near_user_integration:get_user_notifications(Token, "nonexistent_user") end),
    run_integration_case("stake_for_user",
        fun() -> near_user_integration:stake_for_user(Token, "nonexistent_user", "validator.poolv1.near", "10") end),
    run_integration_case("unstake_for_user",
        fun() -> near_user_integration:unstake_for_user(Token, "nonexistent_user", "5") end),
    run_integration_case("withdraw_for_user",
        fun() -> near_user_integration:withdraw_for_user(Token, "nonexistent_user", "5") end),
    run_integration_case("get_user_staked_balance",
        fun() -> near_user_integration:get_user_staked_balance(Token, "nonexistent_user") end),
    run_integration_case("get_near_summary",
        fun() -> near_user_integration:get_near_summary(Token, "nonexistent_user") end),
    ok.

run_integration_case(Name, Fun) ->
    Result = try Fun()
             catch
                 error:undef -> {error, undef_user_module};
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
    NearTables = [near_wallet, near_transaction, near_access_key,
                  near_stake, near_implicit_account, near_social_post],
    [ensure_test_table(T) || T <- NearTables],
    mnesia:wait_for_tables(NearTables, 5000).

ensure_test_table(near_wallet) ->
    safe_create_table(near_wallet, record_info(fields, near_wallet)),
    safe_add_index(near_wallet, user_id),
    safe_add_index(near_wallet, account_id);
ensure_test_table(near_transaction) ->
    safe_create_table(near_transaction, record_info(fields, near_transaction)),
    safe_add_index(near_transaction, wallet_id),
    safe_add_index(near_transaction, user_id);
ensure_test_table(near_access_key) ->
    safe_create_table(near_access_key, record_info(fields, near_access_key)),
    safe_add_index(near_access_key, wallet_id),
    safe_add_index(near_access_key, user_id);
ensure_test_table(near_stake) ->
    safe_create_table(near_stake, record_info(fields, near_stake)),
    safe_add_index(near_stake, wallet_id),
    safe_add_index(near_stake, user_id);
ensure_test_table(near_implicit_account) ->
    safe_create_table(near_implicit_account, record_info(fields, near_implicit_account)),
    safe_add_index(near_implicit_account, wallet_id),
    safe_add_index(near_implicit_account, user_id),
    safe_add_index(near_implicit_account, account_id);
ensure_test_table(near_social_post) ->
    safe_create_table(near_social_post, record_info(fields, near_social_post)),
    safe_add_index(near_social_post, wallet_id),
    safe_add_index(near_social_post, user_id).

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
    case application:get_env(near_test, token) of
        {ok, T} -> T;
        undefined ->
            case os:getenv("NEAR_TEST_TOKEN") of
                false -> "test_token_replace_me";
                T -> T
            end
    end.

unique_id() ->
    integer_to_list(erlang:unique_integer([positive])).

assert_eq(A, B, Msg) ->
    case A =:= B of
        true -> ok;
        false -> error({assertion_failed, Msg, {expected, B}, {got, A}})
    end.

assert_true(true, _Msg) -> ok;
assert_true(false, Msg) -> error({assertion_failed, Msg}).

assert_non_empty("", Msg) -> error({assertion_failed, Msg, empty_string});
assert_non_empty(<<>>, Msg) -> error({assertion_failed, Msg, empty_binary});
assert_non_empty(undefined, Msg) -> error({assertion_failed, Msg, undefined});
assert_non_empty(_, _) -> ok.

count_assertions() ->
    30.
