-module(solana_test).
-author("Zaryn Technologies").
-include("../records.hrl").
-include("../wallet_records.hrl").
-export([
    run_all_tests/0,
    test_user_wallet_creation/0,
    test_wallet_operations/0,
    test_transaction_recording/0,
    test_airdrop_system/0,
    test_stake_operations/0,
    test_token_and_nft/0,
    test_user_wallet_queries/0,
    cleanup_test_data/0
]).

run_all_tests() ->
    io:format("~n=== Starting Solana-Mazaryn Integration Tests ===~n~n"),

    Tests = [
        {user_wallet_creation, fun test_user_wallet_creation/0},
        {wallet_operations, fun test_wallet_operations/0},
        {transaction_recording, fun test_transaction_recording/0},
        {airdrop_system, fun test_airdrop_system/0},
        {stake_operations, fun test_stake_operations/0},
        {token_and_nft, fun test_token_and_nft/0},
        {user_wallet_queries, fun test_user_wallet_queries/0}
    ],

    Results = lists:map(fun({Name, TestFun}) ->
        io:format("Running test: ~p~n", [Name]),
        try
            Result = TestFun(),
            io:format("  ✓ ~p: PASSED~n", [Name]),
            {Name, passed, Result}
        catch
            Error:Reason:Stacktrace ->
                io:format("  ✗ ~p: FAILED~n    ~p:~p~n", [Name, Error, Reason]),
                io:format("    Stacktrace: ~p~n", [Stacktrace]),
                {Name, failed, {Error, Reason}}
        end
    end, Tests),

    Passed = length([Result || Result = {_, passed, _} <- Results]),
    Failed = length([Result || Result = {_, failed, _} <- Results]),

    io:format("~n=== Test Summary ===~n"),
    io:format("Passed: ~p~n", [Passed]),
    io:format("Failed: ~p~n", [Failed]),
    io:format("Total:  ~p~n~n", [length(Tests)]),

    cleanup_test_data(),

    {Passed, Failed, Results}.

test_user_wallet_creation() ->
    Username = "alice_" ++ integer_to_list(erlang:system_time(millisecond)),
    Email = Username ++ "@mazaryn.com",
    Password = "SecurePass123!",

    UserId = userdb:insert_concurrent(Username, Password, Email),
    true = is_list(UserId),

    User = userdb:get_user_by_id(UserId),
    true = is_record(User, user),
    true = User#user.username =:= Username,
    true = User#user.email =:= Email,

    PublicKey = "TestPubKey_" ++ UserId,
    EncPrivKey = base64:encode(crypto:strong_rand_bytes(32)),
    IV = base64:encode(crypto:strong_rand_bytes(16)),
    AuthTag = base64:encode(crypto:strong_rand_bytes(16)),

    {ok, WalletId} = solana_walletdb:create_wallet(
        UserId,
        PublicKey,
        EncPrivKey,
        IV,
        AuthTag,
        "Alice's Main Wallet"
    ),

    {ok, Wallet} = solana_walletdb:get_wallet(WalletId),
    true = Wallet#solana_wallet.user_id =:= UserId,
    true = Wallet#solana_wallet.public_key =:= PublicKey,
    true = Wallet#solana_wallet.is_primary =:= true,
    true = Wallet#solana_wallet.label =:= "Alice's Main Wallet",

    {ok, UserWallets} = solana_walletdb:get_user_wallets(UserId),
    true = length(UserWallets) =:= 1,
    [FirstWallet] = UserWallets,
    true = FirstWallet#solana_wallet.wallet_id =:= WalletId,

    {ok, test_passed, #{user_id => UserId, wallet_id => WalletId, username => Username}}.

test_wallet_operations() ->
    Username = "bob_" ++ integer_to_list(erlang:system_time(millisecond)),
    Email = Username ++ "@mazaryn.com",
    Password = "SecurePass123!",

    UserId = userdb:insert_concurrent(Username, Password, Email),

    PublicKey1 = "BobPubKey1_" ++ UserId,
    PublicKey2 = "BobPubKey2_" ++ UserId,

    EncPrivKey = base64:encode(crypto:strong_rand_bytes(32)),
    IV = base64:encode(crypto:strong_rand_bytes(16)),
    AuthTag = base64:encode(crypto:strong_rand_bytes(16)),

    {ok, WalletId1} = solana_walletdb:create_wallet(UserId, PublicKey1, EncPrivKey, IV, AuthTag, "Bob Wallet 1"),
    {ok, WalletId2} = solana_walletdb:create_wallet(UserId, PublicKey2, EncPrivKey, IV, AuthTag, "Bob Wallet 2"),

    {ok, Wallets} = solana_walletdb:get_user_wallets(UserId),
    true = length(Wallets) =:= 2,

    [PrimaryWallet | _] = Wallets,
    true = PrimaryWallet#solana_wallet.is_primary =:= true,

    ok = solana_walletdb:update_wallet_label(WalletId1, "Bob's Trading Wallet"),
    {ok, UpdatedWallet} = solana_walletdb:get_wallet(WalletId1),
    true = UpdatedWallet#solana_wallet.label =:= "Bob's Trading Wallet",

    ok = solana_walletdb:set_primary_wallet(UserId, WalletId2),
    {ok, NewPrimary} = solana_walletdb:get_primary_wallet(UserId),
    true = NewPrimary#solana_wallet.wallet_id =:= WalletId2,

    ok = solana_walletdb:update_wallet_last_used(WalletId1),

    {ok, test_passed, #{user_id => UserId, wallets => length(Wallets)}}.

test_transaction_recording() ->
    Username = "charlie_" ++ integer_to_list(erlang:system_time(millisecond)),
    Email = Username ++ "@mazaryn.com",
    Password = "SecurePass123!",

    UserId = userdb:insert_concurrent(Username, Password, Email),

    PublicKey = "CharliePubKey_" ++ UserId,
    EncPrivKey = base64:encode(crypto:strong_rand_bytes(32)),
    IV = base64:encode(crypto:strong_rand_bytes(16)),
    AuthTag = base64:encode(crypto:strong_rand_bytes(16)),

    {ok, WalletId} = solana_walletdb:create_wallet(UserId, PublicKey, EncPrivKey, IV, AuthTag, "Charlie's Wallet"),

    TxData1 = #{
        signature => "TxSig1_" ++ nanoid:gen(),
        tx_type => sol_transfer,
        from_address => PublicKey,
        to_address => "RecipientKey_" ++ nanoid:gen(),
        amount_lamports => 1500000000,
        status => confirmed,
        fee_lamports => 5000,
        memo => "Payment for services"
    },

    {ok, TxId1} = solana_walletdb:create_transaction(WalletId, TxData1),

    TxData2 = #{
        signature => "TxSig2_" ++ nanoid:gen(),
        tx_type => token_transfer,
        from_address => PublicKey,
        to_address => "RecipientKey2_" ++ nanoid:gen(),
        token_mint => "TokenMint_USDC",
        amount_lamports => 100,
        status => pending,
        fee_lamports => 5000
    },

    {ok, TxId2} = solana_walletdb:create_transaction(WalletId, TxData2),

    {ok, Tx1} = solana_walletdb:get_transaction(TxId1),
    true = Tx1#solana_transaction.wallet_id =:= WalletId,
    true = Tx1#solana_transaction.user_id =:= UserId,
    true = Tx1#solana_transaction.amount_lamports =:= 1500000000,
    true = Tx1#solana_transaction.status =:= confirmed,

    {ok, AllTxs} = solana_walletdb:get_wallet_transactions(WalletId),
    true = length(AllTxs) =:= 2,

    {ok, SolTxs} = solana_walletdb:get_transactions_by_type(WalletId, sol_transfer),
    true = length(SolTxs) =:= 1,

    {ok, UserTxs} = solana_walletdb:get_user_transactions(UserId),
    true = length(UserTxs) =:= 2,

    ok = solana_walletdb:update_transaction_status(TxId2, confirmed, calendar:universal_time()),
    {ok, UpdatedTx} = solana_walletdb:get_transaction(TxId2),
    true = UpdatedTx#solana_transaction.status =:= confirmed,

    {ok, test_passed, #{user_id => UserId, wallet_id => WalletId, transactions => 2}}.

test_airdrop_system() ->
    Username = "diana_" ++ integer_to_list(erlang:system_time(millisecond)),
    Email = Username ++ "@mazaryn.com",
    Password = "SecurePass123!",

    UserId = userdb:insert_concurrent(Username, Password, Email),

    PublicKey = "DianaPubKey_" ++ UserId,
    EncPrivKey = base64:encode(crypto:strong_rand_bytes(32)),
    IV = base64:encode(crypto:strong_rand_bytes(16)),
    AuthTag = base64:encode(crypto:strong_rand_bytes(16)),

    {ok, WalletId} = solana_walletdb:create_wallet(UserId, PublicKey, EncPrivKey, IV, AuthTag, "Diana's Wallet"),

    AirdropData = #{
        type => sol,
        total_recipients => 50,
        total_amount_lamports => 50000000,
        status => processing
    },

    {ok, AirdropId} = solana_walletdb:create_airdrop(WalletId, AirdropData),

    Recipient1Data = #{
        recipient_address => "Recipient1_" ++ nanoid:gen(),
        amount_lamports => 1000000,
        success => true,
        signature => "AirdropSig1_" ++ nanoid:gen(),
        processed_at => calendar:universal_time()
    },

    {ok, _RecipientId1} = solana_walletdb:create_airdrop_recipient(AirdropId, Recipient1Data),

    Recipient2Data = #{
        recipient_address => "Recipient2_" ++ nanoid:gen(),
        amount_lamports => 1000000,
        success => false,
        error_message => "Insufficient balance",
        processed_at => calendar:universal_time()
    },

    {ok, _RecipientId2} = solana_walletdb:create_airdrop_recipient(AirdropId, Recipient2Data),

    ok = solana_walletdb:update_airdrop(AirdropId, #{
        successful => 1,
        failed => 1,
        status => processing
    }),

    {ok, Airdrop} = solana_walletdb:get_airdrop(AirdropId),
    true = Airdrop#solana_airdrop.successful =:= 1,
    true = Airdrop#solana_airdrop.failed =:= 1,

    {ok, Recipients} = solana_walletdb:get_airdrop_recipients(AirdropId),
    true = length(Recipients) =:= 2,

    {ok, UserAirdrops} = solana_walletdb:get_user_airdrops(UserId),
    true = length(UserAirdrops) =:= 1,

    {ok, test_passed, #{user_id => UserId, airdrop_id => AirdropId, recipients => 2}}.

test_stake_operations() ->
    Username = "eve_" ++ integer_to_list(erlang:system_time(millisecond)),
    Email = Username ++ "@mazaryn.com",
    Password = "SecurePass123!",

    UserId = userdb:insert_concurrent(Username, Password, Email),

    PublicKey = "EvePubKey_" ++ UserId,
    EncPrivKey = base64:encode(crypto:strong_rand_bytes(32)),
    IV = base64:encode(crypto:strong_rand_bytes(16)),
    AuthTag = base64:encode(crypto:strong_rand_bytes(16)),

    {ok, WalletId} = solana_walletdb:create_wallet(UserId, PublicKey, EncPrivKey, IV, AuthTag, "Eve's Staking Wallet"),

    StakeData = #{
        stake_account_address => "StakeAccount_" ++ nanoid:gen(),
        validator_vote_address => "ValidatorVote_" ++ nanoid:gen(),
        amount_lamports => 5000000000,
        status => delegated,
        signature => "StakeSig_" ++ nanoid:gen(),
        delegated_at => calendar:universal_time()
    },

    {ok, StakeId} = solana_walletdb:create_stake_account(WalletId, StakeData),

    {ok, StakeAccount} = solana_walletdb:get_stake_account(StakeId),
    true = StakeAccount#solana_stake_account.user_id =:= UserId,
    true = StakeAccount#solana_stake_account.wallet_id =:= WalletId,
    true = StakeAccount#solana_stake_account.status =:= delegated,
    true = StakeAccount#solana_stake_account.amount_lamports =:= 5000000000,

    StakeAddress = StakeAccount#solana_stake_account.stake_account_address,
    {ok, StakeByAddress} = solana_walletdb:get_stake_by_address(StakeAddress),
    true = StakeByAddress#solana_stake_account.stake_id =:= StakeId,

    ok = solana_walletdb:update_stake_account(StakeId, #{
        status => deactivated,
        deactivated_at => calendar:universal_time()
    }),

    {ok, UpdatedStake} = solana_walletdb:get_stake_account(StakeId),
    true = UpdatedStake#solana_stake_account.status =:= deactivated,
    true = UpdatedStake#solana_stake_account.deactivated_at =/= undefined,

    {ok, UserStakes} = solana_walletdb:get_user_stakes(UserId),
    true = length(UserStakes) =:= 1,

    {ok, WalletStakes} = solana_walletdb:get_wallet_stakes(WalletId),
    true = length(WalletStakes) =:= 1,

    {ok, test_passed, #{user_id => UserId, wallet_id => WalletId, stake_id => StakeId}}.

test_token_and_nft() ->
    Username = "frank_" ++ integer_to_list(erlang:system_time(millisecond)),
    Email = Username ++ "@mazaryn.com",
    Password = "SecurePass123!",

    UserId = userdb:insert_concurrent(Username, Password, Email),

    PublicKey = "FrankPubKey_" ++ UserId,
    EncPrivKey = base64:encode(crypto:strong_rand_bytes(32)),
    IV = base64:encode(crypto:strong_rand_bytes(16)),
    AuthTag = base64:encode(crypto:strong_rand_bytes(16)),

    {ok, WalletId} = solana_walletdb:create_wallet(UserId, PublicKey, EncPrivKey, IV, AuthTag, "Frank's NFT Wallet"),

    TokenData = #{
        token_account_address => "TokenAccount_" ++ nanoid:gen(),
        token_mint => "USDC_Mint",
        balance => <<"5000">>,
        decimals => 6,
        owner_address => PublicKey
    },

    {ok, TokenAccountId} = solana_walletdb:create_token_account(WalletId, TokenData),

    {ok, TokenAccount} = solana_walletdb:get_token_account(TokenAccountId),
    true = TokenAccount#solana_token_account.user_id =:= UserId,
    true = TokenAccount#solana_token_account.balance =:= <<"5000">>,
    true = TokenAccount#solana_token_account.token_mint =:= "USDC_Mint",

    ok = solana_walletdb:update_token_account_balance(TokenAccountId, <<"7500">>, calendar:universal_time()),
    {ok, UpdatedToken} = solana_walletdb:get_token_account(TokenAccountId),
    true = UpdatedToken#solana_token_account.balance =:= <<"7500">>,

    NFTData = #{
        mint_address => "NFTMint_" ++ nanoid:gen(),
        token_account_address => "NFTTokenAccount_" ++ nanoid:gen(),
        name => "Frank's Cool NFT",
        symbol => "FNFT",
        uri => "https://nft.mazaryn.com/frank/1.json",
        verified => true,
        creators => [],
        attributes => [
            #{trait_type => "Rarity", value => "Legendary"},
            #{trait_type => "Power", value => 100}
        ]
    },

    {ok, NFTId} = solana_walletdb:create_nft(WalletId, NFTData),

    {ok, NFT} = solana_walletdb:get_nft(NFTId),
    true = NFT#solana_nft.user_id =:= UserId,
    true = NFT#solana_nft.name =:= "Frank's Cool NFT",
    true = NFT#solana_nft.verified =:= true,

    ok = solana_walletdb:update_nft(NFTId, #{
        image_url => "https://nft.mazaryn.com/frank/1.png"
    }),

    {ok, UpdatedNFT} = solana_walletdb:get_nft(NFTId),
    true = UpdatedNFT#solana_nft.image_url =:= "https://nft.mazaryn.com/frank/1.png",

    {ok, WalletTokens} = solana_walletdb:get_wallet_token_accounts(WalletId),
    true = length(WalletTokens) =:= 1,

    {ok, WalletNFTs} = solana_walletdb:get_wallet_nfts(WalletId),
    true = length(WalletNFTs) =:= 1,

    {ok, test_passed, #{user_id => UserId, wallet_id => WalletId, tokens => 1, nfts => 1}}.

test_user_wallet_queries() ->
    Username = "grace_" ++ integer_to_list(erlang:system_time(millisecond)),
    Email = Username ++ "@mazaryn.com",
    Password = "SecurePass123!",

    UserId = userdb:insert_concurrent(Username, Password, Email),
    User = userdb:get_user_by_id(UserId),
    true = User#user.username =:= Username,

    {ok, WalletId1, _PublicKey1, needs_password_setup} = solana_user_integration:create_wallet_for_user(Username, "Grace Main"),
    {ok, WalletId2, _PublicKey2, needs_password_setup} = solana_user_integration:create_wallet_for_user(Username, "Grace Trading"),

    {ok, UserWallets} = solana_user_integration:get_user_solana_wallets(Username),
    true = length(UserWallets) =:= 2,

    {ok, PrimaryWallet} = solana_user_integration:get_user_primary_wallet(Username),
    true = PrimaryWallet#solana_wallet.is_primary =:= true,

    true = solana_user_integration:verify_wallet_ownership(WalletId1, Username),
    true = solana_user_integration:verify_wallet_ownership(WalletId2, Username),

    {ok, Owner} = solana_user_integration:get_wallet_owner(WalletId1),
    true = Owner#user.id =:= UserId,
    true = Owner#user.username =:= Username,

    {ok, already_linked} = solana_user_integration:link_wallet_to_user(WalletId1, Username),

    {ok, _Count, WalletBalances} = solana_user_integration:get_user_solana_balance(Username),
    true = length(WalletBalances) =:= 2,

    {ok, test_passed, #{user_id => UserId, username => Username, wallets => 2}}.

cleanup_test_data() ->
    io:format("~nCleaning up test data...~n"),

    Fun = fun() ->
        AllUsers = mnesia:all_keys(user),
        TestUsers = lists:filter(fun(UserId) ->
            case mnesia:read(user, UserId) of
                [User] ->
                    Username = User#user.username,
                    string:str(Username, "alice_") =:= 1 orelse
                    string:str(Username, "bob_") =:= 1 orelse
                    string:str(Username, "charlie_") =:= 1 orelse
                    string:str(Username, "diana_") =:= 1 orelse
                    string:str(Username, "eve_") =:= 1 orelse
                    string:str(Username, "frank_") =:= 1 orelse
                    string:str(Username, "grace_") =:= 1;
                [] -> false
            end
        end, AllUsers),

        lists:foreach(fun(UserId) ->
            Wallets = mnesia:index_read(solana_wallet, UserId, #solana_wallet.user_id),
            lists:foreach(fun(Wallet) ->
                mnesia:delete({solana_wallet, Wallet#solana_wallet.wallet_id})
            end, Wallets),

            mnesia:delete({user, UserId})
        end, TestUsers),

        ok
    end,

    case mnesia:transaction(Fun) of
        {atomic, ok} -> io:format("Cleanup successful~n");
        {aborted, Reason} -> io:format("Cleanup failed: ~p~n", [Reason])
    end.
