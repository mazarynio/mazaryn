-module(solana_walletdb).
-author("Zaryn Technologies").
-include("../records.hrl").
-include("../wallet_records.hrl").
-export([
    create_wallet/5,
    create_wallet/6,
    create_wallet/3,
    import_wallet/6,
    import_wallet/7,
    get_wallet/1,
    get_wallet_by_public_key/1,
    get_user_wallets/1,
    update_wallet_last_used/1,
    update_wallet_label/2,
    set_primary_wallet/2,
    get_primary_wallet/1,
    delete_wallet/1,
    wallet_exists/1,
    public_key_exists/1,
    export_private_key/2,

    create_transaction/2,
    get_transaction/1,
    get_wallet_transactions/1,
    get_wallet_transactions/3,
    get_user_transactions/1,
    get_transactions_by_type/2,
    update_transaction_status/3,

    create_airdrop/2,
    get_airdrop/1,
    update_airdrop/2,
    get_user_airdrops/1,
    get_wallet_airdrops/1,
    create_airdrop_recipient/2,
    get_airdrop_recipients/1,
    update_airdrop_recipient/2,

    create_stake_account/2,
    get_stake_account/1,
    get_stake_by_address/1,
    update_stake_account/2,
    get_user_stakes/1,
    get_wallet_stakes/1,

    create_token_account/2,
    get_token_account/1,
    get_wallet_token_accounts/1,
    update_token_account_balance/3,

    create_nft/2,
    get_nft/1,
    get_wallet_nfts/1,
    update_nft/2
]).

-define(MAX_RETRIES, 10).
-define(BACKOFF_TIME, 20).

create_wallet(UserId, Label, UserPassword) ->
    #{
        public_key := PublicKey,
        private_key := PrivateKey,
        seed := Seed
    } = solana_crypto:generate_keypair(),

    EncryptedData = solana_crypto:encrypt_with_user_password(
        list_to_binary(PrivateKey),
        UserPassword
    ),

    Fun = fun() ->
        case check_public_key_exists(PublicKey) of
            true ->
                {error, public_key_already_exists};
            false ->
                WalletId = nanoid:gen(),
                Now = calendar:universal_time(),

                IsPrimary = case get_user_wallets_internal(UserId) of
                    [] -> true;
                    _ -> false
                end,

                Wallet = #solana_wallet{
                    wallet_id = WalletId,
                    user_id = UserId,
                    label = Label,
                    public_key = PublicKey,
                    encrypted_private_key = maps:get(encrypted_private_key, EncryptedData),
                    encryption_iv = maps:get(encryption_iv, EncryptedData),
                    encryption_auth_tag = maps:get(encryption_auth_tag, EncryptedData),
                    encryption_salt = maps:get(encryption_salt, EncryptedData),
                    created_at = Now,
                    last_used = Now,
                    is_primary = IsPrimary,
                    metadata = #{seed => Seed}
                },
                mnesia:write(Wallet),
                {ok, WalletId, PublicKey}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

create_wallet(UserId, PublicKey, EncryptedPrivateKey, IV, AuthTag) ->
    create_wallet(UserId, PublicKey, EncryptedPrivateKey, IV, AuthTag, undefined).

create_wallet(UserId, PublicKey, EncryptedPrivateKey, IV, AuthTag, Label) ->
    Fun = fun() ->
        case check_public_key_exists(PublicKey) of
            true ->
                {error, public_key_already_exists};
            false ->
                WalletId = nanoid:gen(),
                Now = calendar:universal_time(),
                Salt = base64:encode(crypto:strong_rand_bytes(32)),

                IsPrimary = case get_user_wallets_internal(UserId) of
                    [] -> true;
                    _ -> false
                end,

                Wallet = #solana_wallet{
                    wallet_id = WalletId,
                    user_id = UserId,
                    label = Label,
                    public_key = PublicKey,
                    encrypted_private_key = EncryptedPrivateKey,
                    encryption_iv = IV,
                    encryption_auth_tag = AuthTag,
                    encryption_salt = Salt,
                    created_at = Now,
                    last_used = Now,
                    is_primary = IsPrimary
                },
                mnesia:write(Wallet),
                {ok, WalletId}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

export_private_key(WalletId, UserPassword) ->
    Fun = fun() ->
        case mnesia:read(solana_wallet, WalletId) of
            [] -> {error, wallet_not_found};
            [Wallet] ->
                EncryptedData = #{
                    encrypted_private_key => Wallet#solana_wallet.encrypted_private_key,
                    encryption_iv => Wallet#solana_wallet.encryption_iv,
                    encryption_auth_tag => Wallet#solana_wallet.encryption_auth_tag
                },
                Salt = base64:decode(Wallet#solana_wallet.encryption_salt),

                case solana_crypto:decrypt_with_user_password(EncryptedData, UserPassword, Salt) of
                    {ok, PrivateKey} -> {ok, binary_to_list(PrivateKey)};
                    {error, _} = Error -> Error
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

import_wallet(UserId, PublicKey, EncryptedPrivateKey, IV, AuthTag, DerivationPath) ->
    import_wallet(UserId, PublicKey, EncryptedPrivateKey, IV, AuthTag, DerivationPath, undefined).

import_wallet(UserId, PublicKey, EncryptedPrivateKey, IV, AuthTag, DerivationPath, Label) ->
    Fun = fun() ->
        case check_public_key_exists(PublicKey) of
            true ->
                {error, public_key_already_exists};
            false ->
                WalletId = nanoid:gen(),
                Now = calendar:universal_time(),
                Salt = base64:encode(crypto:strong_rand_bytes(32)),

                IsPrimary = case get_user_wallets_internal(UserId) of
                    [] -> true;
                    _ -> false
                end,

                Wallet = #solana_wallet{
                    wallet_id = WalletId,
                    user_id = UserId,
                    label = Label,
                    public_key = PublicKey,
                    encrypted_private_key = EncryptedPrivateKey,
                    encryption_iv = IV,
                    encryption_auth_tag = AuthTag,
                    encryption_salt = Salt,
                    derivation_path = DerivationPath,
                    created_at = Now,
                    last_used = Now,
                    is_primary = IsPrimary
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
        case mnesia:read(solana_wallet, WalletId) of
            [] -> {error, wallet_not_found};
            [Wallet] -> {ok, Wallet}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet_by_public_key(PublicKey) ->
    Fun = fun() ->
        case mnesia:index_read(solana_wallet, PublicKey, #solana_wallet.public_key) of
            [] -> {error, wallet_not_found};
            [Wallet] -> {ok, Wallet};
            [Wallet | _] -> {ok, Wallet}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_user_wallets(UserId) ->
    Fun = fun() ->
        Wallets = mnesia:index_read(solana_wallet, UserId, #solana_wallet.user_id),
        lists:sort(fun(A, B) ->
            if
                A#solana_wallet.is_primary -> true;
                B#solana_wallet.is_primary -> false;
                true -> A#solana_wallet.created_at >= B#solana_wallet.created_at
            end
        end, Wallets)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Wallets} -> {ok, Wallets};
        {aborted, Reason} -> {error, Reason}
    end.

get_user_wallets_internal(UserId) ->
    mnesia:index_read(solana_wallet, UserId, #solana_wallet.user_id).

update_wallet_last_used(WalletId) ->
    Fun = fun() ->
        case mnesia:read(solana_wallet, WalletId) of
            [] -> {error, wallet_not_found};
            [Wallet] ->
                UpdatedWallet = Wallet#solana_wallet{last_used = calendar:universal_time()},
                mnesia:write(UpdatedWallet),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

update_wallet_label(WalletId, NewLabel) ->
    Fun = fun() ->
        case mnesia:read(solana_wallet, WalletId) of
            [] -> {error, wallet_not_found};
            [Wallet] ->
                UpdatedWallet = Wallet#solana_wallet{label = NewLabel},
                mnesia:write(UpdatedWallet),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

set_primary_wallet(UserId, WalletId) ->
    Fun = fun() ->
        case mnesia:read(solana_wallet, WalletId) of
            [] -> {error, wallet_not_found};
            [Wallet] ->
                if
                    Wallet#solana_wallet.user_id =/= UserId ->
                        {error, access_denied};
                    true ->
                        UserWallets = get_user_wallets_internal(UserId),
                        lists:foreach(fun(W) ->
                            if
                                W#solana_wallet.wallet_id =:= WalletId ->
                                    mnesia:write(W#solana_wallet{is_primary = true});
                                W#solana_wallet.is_primary =:= true ->
                                    mnesia:write(W#solana_wallet{is_primary = false});
                                true ->
                                    ok
                            end
                        end, UserWallets),
                        ok
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_primary_wallet(UserId) ->
    Fun = fun() ->
        case mnesia:index_read(solana_wallet, UserId, #solana_wallet.user_id) of
            [] -> {error, no_wallets_found};
            Wallets ->
                case lists:filter(fun(W) -> W#solana_wallet.is_primary end, Wallets) of
                    [] ->
                        [FirstWallet | _] = lists:sort(
                            fun(A, B) -> A#solana_wallet.created_at =< B#solana_wallet.created_at end,
                            Wallets
                        ),
                        {ok, FirstWallet};
                    [PrimaryWallet] -> {ok, PrimaryWallet};
                    [PrimaryWallet | _] -> {ok, PrimaryWallet}
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

delete_wallet(WalletId) ->
    Fun = fun() ->
        case mnesia:read(solana_wallet, WalletId) of
            [] -> {error, wallet_not_found};
            [Wallet] ->
                mnesia:delete({solana_wallet, WalletId}),

                UserWallets = get_user_wallets_internal(Wallet#solana_wallet.user_id),
                if
                    Wallet#solana_wallet.is_primary andalso length(UserWallets) > 1 ->
                        RemainingWallets = lists:filter(
                            fun(W) -> W#solana_wallet.wallet_id =/= WalletId end,
                            UserWallets
                        ),
                        case RemainingWallets of
                            [] -> ok;
                            [FirstRemaining | _] ->
                                mnesia:write(FirstRemaining#solana_wallet{is_primary = true})
                        end;
                    true -> ok
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
        case mnesia:read(solana_wallet, WalletId) of
            [] -> false;
            [_] -> true
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, _} -> false
    end.

public_key_exists(PublicKey) ->
    Fun = fun() ->
        check_public_key_exists(PublicKey)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, _} -> false
    end.

check_public_key_exists(PublicKey) ->
    case mnesia:index_read(solana_wallet, PublicKey, #solana_wallet.public_key) of
        [] -> false;
        [_|_] -> true
    end.

create_transaction(WalletId, TxData) ->
    Fun = fun() ->
        case mnesia:read(solana_wallet, WalletId) of
            [] -> {error, wallet_not_found};
            [Wallet] ->
                TxId = nanoid:gen(),
                Now = calendar:universal_time(),

                Transaction = #solana_transaction{
                    tx_id = TxId,
                    wallet_id = WalletId,
                    user_id = Wallet#solana_wallet.user_id,
                    signature = maps:get(signature, TxData, undefined),
                    tx_type = maps:get(tx_type, TxData),
                    from_address = maps:get(from_address, TxData, undefined),
                    to_address = maps:get(to_address, TxData, undefined),
                    amount_lamports = maps:get(amount_lamports, TxData, 0),
                    token_mint = maps:get(token_mint, TxData, undefined),
                    nft_mint = maps:get(nft_mint, TxData, undefined),
                    status = maps:get(status, TxData, pending),
                    fee_lamports = maps:get(fee_lamports, TxData, 0),
                    slot = maps:get(slot, TxData, undefined),
                    block_time = maps:get(block_time, TxData, undefined),
                    memo = maps:get(memo, TxData, undefined),
                    error_message = maps:get(error_message, TxData, undefined),
                    created_at = Now,
                    confirmed_at = maps:get(confirmed_at, TxData, undefined),
                    metadata = maps:get(metadata, TxData, #{})
                },
                mnesia:write(Transaction),
                {ok, TxId}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_transaction(TxId) ->
    Fun = fun() ->
        case mnesia:read(solana_transaction, TxId) of
            [] -> {error, transaction_not_found};
            [Transaction] -> {ok, Transaction}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet_transactions(WalletId) ->
    Fun = fun() ->
        Transactions = mnesia:index_read(solana_transaction, WalletId, #solana_transaction.wallet_id),
        lists:sort(fun(A, B) ->
            A#solana_transaction.created_at >= B#solana_transaction.created_at
        end, Transactions)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Transactions} -> {ok, Transactions};
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet_transactions(WalletId, Limit, Offset) ->
    Fun = fun() ->
        AllTxs = mnesia:index_read(solana_transaction, WalletId, #solana_transaction.wallet_id),
        Sorted = lists:sort(fun(A, B) ->
            A#solana_transaction.created_at >= B#solana_transaction.created_at
        end, AllTxs),
        Paginated = lists:sublist(Sorted, Offset + 1, Limit),
        {Paginated, length(AllTxs)}
    end,
    case mnesia:transaction(Fun) of
        {atomic, {Transactions, Total}} -> {ok, Transactions, Total};
        {aborted, Reason} -> {error, Reason}
    end.

get_user_transactions(UserId) ->
    Fun = fun() ->
        Transactions = mnesia:index_read(solana_transaction, UserId, #solana_transaction.user_id),
        lists:sort(fun(A, B) ->
            A#solana_transaction.created_at >= B#solana_transaction.created_at
        end, Transactions)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Transactions} -> {ok, Transactions};
        {aborted, Reason} -> {error, Reason}
    end.

get_transactions_by_type(WalletId, TxType) ->
    Fun = fun() ->
        AllTxs = mnesia:index_read(solana_transaction, WalletId, #solana_transaction.wallet_id),
        Filtered = lists:filter(fun(Tx) ->
            Tx#solana_transaction.tx_type =:= TxType
        end, AllTxs),
        lists:sort(fun(A, B) ->
            A#solana_transaction.created_at >= B#solana_transaction.created_at
        end, Filtered)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Transactions} -> {ok, Transactions};
        {aborted, Reason} -> {error, Reason}
    end.

update_transaction_status(TxId, Status, ConfirmedAt) ->
    Fun = fun() ->
        case mnesia:read(solana_transaction, TxId) of
            [] -> {error, transaction_not_found};
            [Transaction] ->
                UpdatedTx = Transaction#solana_transaction{
                    status = Status,
                    confirmed_at = ConfirmedAt
                },
                mnesia:write(UpdatedTx),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

create_airdrop(WalletId, AirdropData) ->
    Fun = fun() ->
        case mnesia:read(solana_wallet, WalletId) of
            [] -> {error, wallet_not_found};
            [Wallet] ->
                AirdropId = nanoid:gen(),
                Now = calendar:universal_time(),

                Airdrop = #solana_airdrop{
                    airdrop_id = AirdropId,
                    wallet_id = WalletId,
                    user_id = Wallet#solana_wallet.user_id,
                    type = maps:get(type, AirdropData),
                    token_mint = maps:get(token_mint, AirdropData, undefined),
                    nft_collection = maps:get(nft_collection, AirdropData, undefined),
                    total_recipients = maps:get(total_recipients, AirdropData),
                    successful = maps:get(successful, AirdropData, 0),
                    failed = maps:get(failed, AirdropData, 0),
                    total_amount_lamports = maps:get(total_amount_lamports, AirdropData, 0),
                    status = maps:get(status, AirdropData, pending),
                    created_at = Now,
                    started_at = maps:get(started_at, AirdropData, undefined),
                    completed_at = maps:get(completed_at, AirdropData, undefined),
                    metadata = maps:get(metadata, AirdropData, #{})
                },
                mnesia:write(Airdrop),
                {ok, AirdropId}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_airdrop(AirdropId) ->
    Fun = fun() ->
        case mnesia:read(solana_airdrop, AirdropId) of
            [] -> {error, airdrop_not_found};
            [Airdrop] -> {ok, Airdrop}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

update_airdrop(AirdropId, Updates) ->
    Fun = fun() ->
        case mnesia:read(solana_airdrop, AirdropId) of
            [] -> {error, airdrop_not_found};
            [Airdrop] ->
                UpdatedAirdrop = Airdrop#solana_airdrop{
                    successful = maps:get(successful, Updates, Airdrop#solana_airdrop.successful),
                    failed = maps:get(failed, Updates, Airdrop#solana_airdrop.failed),
                    status = maps:get(status, Updates, Airdrop#solana_airdrop.status),
                    started_at = maps:get(started_at, Updates, Airdrop#solana_airdrop.started_at),
                    completed_at = maps:get(completed_at, Updates, Airdrop#solana_airdrop.completed_at),
                    metadata = maps:get(metadata, Updates, Airdrop#solana_airdrop.metadata)
                },
                mnesia:write(UpdatedAirdrop),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_user_airdrops(UserId) ->
    Fun = fun() ->
        Airdrops = mnesia:index_read(solana_airdrop, UserId, #solana_airdrop.user_id),
        lists:sort(fun(A, B) ->
            A#solana_airdrop.created_at >= B#solana_airdrop.created_at
        end, Airdrops)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Airdrops} -> {ok, Airdrops};
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet_airdrops(WalletId) ->
    Fun = fun() ->
        Airdrops = mnesia:index_read(solana_airdrop, WalletId, #solana_airdrop.wallet_id),
        lists:sort(fun(A, B) ->
            A#solana_airdrop.created_at >= B#solana_airdrop.created_at
        end, Airdrops)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Airdrops} -> {ok, Airdrops};
        {aborted, Reason} -> {error, Reason}
    end.

create_airdrop_recipient(AirdropId, RecipientData) ->
    Fun = fun() ->
        case mnesia:read(solana_airdrop, AirdropId) of
            [] -> {error, airdrop_not_found};
            [_Airdrop] ->
                RecipientId = nanoid:gen(),

                Recipient = #solana_airdrop_recipient{
                    id = RecipientId,
                    airdrop_id = AirdropId,
                    recipient_address = maps:get(recipient_address, RecipientData),
                    amount_lamports = maps:get(amount_lamports, RecipientData, 0),
                    mint_address = maps:get(mint_address, RecipientData, undefined),
                    success = maps:get(success, RecipientData, false),
                    signature = maps:get(signature, RecipientData, undefined),
                    error_message = maps:get(error_message, RecipientData, undefined),
                    processed_at = maps:get(processed_at, RecipientData, undefined),
                    metadata = maps:get(metadata, RecipientData, #{})
                },
                mnesia:write(Recipient),
                {ok, RecipientId}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_airdrop_recipients(AirdropId) ->
    Fun = fun() ->
        mnesia:index_read(solana_airdrop_recipient, AirdropId, #solana_airdrop_recipient.airdrop_id)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Recipients} -> {ok, Recipients};
        {aborted, Reason} -> {error, Reason}
    end.

update_airdrop_recipient(RecipientId, Updates) ->
    Fun = fun() ->
        case mnesia:read(solana_airdrop_recipient, RecipientId) of
            [] -> {error, recipient_not_found};
            [Recipient] ->
                UpdatedRecipient = Recipient#solana_airdrop_recipient{
                    success = maps:get(success, Updates, Recipient#solana_airdrop_recipient.success),
                    signature = maps:get(signature, Updates, Recipient#solana_airdrop_recipient.signature),
                    error_message = maps:get(error_message, Updates, Recipient#solana_airdrop_recipient.error_message),
                    processed_at = maps:get(processed_at, Updates, Recipient#solana_airdrop_recipient.processed_at),
                    metadata = maps:get(metadata, Updates, Recipient#solana_airdrop_recipient.metadata)
                },
                mnesia:write(UpdatedRecipient),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

create_stake_account(WalletId, StakeData) ->
    Fun = fun() ->
        case mnesia:read(solana_wallet, WalletId) of
            [] -> {error, wallet_not_found};
            [Wallet] ->
                StakeId = nanoid:gen(),
                Now = calendar:universal_time(),

                StakeAccount = #solana_stake_account{
                    stake_id = StakeId,
                    wallet_id = WalletId,
                    user_id = Wallet#solana_wallet.user_id,
                    stake_account_address = maps:get(stake_account_address, StakeData),
                    validator_vote_address = maps:get(validator_vote_address, StakeData),
                    amount_lamports = maps:get(amount_lamports, StakeData),
                    status = maps:get(status, StakeData, created),
                    signature = maps:get(signature, StakeData),
                    created_at = Now,
                    delegated_at = maps:get(delegated_at, StakeData, undefined),
                    deactivated_at = maps:get(deactivated_at, StakeData, undefined),
                    withdrawn_at = maps:get(withdrawn_at, StakeData, undefined),
                    rewards_earned_lamports = maps:get(rewards_earned_lamports, StakeData, 0),
                    metadata = maps:get(metadata, StakeData, #{})
                },
                mnesia:write(StakeAccount),
                {ok, StakeId}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_stake_account(StakeId) ->
    Fun = fun() ->
        case mnesia:read(solana_stake_account, StakeId) of
            [] -> {error, stake_account_not_found};
            [StakeAccount] -> {ok, StakeAccount}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_stake_by_address(StakeAccountAddress) ->
    Fun = fun() ->
        case mnesia:index_read(solana_stake_account, StakeAccountAddress, #solana_stake_account.stake_account_address) of
            [] -> {error, stake_account_not_found};
            [StakeAccount] -> {ok, StakeAccount};
            [StakeAccount | _] -> {ok, StakeAccount}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

update_stake_account(StakeId, Updates) ->
    Fun = fun() ->
        case mnesia:read(solana_stake_account, StakeId) of
            [] -> {error, stake_account_not_found};
            [StakeAccount] ->
                UpdatedStake = StakeAccount#solana_stake_account{
                    status = maps:get(status, Updates, StakeAccount#solana_stake_account.status),
                    delegated_at = maps:get(delegated_at, Updates, StakeAccount#solana_stake_account.delegated_at),
                    deactivated_at = maps:get(deactivated_at, Updates, StakeAccount#solana_stake_account.deactivated_at),
                    withdrawn_at = maps:get(withdrawn_at, Updates, StakeAccount#solana_stake_account.withdrawn_at),
                    rewards_earned_lamports = maps:get(rewards_earned_lamports, Updates, StakeAccount#solana_stake_account.rewards_earned_lamports),
                    metadata = maps:get(metadata, Updates, StakeAccount#solana_stake_account.metadata)
                },
                mnesia:write(UpdatedStake),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_user_stakes(UserId) ->
    Fun = fun() ->
        Stakes = mnesia:index_read(solana_stake_account, UserId, #solana_stake_account.user_id),
        lists:sort(fun(A, B) ->
            A#solana_stake_account.created_at >= B#solana_stake_account.created_at
        end, Stakes)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Stakes} -> {ok, Stakes};
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet_stakes(WalletId) ->
    Fun = fun() ->
        Stakes = mnesia:index_read(solana_stake_account, WalletId, #solana_stake_account.wallet_id),
        lists:sort(fun(A, B) ->
            A#solana_stake_account.created_at >= B#solana_stake_account.created_at
        end, Stakes)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Stakes} -> {ok, Stakes};
        {aborted, Reason} -> {error, Reason}
    end.

create_token_account(WalletId, TokenData) ->
    Fun = fun() ->
        case mnesia:read(solana_wallet, WalletId) of
            [] -> {error, wallet_not_found};
            [Wallet] ->
                TokenAccountId = nanoid:gen(),
                Now = calendar:universal_time(),

                TokenAccount = #solana_token_account{
                    id = TokenAccountId,
                    wallet_id = WalletId,
                    user_id = Wallet#solana_wallet.user_id,
                    token_account_address = maps:get(token_account_address, TokenData),
                    token_mint = maps:get(token_mint, TokenData),
                    balance = maps:get(balance, TokenData, <<"0">>),
                    decimals = maps:get(decimals, TokenData, 0),
                    owner_address = maps:get(owner_address, TokenData),
                    created_at = Now,
                    last_synced = Now,
                    metadata = maps:get(metadata, TokenData, #{})
                },
                mnesia:write(TokenAccount),
                {ok, TokenAccountId}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_token_account(TokenAccountId) ->
    Fun = fun() ->
        case mnesia:read(solana_token_account, TokenAccountId) of
            [] -> {error, token_account_not_found};
            [TokenAccount] -> {ok, TokenAccount}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet_token_accounts(WalletId) ->
    Fun = fun() ->
        TokenAccounts = mnesia:index_read(solana_token_account, WalletId, #solana_token_account.wallet_id),
        lists:sort(fun(A, B) ->
            A#solana_token_account.created_at >= B#solana_token_account.created_at
        end, TokenAccounts)
    end,
    case mnesia:transaction(Fun) of
        {atomic, TokenAccounts} -> {ok, TokenAccounts};
        {aborted, Reason} -> {error, Reason}
    end.

update_token_account_balance(TokenAccountId, NewBalance, LastSynced) ->
    Fun = fun() ->
        case mnesia:read(solana_token_account, TokenAccountId) of
            [] -> {error, token_account_not_found};
            [TokenAccount] ->
                UpdatedAccount = TokenAccount#solana_token_account{
                    balance = NewBalance,
                    last_synced = LastSynced
                },
                mnesia:write(UpdatedAccount),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

create_nft(WalletId, NFTData) ->
    Fun = fun() ->
        case mnesia:read(solana_wallet, WalletId) of
            [] -> {error, wallet_not_found};
            [Wallet] ->
                NFTId = nanoid:gen(),
                Now = calendar:universal_time(),

                NFT = #solana_nft{
                    id = NFTId,
                    wallet_id = WalletId,
                    user_id = Wallet#solana_wallet.user_id,
                    mint_address = maps:get(mint_address, NFTData),
                    token_account_address = maps:get(token_account_address, NFTData),
                    name = maps:get(name, NFTData, undefined),
                    symbol = maps:get(symbol, NFTData, undefined),
                    uri = maps:get(uri, NFTData, undefined),
                    collection_address = maps:get(collection_address, NFTData, undefined),
                    verified = maps:get(verified, NFTData, false),
                    creators = maps:get(creators, NFTData, []),
                    attributes = maps:get(attributes, NFTData, []),
                    image_url = maps:get(image_url, NFTData, undefined),
                    created_at = Now,
                    last_synced = Now,
                    metadata = maps:get(metadata, NFTData, #{})
                },
                mnesia:write(NFT),
                {ok, NFTId}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_nft(NFTId) ->
    Fun = fun() ->
        case mnesia:read(solana_nft, NFTId) of
            [] -> {error, nft_not_found};
            [NFT] -> {ok, NFT}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_wallet_nfts(WalletId) ->
    Fun = fun() ->
        NFTs = mnesia:index_read(solana_nft, WalletId, #solana_nft.wallet_id),
        lists:sort(fun(A, B) ->
            A#solana_nft.created_at >= B#solana_nft.created_at
        end, NFTs)
    end,
    case mnesia:transaction(Fun) of
        {atomic, NFTs} -> {ok, NFTs};
        {aborted, Reason} -> {error, Reason}
    end.

update_nft(NFTId, Updates) ->
    Fun = fun() ->
        case mnesia:read(solana_nft, NFTId) of
            [] -> {error, nft_not_found};
            [NFT] ->
                UpdatedNFT = NFT#solana_nft{
                    name = maps:get(name, Updates, NFT#solana_nft.name),
                    symbol = maps:get(symbol, Updates, NFT#solana_nft.symbol),
                    uri = maps:get(uri, Updates, NFT#solana_nft.uri),
                    collection_address = maps:get(collection_address, Updates, NFT#solana_nft.collection_address),
                    verified = maps:get(verified, Updates, NFT#solana_nft.verified),
                    creators = maps:get(creators, Updates, NFT#solana_nft.creators),
                    attributes = maps:get(attributes, Updates, NFT#solana_nft.attributes),
                    image_url = maps:get(image_url, Updates, NFT#solana_nft.image_url),
                    last_synced = maps:get(last_synced, Updates, NFT#solana_nft.last_synced),
                    metadata = maps:get(metadata, Updates, NFT#solana_nft.metadata)
                },
                mnesia:write(UpdatedNFT),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.
