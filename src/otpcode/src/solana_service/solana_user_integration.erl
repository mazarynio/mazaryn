-module(solana_user_integration).
-author("Zaryn Technologies").
-include("../records.hrl").
-include("../business.hrl").
-include("../wallet_records.hrl").
-export([
    link_wallet_to_user/2,
    get_user_solana_wallets/1,
    get_user_primary_wallet/1,
    sync_wallet_with_typescript/2,
    get_wallet_owner/1,
    verify_wallet_ownership/2,
    create_wallet_for_user/1,
    create_wallet_for_user/2,
    import_wallet_for_user/2,
    import_wallet_for_user/3,
    get_user_solana_balance/1,
    get_user_total_balance/1,
    attach_wallet_to_post/2,
    attach_wallet_to_business/2,
    get_post_creator_wallet/1,
    get_business_owner_wallet/1,
    enable_wallet_notifications/2,
    disable_wallet_notifications/2,
    get_wallet_activity_feed/2,
    record_wallet_transaction_to_feed/3
]).

link_wallet_to_user(WalletId, Username) ->
    Fun = fun() ->
        case mnesia:index_read(user, Username, #user.username) of
            [] -> {error, user_not_found};
            [User] ->
                case solana_walletdb:get_wallet(WalletId) of
                    {ok, Wallet} ->
                        if
                            Wallet#solana_wallet.user_id =:= User#user.id ->
                                {ok, already_linked};
                            true ->
                                {error, wallet_belongs_to_another_user}
                        end;
                    {error, _} = Error -> Error
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_user_solana_wallets(Username) ->
    case userdb:get_user(Username) of
        not_exist -> {error, user_not_found};
        error -> {error, user_fetch_failed};
        User ->
            solana_walletdb:get_user_wallets(User#user.id)
    end.

get_user_primary_wallet(Username) ->
    case userdb:get_user(Username) of
        not_exist -> {error, user_not_found};
        error -> {error, user_fetch_failed};
        User ->
            solana_walletdb:get_primary_wallet(User#user.id)
    end.

sync_wallet_with_typescript(WalletId, TypescriptWalletData) ->
    Updates = #{
        encrypted_private_key => maps:get(encrypted_private_key, TypescriptWalletData, undefined),
        encryption_iv => maps:get(encryption_iv, TypescriptWalletData, undefined),
        encryption_auth_tag => maps:get(encryption_auth_tag, TypescriptWalletData, undefined),
        label => maps:get(label, TypescriptWalletData, undefined)
    },
    FilteredUpdates = maps:filter(fun(_, V) -> V =/= undefined end, Updates),
    case maps:size(FilteredUpdates) of
        0 -> {ok, no_updates};
        _ ->
            Fun = fun() ->
                case mnesia:read(solana_wallet, WalletId) of
                    [] -> {error, wallet_not_found};
                    [Wallet] ->
                        UpdatedWallet = Wallet#solana_wallet{
                            encrypted_private_key = maps:get(encrypted_private_key, FilteredUpdates, Wallet#solana_wallet.encrypted_private_key),
                            encryption_iv = maps:get(encryption_iv, FilteredUpdates, Wallet#solana_wallet.encryption_iv),
                            encryption_auth_tag = maps:get(encryption_auth_tag, FilteredUpdates, Wallet#solana_wallet.encryption_auth_tag),
                            label = maps:get(label, FilteredUpdates, Wallet#solana_wallet.label)
                        },
                        mnesia:write(UpdatedWallet),
                        ok
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, Reason}
            end
    end.

get_wallet_owner(WalletId) ->
    case solana_walletdb:get_wallet(WalletId) of
        {ok, Wallet} ->
            case userdb:get_user_by_id(Wallet#solana_wallet.user_id) of
                user_not_exist -> {error, owner_not_found};
                error -> {error, user_fetch_failed};
                User -> {ok, User}
            end;
        {error, _} = Error -> Error
    end.

verify_wallet_ownership(WalletId, Username) ->
    case userdb:get_user(Username) of
        not_exist -> {error, user_not_found};
        error -> {error, user_fetch_failed};
        User ->
            case solana_walletdb:get_wallet(WalletId) of
                {ok, Wallet} ->
                    Wallet#solana_wallet.user_id =:= User#user.id;
                {error, _} -> false
            end
    end.

create_wallet_for_user(Username) ->
    create_wallet_for_user(Username, undefined).

create_wallet_for_user(Username, Label) ->
    case userdb:get_user(Username) of
        not_exist -> {error, user_not_found};
        error -> {error, user_fetch_failed};
        User ->
            TempKey = crypto:strong_rand_bytes(32),
            TempIV = crypto:strong_rand_bytes(16),
            TempAuthTag = crypto:strong_rand_bytes(16),
            TempPublicKey = base64:encode(crypto:strong_rand_bytes(32)),

            case solana_walletdb:create_wallet(
                User#user.id,
                binary_to_list(TempPublicKey),
                base64:encode(TempKey),
                base64:encode(TempIV),
                base64:encode(TempAuthTag),
                Label
            ) of
                {ok, WalletId} ->
                    {ok, WalletId, needs_typescript_sync};
                {error, _} = Error -> Error
            end
    end.

import_wallet_for_user(Username, PrivateKey) ->
    import_wallet_for_user(Username, PrivateKey, undefined).

import_wallet_for_user(Username, PrivateKey, Label) ->
    case userdb:get_user(Username) of
        not_exist -> {error, user_not_found};
        error -> {error, user_fetch_failed};
        User ->
            TempIV = crypto:strong_rand_bytes(16),
            TempAuthTag = crypto:strong_rand_bytes(16),
            TempPublicKey = base64:encode(crypto:strong_rand_bytes(32)),

            case solana_walletdb:import_wallet(
                User#user.id,
                binary_to_list(TempPublicKey),
                base64:encode(PrivateKey),
                base64:encode(TempIV),
                base64:encode(TempAuthTag),
                "m/44'/501'/0'/0'",
                Label
            ) of
                {ok, WalletId} ->
                    {ok, WalletId, needs_typescript_sync};
                {error, _} = Error -> Error
            end
    end.

get_user_solana_balance(Username) ->
    case get_user_solana_wallets(Username) of
        {ok, []} -> {ok, 0, []};
        {ok, Wallets} ->
            WalletBalances = lists:map(fun(Wallet) ->
                #{
                    wallet_id => Wallet#solana_wallet.wallet_id,
                    public_key => Wallet#solana_wallet.public_key,
                    label => Wallet#solana_wallet.label,
                    is_primary => Wallet#solana_wallet.is_primary,
                    balance_needs_fetch => true
                }
            end, Wallets),
            {ok, length(Wallets), WalletBalances};
        {error, _} = Error -> Error
    end.

get_user_total_balance(Username) ->
    case get_user_solana_balance(Username) of
        {ok, _Count, Wallets} ->
            {ok, Wallets, needs_typescript_balance_fetch};
        {error, _} = Error -> Error
    end.

attach_wallet_to_post(PostId, WalletId) ->
    Fun = fun() ->
        case mnesia:read(post, PostId) of
            [] -> {error, post_not_found};
            [Post] ->
                case solana_walletdb:get_wallet(WalletId) of
                    {ok, Wallet} ->
                        if
                            Post#post.user_id =:= Wallet#solana_wallet.user_id ->
                                Metadata = Post#post.data,
                                UpdatedMetadata = Metadata#{
                                    solana_wallet_id => WalletId,
                                    solana_public_key => Wallet#solana_wallet.public_key
                                },
                                UpdatedPost = Post#post{data = UpdatedMetadata},
                                mnesia:write(UpdatedPost),
                                ok;
                            true ->
                                {error, wallet_user_mismatch}
                        end;
                    {error, _} = Error -> Error
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

attach_wallet_to_business(BusinessId, WalletId) ->
    Fun = fun() ->
        case mnesia:read(business, BusinessId) of
            [] -> {error, business_not_found};
            [Business] ->
                case solana_walletdb:get_wallet(WalletId) of
                    {ok, Wallet} ->
                        if
                            Business#business.user_id =:= Wallet#solana_wallet.user_id ->
                                Metadata = Business#business.data,
                                UpdatedMetadata = Metadata#{
                                    solana_wallet_id => WalletId,
                                    solana_public_key => Wallet#solana_wallet.public_key
                                },
                                UpdatedBusiness = Business#business{data = UpdatedMetadata},
                                mnesia:write(UpdatedBusiness),
                                ok;
                            true ->
                                {error, wallet_user_mismatch}
                        end;
                    {error, _} = Error -> Error
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_post_creator_wallet(PostId) ->
    Fun = fun() ->
        case mnesia:read(post, PostId) of
            [] -> {error, post_not_found};
            [Post] ->
                case maps:get(solana_wallet_id, Post#post.data, undefined) of
                    undefined ->
                        case solana_walletdb:get_primary_wallet(Post#post.user_id) of
                            {ok, Wallet} -> {ok, Wallet};
                            Error -> Error
                        end;
                    WalletId ->
                        solana_walletdb:get_wallet(WalletId)
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

get_business_owner_wallet(BusinessId) ->
    Fun = fun() ->
        case mnesia:read(business, BusinessId) of
            [] -> {error, business_not_found};
            [Business] ->
                case maps:get(solana_wallet_id, Business#business.data, undefined) of
                    undefined ->
                        case solana_walletdb:get_primary_wallet(Business#business.user_id) of
                            {ok, Wallet} -> {ok, Wallet};
                            Error -> Error
                        end;
                    WalletId ->
                        solana_walletdb:get_wallet(WalletId)
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

enable_wallet_notifications(Username, WalletId) ->
    case verify_wallet_ownership(WalletId, Username) of
        true ->
            Fun = fun() ->
                case mnesia:read(solana_wallet, WalletId) of
                    [] -> {error, wallet_not_found};
                    [Wallet] ->
                        Metadata = Wallet#solana_wallet.metadata,
                        UpdatedMetadata = Metadata#{notifications_enabled => true},
                        UpdatedWallet = Wallet#solana_wallet{metadata = UpdatedMetadata},
                        mnesia:write(UpdatedWallet),
                        ok
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, Reason}
            end;
        false -> {error, access_denied}
    end.

disable_wallet_notifications(Username, WalletId) ->
    case verify_wallet_ownership(WalletId, Username) of
        true ->
            Fun = fun() ->
                case mnesia:read(solana_wallet, WalletId) of
                    [] -> {error, wallet_not_found};
                    [Wallet] ->
                        Metadata = Wallet#solana_wallet.metadata,
                        UpdatedMetadata = Metadata#{notifications_enabled => false},
                        UpdatedWallet = Wallet#solana_wallet{metadata = UpdatedMetadata},
                        mnesia:write(UpdatedWallet),
                        ok
                end
            end,
            case mnesia:transaction(Fun) of
                {atomic, Result} -> Result;
                {aborted, Reason} -> {error, Reason}
            end;
        false -> {error, access_denied}
    end.

get_wallet_activity_feed(WalletId, Limit) ->
    case solana_walletdb:get_wallet_transactions(WalletId, Limit, 0) of
        {ok, Transactions, _Total} ->
            Activities = lists:map(fun(Tx) ->
                #{
                    type => transaction,
                    tx_id => Tx#solana_transaction.tx_id,
                    tx_type => Tx#solana_transaction.tx_type,
                    amount => Tx#solana_transaction.amount_lamports,
                    status => Tx#solana_transaction.status,
                    timestamp => Tx#solana_transaction.created_at,
                    signature => Tx#solana_transaction.signature
                }
            end, Transactions),
            {ok, Activities};
        {error, _} = Error -> Error
    end.

record_wallet_transaction_to_feed(WalletId, TxData, NotifyUser) ->
    case solana_walletdb:create_transaction(WalletId, TxData) of
        {ok, TxId} ->
            if
                NotifyUser ->
                    case solana_walletdb:get_wallet(WalletId) of
                        {ok, Wallet} ->
                            NotifId = nanoid:gen(),
                            TxType = maps:get(tx_type, TxData),
                            Amount = maps:get(amount_lamports, TxData, 0),
                            Message = format_transaction_message(TxType, Amount),

                            Notif = #notif{
                                id = NotifId,
                                user_id = Wallet#solana_wallet.user_id,
                                message = Message,
                                date_created = calendar:universal_time(),
                                read = false,
                                type = solana_transaction,
                                data = #{
                                    tx_id => TxId,
                                    wallet_id => WalletId,
                                    tx_type => TxType,
                                    amount => Amount
                                }
                            },

                            Fun = fun() -> mnesia:write(Notif) end,
                            mnesia:transaction(Fun),
                            {ok, TxId, notification_sent};
                        {error, _} ->
                            {ok, TxId, notification_failed}
                    end;
                true ->
                    {ok, TxId}
            end;
        {error, _} = Error -> Error
    end.

format_transaction_message(sol_transfer, Amount) ->
    AmountSol = Amount / 1000000000,
    io_lib:format("SOL Transfer: ~.4f SOL", [AmountSol]);
format_transaction_message(token_transfer, _Amount) ->
    "Token Transfer Completed";
format_transaction_message(nft_transfer, _Amount) ->
    "NFT Transfer Completed";
format_transaction_message(stake_created, Amount) ->
    AmountSol = Amount / 1000000000,
    io_lib:format("Stake Created: ~.4f SOL", [AmountSol]);
format_transaction_message(stake_deactivated, _Amount) ->
    "Stake Deactivated";
format_transaction_message(stake_withdrawn, Amount) ->
    AmountSol = Amount / 1000000000,
    io_lib:format("Stake Withdrawn: ~.4f SOL", [AmountSol]);
format_transaction_message(_, _) ->
    "Solana Transaction Completed".
