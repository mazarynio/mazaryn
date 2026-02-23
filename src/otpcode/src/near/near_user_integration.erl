-module(near_user_integration).
-author("Zaryn Technologies").
-include("../records.hrl").
-include("../wallet_records.hrl").
-export([
    create_wallet_for_user/2,
    create_wallet_for_user/3,
    import_wallet_for_user/3,
    import_wallet_for_user/4,
    get_user_primary_wallet/2,
    get_user_wallets/2,
    get_user_balance/2,
    get_user_multi_balance/2,
    transfer_between_users/5,
    transfer_between_users/6,
    transfer_ft_between_users/6,
    tip_user/4,
    tip_user/5,
    create_post_for_user/3,
    create_post_for_user/4,
    like_post_for_user/3,
    comment_for_user/4,
    follow_user/3,
    unfollow_user/3,
    get_user_feed/2,
    get_user_feed/3,
    get_user_social_profile/2,
    set_user_social_profile/3,
    get_user_notifications/2,
    stake_for_user/4,
    unstake_for_user/3,
    withdraw_for_user/3,
    get_user_staked_balance/2,
    ensure_user_has_wallet/2,
    get_near_summary/2
]).

create_wallet_for_user(Token, Username) ->
    create_wallet_for_user(Token, Username, undefined).

create_wallet_for_user(Token, Username, Label) ->
    case get_user_id(Username) of
        {error, _} = Err -> Err;
        {ok, UserId} ->
            case near_wallet:create_wallet(Token, UserId, Label) of
                {error, _} = Err -> Err;
                {ok, WalletInfo} ->
                    WalletId = maps:get(wallet_id, WalletInfo),
                    AccountId = maps:get(account_id, WalletInfo),
                    log_wallet_created(Username, UserId, WalletId, AccountId),
                    {ok, WalletInfo}
            end
    end.

import_wallet_for_user(Token, Username, SeedPhrase) ->
    import_wallet_for_user(Token, Username, SeedPhrase, undefined).

import_wallet_for_user(Token, Username, SeedPhrase, Label) ->
    case get_user_id(Username) of
        {error, _} = Err -> Err;
        {ok, UserId} ->
            case near_wallet:import_wallet(Token, UserId, SeedPhrase, Label) of
                {error, _} = Err -> Err;
                {ok, WalletInfo} ->
                    WalletId = maps:get(wallet_id, WalletInfo),
                    AccountId = maps:get(account_id, WalletInfo),
                    log_wallet_created(Username, UserId, WalletId, AccountId),
                    {ok, WalletInfo}
            end
    end.

get_user_primary_wallet(Token, Username) ->
    case get_user_id(Username) of
        {error, _} = Err -> Err;
        {ok, UserId} ->
            case near_walletdb:get_primary_wallet(UserId) of
                {error, _} = Err -> Err;
                {ok, Wallet} ->
                    near_wallet:get_wallet_info(Token, Wallet#near_wallet.wallet_id)
            end
    end.

get_user_wallets(Token, Username) ->
    case get_user_id(Username) of
        {error, _} = Err -> Err;
        {ok, UserId} ->
            near_wallet:get_user_wallets(Token, UserId)
    end.

get_user_balance(Token, Username) ->
    case resolve_primary_wallet_id(Username) of
        {error, _} = Err -> Err;
        {ok, WalletId} ->
            near_wallet:get_balance(Token, WalletId)
    end.

get_user_multi_balance(Token, Username) ->
    case resolve_primary_wallet_id(Username) of
        {error, _} = Err -> Err;
        {ok, WalletId} ->
            near_wallet:get_multi_balance(Token, WalletId)
    end.

transfer_between_users(Token, FromUsername, ToUsername, AmountNear, _Opts) ->
    transfer_between_users(Token, FromUsername, ToUsername, AmountNear, _Opts, undefined).

transfer_between_users(Token, FromUsername, ToUsername, AmountNear, _Opts, Memo) ->
    case resolve_primary_wallet_id(FromUsername) of
        {error, _} = Err -> Err;
        {ok, FromWalletId} ->
            case resolve_account_id(ToUsername) of
                {error, _} = Err -> Err;
                {ok, ToAccountId} ->
                    near_wallet:transfer(Token, FromWalletId, ToAccountId, AmountNear, Memo)
            end
    end.

transfer_ft_between_users(Token, FromUsername, ToUsername, ContractId, Amount, Opts) ->
    case resolve_primary_wallet_id(FromUsername) of
        {error, _} = Err -> Err;
        {ok, FromWalletId} ->
            case resolve_account_id(ToUsername) of
                {error, _} = Err -> Err;
                {ok, ToAccountId} ->
                    Memo = maps:get(memo, Opts, undefined),
                    near_tokens:transfer_ft(Token, FromWalletId, ToAccountId, ContractId, Amount, Memo)
            end
    end.

tip_user(Token, FromUsername, ToUsername, AmountNear) ->
    tip_user(Token, FromUsername, ToUsername, AmountNear, undefined).

tip_user(Token, FromUsername, ToUsername, AmountNear, Memo) ->
    case resolve_primary_wallet_id(FromUsername) of
        {error, _} = Err -> Err;
        {ok, FromWalletId} ->
            case resolve_account_id(ToUsername) of
                {error, _} = Err -> Err;
                {ok, ToAccountId} ->
                    near_social:tip_account(Token, FromWalletId, ToAccountId, AmountNear, Memo)
            end
    end.

create_post_for_user(Token, Username, Text) ->
    create_post_for_user(Token, Username, Text, #{}).

create_post_for_user(Token, Username, Text, Opts) ->
    case resolve_primary_wallet_id(Username) of
        {error, _} = Err -> Err;
        {ok, WalletId} ->
            near_social:create_post(Token, WalletId, Text, Opts)
    end.

like_post_for_user(Token, Username, PostKey) ->
    case resolve_primary_wallet_id(Username) of
        {error, _} = Err -> Err;
        {ok, WalletId} ->
            near_social:like_post(Token, WalletId, PostKey)
    end.

comment_for_user(Token, Username, PostKey, Text) ->
    case resolve_primary_wallet_id(Username) of
        {error, _} = Err -> Err;
        {ok, WalletId} ->
            near_social:comment_on_post(Token, WalletId, PostKey, Text)
    end.

follow_user(Token, FromUsername, ToUsername) ->
    case resolve_primary_wallet_id(FromUsername) of
        {error, _} = Err -> Err;
        {ok, FromWalletId} ->
            case resolve_account_id(ToUsername) of
                {error, _} = Err -> Err;
                {ok, ToAccountId} ->
                    near_social:follow_account(Token, FromWalletId, ToAccountId)
            end
    end.

unfollow_user(Token, FromUsername, ToUsername) ->
    case resolve_primary_wallet_id(FromUsername) of
        {error, _} = Err -> Err;
        {ok, FromWalletId} ->
            case resolve_account_id(ToUsername) of
                {error, _} = Err -> Err;
                {ok, ToAccountId} ->
                    near_social:unfollow_account(Token, FromWalletId, ToAccountId)
            end
    end.

get_user_feed(Token, Username) ->
    get_user_feed(Token, Username, #{}).

get_user_feed(Token, Username, Opts) ->
    case resolve_primary_wallet_id(Username) of
        {error, _} = Err -> Err;
        {ok, WalletId} ->
            near_social:get_feed(Token, WalletId, Opts)
    end.

get_user_social_profile(Token, Username) ->
    case resolve_account_id(Username) of
        {error, _} = Err -> Err;
        {ok, AccountId} ->
            near_social:get_social_profile(Token, AccountId)
    end.

set_user_social_profile(Token, Username, ProfileData) ->
    case resolve_primary_wallet_id(Username) of
        {error, _} = Err -> Err;
        {ok, WalletId} ->
            near_social:set_social_profile(Token, WalletId, ProfileData)
    end.

get_user_notifications(Token, Username) ->
    case resolve_primary_wallet_id(Username) of
        {error, _} = Err -> Err;
        {ok, WalletId} ->
            near_social:get_notifications(Token, WalletId)
    end.

stake_for_user(Token, Username, ValidatorId, AmountNear) ->
    case resolve_primary_wallet_id(Username) of
        {error, _} = Err -> Err;
        {ok, WalletId} ->
            near_staking:stake(Token, WalletId, ValidatorId, AmountNear)
    end.

unstake_for_user(Token, Username, AmountNear) ->
    case resolve_primary_wallet_id(Username) of
        {error, _} = Err -> Err;
        {ok, WalletId} ->
            near_staking:unstake(Token, WalletId, AmountNear)
    end.

withdraw_for_user(Token, Username, AmountNear) ->
    case resolve_primary_wallet_id(Username) of
        {error, _} = Err -> Err;
        {ok, WalletId} ->
            near_staking:withdraw(Token, WalletId, AmountNear)
    end.

get_user_staked_balance(Token, Username) ->
    case resolve_primary_wallet_id(Username) of
        {error, _} = Err -> Err;
        {ok, WalletId} ->
            near_staking:get_staked_balance(Token, WalletId)
    end.

ensure_user_has_wallet(Token, Username) ->
    case get_user_id(Username) of
        {error, _} = Err -> Err;
        {ok, UserId} ->
            case near_walletdb:get_primary_wallet(UserId) of
                {ok, Wallet} ->
                    {ok, already_exists, Wallet#near_wallet.wallet_id};
                {error, no_wallets_found} ->
                    case create_wallet_for_user(Token, Username) of
                        {error, _} = Err -> Err;
                        {ok, WalletInfo} ->
                            {ok, created, maps:get(wallet_id, WalletInfo)}
                    end;
                {error, _} = Err -> Err
            end
    end.

get_near_summary(Token, Username) ->
    case get_user_id(Username) of
        {error, _} = Err -> Err;
        {ok, UserId} ->
            case near_walletdb:get_primary_wallet(UserId) of
                {error, _} = Err -> Err;
                {ok, Wallet} ->
                    WalletId = Wallet#near_wallet.wallet_id,
                    AccountId = Wallet#near_wallet.account_id,
                    BalanceResult = near_wallet:get_balance(Token, WalletId),
                    StakeResult = near_staking:get_all_staked(Token, WalletId),
                    PostsResult = near_social:get_user_posts(Token, AccountId, #{limit => 5}),
                    NotifResult = near_social:get_notifications(Token, WalletId, #{limit => 5}),
                    {ok, #{
                        username => Username,
                        user_id => UserId,
                        wallet_id => WalletId,
                        account_id => AccountId,
                        network => Wallet#near_wallet.network,
                        balance => case BalanceResult of
                            {ok, B} -> B;
                            _ -> undefined
                        end,
                        staking => case StakeResult of
                            {ok, S} -> S;
                            _ -> undefined
                        end,
                        recent_posts => case PostsResult of
                            {ok, #{posts := Posts}} -> Posts;
                            _ -> []
                        end,
                        recent_notifications => case NotifResult of
                            {ok, #{notifications := Notifs}} -> Notifs;
                            _ -> []
                        end
                    }}
            end
    end.

resolve_primary_wallet_id(Username) ->
    case get_user_id(Username) of
        {error, _} = Err -> Err;
        {ok, UserId} ->
            case near_walletdb:get_primary_wallet(UserId) of
                {error, _} = Err -> Err;
                {ok, Wallet} -> {ok, Wallet#near_wallet.wallet_id}
            end
    end.

resolve_account_id(Username) ->
    case get_user_id(Username) of
        {error, user_not_found} ->
            case looks_like_near_account(Username) of
                true -> {ok, Username};
                false -> {error, user_not_found}
            end;
        {error, _} = Err -> Err;
        {ok, UserId} ->
            case near_walletdb:get_primary_wallet(UserId) of
                {error, _} = Err -> Err;
                {ok, Wallet} -> {ok, Wallet#near_wallet.account_id}
            end
    end.

get_user_id(Username) ->
    try
        case userdb:get_user_by_username(Username) of
            {ok, User} -> {ok, User#user.id};
            {error, not_found} -> {error, user_not_found};
            {error, _} = Err -> Err;
            [] -> {error, user_not_found};
            [User | _] -> {ok, User#user.id}
        end
    catch
        error:undef ->
            try
                case user:get_user(Username) of
                    [] -> {error, user_not_found};
                    [U | _] -> {ok, U#user.id};
                    {error, _} -> {error, sth_bad_happened}
                end
            catch
                error:undef -> {error, user_not_found}
            end
    end.

looks_like_near_account(Value) when is_list(Value) ->
    case string:find(Value, ".") of
        nomatch ->
            length(Value) =:= 64 andalso
            lists:all(fun(C) ->
                (C >= $0 andalso C =< $9) orelse
                (C >= $a andalso C =< $f)
            end, Value);
        _ ->
            true
    end;
looks_like_near_account(Value) when is_binary(Value) ->
    looks_like_near_account(binary_to_list(Value));
looks_like_near_account(_) ->
    false.

log_wallet_created(Username, UserId, WalletId, AccountId) ->
    error_logger:info_msg(
        "[near_user_integration] wallet created: username=~s user_id=~s wallet_id=~s account_id=~s~n",
        [Username, UserId, WalletId, AccountId]
    ).
