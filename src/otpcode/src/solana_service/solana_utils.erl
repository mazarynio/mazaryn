-module(solana_utils).
-author("Zaryn Technologies").
-include("../records.hrl").
-include("../wallet_records.hrl").
-export([
    lamports_to_sol/1,
    sol_to_lamports/1,
    format_public_key/1,
    truncate_signature/1,
    validate_public_key/1,
    validate_signature/1,
    format_timestamp/1,
    calculate_transaction_fee/2,
    estimate_rent_exemption/1,
    format_balance_display/1,
    format_balance_display/2,
    parse_transaction_type/1,
    get_transaction_status_color/1,
    format_wallet_summary/1,
    calculate_wallet_value/1,
    group_transactions_by_date/1,
    filter_transactions_by_date_range/3,
    get_transaction_statistics/1,
    format_airdrop_progress/1,
    calculate_stake_rewards/2,
    format_stake_status/1,
    get_validator_performance/1,
    is_valid_solana_address/1,
    generate_wallet_qr_data/1,
    parse_wallet_qr_data/1,
    encrypt_private_key/2,
    decrypt_private_key/2,
    hash_password/1,
    verify_password/2
]).

-define(LAMPORTS_PER_SOL, 1000000000).
-define(SIGNATURE_LENGTH, 88).
-define(PUBLIC_KEY_LENGTH, 44).

lamports_to_sol(Lamports) when is_integer(Lamports) ->
    Lamports / ?LAMPORTS_PER_SOL;
lamports_to_sol(_) -> 0.0.

sol_to_lamports(Sol) when is_number(Sol) ->
    round(Sol * ?LAMPORTS_PER_SOL);
sol_to_lamports(_) -> 0.

format_public_key(PublicKey) when is_list(PublicKey) ->
    case length(PublicKey) of
        Len when Len > 8 ->
            First = lists:sublist(PublicKey, 4),
            Last = lists:sublist(PublicKey, Len - 3, 4),
            First ++ "..." ++ Last;
        _ -> PublicKey
    end;
format_public_key(PublicKey) when is_binary(PublicKey) ->
    format_public_key(binary_to_list(PublicKey));
format_public_key(_) -> "invalid".

truncate_signature(Signature) when is_list(Signature) ->
    case length(Signature) of
        Len when Len > 16 ->
            First = lists:sublist(Signature, 8),
            Last = lists:sublist(Signature, Len - 7, 8),
            First ++ "..." ++ Last;
        _ -> Signature
    end;
truncate_signature(Signature) when is_binary(Signature) ->
    truncate_signature(binary_to_list(Signature));
truncate_signature(_) -> "invalid".

validate_public_key(PublicKey) when is_list(PublicKey) ->
    length(PublicKey) >= ?PUBLIC_KEY_LENGTH andalso length(PublicKey) =< 50;
validate_public_key(PublicKey) when is_binary(PublicKey) ->
    validate_public_key(binary_to_list(PublicKey));
validate_public_key(_) -> false.

validate_signature(Signature) when is_list(Signature) ->
    length(Signature) >= ?SIGNATURE_LENGTH andalso length(Signature) =< 100;
validate_signature(Signature) when is_binary(Signature) ->
    validate_signature(binary_to_list(Signature));
validate_signature(_) -> false.

format_timestamp({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                  [Year, Month, Day, Hour, Minute, Second]);
format_timestamp(Timestamp) when is_integer(Timestamp) ->
    DateTime = calendar:gregorian_seconds_to_datetime(Timestamp div 1000 + 62167219200),
    format_timestamp(DateTime);
format_timestamp(_) -> "unknown".

calculate_transaction_fee(TransactionSize, PriorityFee) ->
    BaseFee = 5000,
    SizeFee = TransactionSize * 100,
    BaseFee + SizeFee + PriorityFee.

estimate_rent_exemption(DataSize) ->
    BaseRent = 890880,
    PerByteRent = 6960,
    BaseRent + (DataSize * PerByteRent).

format_balance_display(Lamports) ->
    format_balance_display(Lamports, sol).

format_balance_display(Lamports, sol) ->
    Sol = lamports_to_sol(Lamports),
    io_lib:format("~.4f SOL", [Sol]);
format_balance_display(Lamports, lamports) ->
    io_lib:format("~B lamports", [Lamports]);
format_balance_display(Lamports, both) ->
    Sol = lamports_to_sol(Lamports),
    io_lib:format("~.4f SOL (~B lamports)", [Sol, Lamports]).

parse_transaction_type(<<"sol_transfer">>) -> sol_transfer;
parse_transaction_type(<<"token_transfer">>) -> token_transfer;
parse_transaction_type(<<"nft_transfer">>) -> nft_transfer;
parse_transaction_type(<<"stake_created">>) -> stake_created;
parse_transaction_type(<<"stake_deactivated">>) -> stake_deactivated;
parse_transaction_type(<<"stake_withdrawn">>) -> stake_withdrawn;
parse_transaction_type(<<"token_mint">>) -> token_mint;
parse_transaction_type(<<"token_burn">>) -> token_burn;
parse_transaction_type(sol_transfer) -> <<"sol_transfer">>;
parse_transaction_type(token_transfer) -> <<"token_transfer">>;
parse_transaction_type(nft_transfer) -> <<"nft_transfer">>;
parse_transaction_type(stake_created) -> <<"stake_created">>;
parse_transaction_type(stake_deactivated) -> <<"stake_deactivated">>;
parse_transaction_type(stake_withdrawn) -> <<"stake_withdrawn">>;
parse_transaction_type(token_mint) -> <<"token_mint">>;
parse_transaction_type(token_burn) -> <<"token_burn">>;
parse_transaction_type(_) -> unknown.

get_transaction_status_color(confirmed) -> green;
get_transaction_status_color(pending) -> yellow;
get_transaction_status_color(failed) -> red;
get_transaction_status_color(_) -> gray.

format_wallet_summary(Wallet) when is_record(Wallet, solana_wallet) ->
    #{
        wallet_id => Wallet#solana_wallet.wallet_id,
        label => Wallet#solana_wallet.label,
        public_key => Wallet#solana_wallet.public_key,
        public_key_short => format_public_key(Wallet#solana_wallet.public_key),
        is_primary => Wallet#solana_wallet.is_primary,
        created_at => format_timestamp(Wallet#solana_wallet.created_at),
        last_used => format_timestamp(Wallet#solana_wallet.last_used)
    }.

calculate_wallet_value(WalletId) ->
    case solana_walletdb:get_wallet_transactions(WalletId) of
        {ok, Transactions} ->
            TotalReceived = lists:sum([
                Tx#solana_transaction.amount_lamports ||
                Tx <- Transactions,
                Tx#solana_transaction.tx_type =:= sol_transfer,
                Tx#solana_transaction.to_address =/= undefined
            ]),
            TotalSent = lists:sum([
                Tx#solana_transaction.amount_lamports ||
                Tx <- Transactions,
                Tx#solana_transaction.tx_type =:= sol_transfer,
                Tx#solana_transaction.from_address =/= undefined
            ]),
            #{
                total_received_lamports => TotalReceived,
                total_sent_lamports => TotalSent,
                net_lamports => TotalReceived - TotalSent,
                total_received_sol => lamports_to_sol(TotalReceived),
                total_sent_sol => lamports_to_sol(TotalSent),
                net_sol => lamports_to_sol(TotalReceived - TotalSent)
            };
        {error, _} ->
            #{
                total_received_lamports => 0,
                total_sent_lamports => 0,
                net_lamports => 0,
                total_received_sol => 0.0,
                total_sent_sol => 0.0,
                net_sol => 0.0
            }
    end.

group_transactions_by_date(Transactions) ->
    Grouped = lists:foldl(fun(Tx, Acc) ->
        {{Year, Month, Day}, _} = Tx#solana_transaction.created_at,
        DateKey = {Year, Month, Day},
        CurrentList = maps:get(DateKey, Acc, []),
        Acc#{DateKey => [Tx | CurrentList]}
    end, #{}, Transactions),
    maps:map(fun(_, TxList) ->
        lists:sort(fun(A, B) ->
            A#solana_transaction.created_at >= B#solana_transaction.created_at
        end, TxList)
    end, Grouped).

filter_transactions_by_date_range(Transactions, StartDate, EndDate) ->
    lists:filter(fun(Tx) ->
        TxDate = Tx#solana_transaction.created_at,
        TxDate >= StartDate andalso TxDate =< EndDate
    end, Transactions).

get_transaction_statistics(Transactions) ->
    Total = length(Transactions),
    Confirmed = length([Tx || Tx <- Transactions, Tx#solana_transaction.status =:= confirmed]),
    Pending = length([Tx || Tx <- Transactions, Tx#solana_transaction.status =:= pending]),
    Failed = length([Tx || Tx <- Transactions, Tx#solana_transaction.status =:= failed]),

    TotalVolume = lists:sum([Tx#solana_transaction.amount_lamports || Tx <- Transactions]),
    TotalFees = lists:sum([Tx#solana_transaction.fee_lamports || Tx <- Transactions]),

    TypeCounts = lists:foldl(fun(Tx, Acc) ->
        Type = Tx#solana_transaction.tx_type,
        Count = maps:get(Type, Acc, 0),
        Acc#{Type => Count + 1}
    end, #{}, Transactions),

    #{
        total => Total,
        confirmed => Confirmed,
        pending => Pending,
        failed => Failed,
        total_volume_lamports => TotalVolume,
        total_volume_sol => lamports_to_sol(TotalVolume),
        total_fees_lamports => TotalFees,
        total_fees_sol => lamports_to_sol(TotalFees),
        by_type => TypeCounts,
        success_rate => if Total > 0 -> (Confirmed / Total) * 100; true -> 0.0 end
    }.

format_airdrop_progress(Airdrop) when is_record(Airdrop, solana_airdrop) ->
    Total = Airdrop#solana_airdrop.total_recipients,
    Successful = Airdrop#solana_airdrop.successful,
    Failed = Airdrop#solana_airdrop.failed,
    Remaining = Total - Successful - Failed,

    Progress = if Total > 0 -> (Successful / Total) * 100; true -> 0.0 end,

    #{
        airdrop_id => Airdrop#solana_airdrop.airdrop_id,
        type => Airdrop#solana_airdrop.type,
        status => Airdrop#solana_airdrop.status,
        total => Total,
        successful => Successful,
        failed => Failed,
        remaining => Remaining,
        progress_percentage => Progress,
        created_at => format_timestamp(Airdrop#solana_airdrop.created_at)
    }.

calculate_stake_rewards(StakeAmount, EpochsStaked) ->
    AnnualRate = 0.07,
    EpochsPerYear = 365,
    EpochRate = AnnualRate / EpochsPerYear,
    RewardsLamports = round(StakeAmount * EpochRate * EpochsStaked),
    #{
        stake_amount_lamports => StakeAmount,
        stake_amount_sol => lamports_to_sol(StakeAmount),
        epochs_staked => EpochsStaked,
        estimated_rewards_lamports => RewardsLamports,
        estimated_rewards_sol => lamports_to_sol(RewardsLamports),
        annual_rate => AnnualRate
    }.

format_stake_status(created) -> "Created - Not Delegated";
format_stake_status(delegated) -> "Active - Earning Rewards";
format_stake_status(deactivated) -> "Deactivating - Cooldown Period";
format_stake_status(withdrawn) -> "Withdrawn - Inactive";
format_stake_status(_) -> "Unknown Status".

get_validator_performance(ValidatorVoteAddress) ->
    #{
        vote_address => ValidatorVoteAddress,
        vote_address_short => format_public_key(ValidatorVoteAddress),
        performance_data => needs_rpc_fetch,
        recommendation => check_solana_beach_or_validators_app
    }.

is_valid_solana_address(Address) when is_list(Address) ->
    length(Address) >= 32 andalso length(Address) =< 44;
is_valid_solana_address(Address) when is_binary(Address) ->
    is_valid_solana_address(binary_to_list(Address));
is_valid_solana_address(_) -> false.

generate_wallet_qr_data(PublicKey) ->
    base64:encode(list_to_binary(PublicKey)).

parse_wallet_qr_data(QRData) ->
    try
        binary_to_list(base64:decode(QRData))
    catch
        _:_ -> {error, invalid_qr_data}
    end.

encrypt_private_key(PrivateKey, Password) ->
    Salt = crypto:strong_rand_bytes(16),
    Key = crypto:pbkdf2_hmac(sha256, Password, Salt, 100000, 32),
    IV = crypto:strong_rand_bytes(16),
    {Ciphertext, AuthTag} = crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, PrivateKey, <<>>, true),
    #{
        encrypted_private_key => base64:encode(Ciphertext),
        encryption_iv => base64:encode(IV),
        encryption_auth_tag => base64:encode(AuthTag),
        encryption_salt => base64:encode(Salt)
    }.

decrypt_private_key(EncryptedData, Password) ->
    try
        Salt = base64:decode(maps:get(encryption_salt, EncryptedData)),
        Key = crypto:pbkdf2_hmac(sha256, Password, Salt, 100000, 32),
        IV = base64:decode(maps:get(encryption_iv, EncryptedData)),
        Ciphertext = base64:decode(maps:get(encrypted_private_key, EncryptedData)),
        AuthTag = base64:decode(maps:get(encryption_auth_tag, EncryptedData)),

        case crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, Ciphertext, <<>>, AuthTag, false) of
            error -> {error, decryption_failed};
            Plaintext -> {ok, Plaintext}
        end
    catch
        _:_ -> {error, decryption_failed}
    end.

hash_password(Password) ->
    erlpass:hash(Password).

verify_password(Password, Hash) ->
    erlpass:match(Password, Hash).
