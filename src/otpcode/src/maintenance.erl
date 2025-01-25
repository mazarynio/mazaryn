-module(maintenance).

-export([backup/1, restore/1, check_data/0]).

-define(ENCRYPTION_ALG, aes_256_gcm).
-define(BACKUP_DIR, "/home/zaryn/mazaryn/backups").

backup(Password) when is_list(Password) ->
    try
        ensure_mnesia_started(),
        ok = filelib:ensure_dir(?BACKUP_DIR ++ "/"),
        BackupFile = get_next_backup_name(),
        do_backup(BackupFile, Password)
    catch
        error:Reason:Stacktrace ->
            error_logger:error_msg("Backup failed: ~p~n~p", [Reason, Stacktrace]),
            {error, Reason}
    end.

get_next_backup_name() ->
    get_next_backup_name(1).

get_next_backup_name(N) ->
    Filename = filename:join(?BACKUP_DIR, "backup_" ++ integer_to_list(N) ++ ".mz"),
    case filelib:is_file(Filename) of
        true -> get_next_backup_name(N + 1);
        false -> Filename
    end.

restore(Args) ->
    try
        #{file := BackupFile, password := Password} = validate_restore_args(Args),
        ensure_mnesia_started(),
        do_restore(BackupFile, Password)
    catch
        error:Reason:Stacktrace ->
            error_logger:error_msg("Restore failed: ~p~n~p", [Reason, Stacktrace]),
            {error, Reason}
    end.

ensure_mnesia_started() ->
    case mnesia:system_info(is_running) of
        yes -> ok;
        no ->
            ok = mnesia:start(),
            wait_for_mnesia()
    end.

wait_for_mnesia() ->
    case mnesia:system_info(is_running) of
        yes -> ok;
        no ->
            timer:sleep(100),
            wait_for_mnesia()
    end.

do_backup(BackupFile, Password) ->
    TempFile = BackupFile ++ ".tmp",
    case mnesia:backup(TempFile) of
        ok ->
            {ok, Data} = file:read_file(TempFile),
            Compressed = zlib:compress(Data),
            IV = crypto:strong_rand_bytes(16),
            Salt = crypto:strong_rand_bytes(16),
            Key = derive_key(Password, Salt),
            {EncryptedData, Tag} = crypto:crypto_one_time_aead(?ENCRYPTION_ALG, Key, IV, Compressed, <<>>, true),
            Backup = term_to_binary({1, Salt, IV, Tag, EncryptedData}),
            case file:write_file(BackupFile, Backup) of
                ok ->
                    file:delete(TempFile),
                    {ok, BackupFile};
                {error, Reason} ->
                    error({write_failed, Reason})
            end;
        {error, Reason} ->
            error({backup_failed, Reason})
    end.

do_restore(BackupFile, Password) ->
    case file:read_file(BackupFile) of
        {ok, EncryptedBackup} ->
            case binary_to_term(EncryptedBackup) of
                {1, Salt, IV, Tag, EncryptedData} ->
                    Key = derive_key(Password, Salt),
                    case crypto:crypto_one_time_aead(?ENCRYPTION_ALG, Key, IV, EncryptedData, <<>>, Tag, false) of
                        {error, _} -> error(decryption_failed);
                        Compressed ->
                            Data = zlib:uncompress(Compressed),
                            TempFile = BackupFile ++ ".tmp",
                            ok = file:write_file(TempFile, Data),
                            restore_from_file(TempFile)
                    end;
                _ ->
                    error(invalid_backup_format)
            end;
        {error, Reason} ->
            error({read_failed, Reason})
    end.

restore_from_file(File) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    ok = mnesia:start(),
    case mnesia:restore(File, [{default_op, recreate_tables}]) of
        {atomic, _} ->
            file:delete(File),
            restart_mnesia(),
            ok;
        {aborted, Reason} ->
            error({restore_failed, Reason})
    end.

restart_mnesia() ->
    mnesia:stop(),
    mnesia:start(),
    wait_for_mnesia().

check_data() ->
    {atomic, Results} = mnesia:transaction(fun() ->
        Tables = mnesia:system_info(tables),
        [{Table, mnesia:table_info(Table, size)} || Table <- Tables, Table /= schema]
    end),
    {ok, Results}.

derive_key(Password, Salt) ->
    crypto:pbkdf2_hmac(sha512, list_to_binary(Password), Salt, 100000, 32).

validate_restore_args(#{file := File, password := Pass}) when is_list(File), is_list(Pass) ->
    case filelib:is_regular(File) of
        true -> #{file => File, password => Pass};
        false -> error(backup_file_not_found)
    end;
validate_restore_args(_) ->
    error(invalid_restore_args).