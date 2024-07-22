-module(maintenance).

-export([backup/0, restore/0, check_data/0]).

-define(BACKUP_FILE, "/home/zaryn/mazaryn/file.bup").

backup() ->
    io:format("Starting backup process...~n"),
    ensure_mnesia_started(),
    do_backup().

restore() ->
    io:format("Starting restore process...~n"),
    ensure_mnesia_started(),
    do_restore(),
    restart_mnesia().

ensure_mnesia_started() ->
    case mnesia:system_info(is_running) of
        yes ->
            io:format("Mnesia is already running.~n");
        no ->
            io:format("Starting Mnesia...~n"),
            mnesia:start(),
            io:format("Mnesia started successfully.~n")
    end.

do_backup() ->
    case mnesia:backup(?BACKUP_FILE) of
        ok ->
            io:format("Backup created successfully at ~p~n", [?BACKUP_FILE]);
        {error, Reason} ->
            io:format("Error creating backup: ~p~n", [Reason])
    end.

do_restore() ->
    io:format("Preparing for restore...~n"),
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    io:format("Starting restore...~n"),
    case mnesia:restore(?BACKUP_FILE, [{default_op, recreate_tables}]) of
        {atomic, _} ->
            io:format("Restore completed successfully from: ~s~n", [?BACKUP_FILE]);
        {aborted, Reason} ->
            io:format("Restore failed: ~p~n", [Reason])
    end.

restart_mnesia() ->
    io:format("Restarting Mnesia...~n"),
    mnesia:stop(),
    mnesia:start(),
    io:format("Mnesia restarted successfully.~n").

check_data() ->
    {atomic, Results} = mnesia:transaction(fun() ->
        Tables = mnesia:system_info(tables),
        [{Table, mnesia:table_info(Table, size)} || Table <- Tables, Table /= schema]
    end),
    io:format("Table sizes after restore:~n"),
    [io:format("~p: ~p records~n", [Table, Size]) || {Table, Size} <- Results].