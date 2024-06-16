%%%-------------------------------------------------------------------
%% @doc Maintenance operations for Mnesia backup and restore.
%% @end
%%%-------------------------------------------------------------------

-module(maintenance).

-export([start_maintenance/0, restore_backup/0]).

-define(BACKUP_FILE, "/home/emilano/mazaryn/file.BCK").

start_maintenance() ->
    %% Notify users about the maintenance window
    notify_users(),
    %% Proceed with backup restore
    restore_backup().

notify_users() ->
    io:format("Starting maintenance. Users will experience downtime.~n"),
    ok.

restore_backup() ->
    stop_application_services(),
    case mnesia:stop() of
        stopped ->
            io:format("Mnesia stopped successfully.~n"),
            do_restore();
        {error, not_started} ->
            io:format("Mnesia was not started.~n"),
            do_restore();
        {error, Reason} ->
            io:format("Error stopping Mnesia: ~p~n", [Reason])
    end,
    start_application_services().

do_restore() ->
    case mnesia:restore(?BACKUP_FILE, [node()]) of
        ok ->
            io:format("Mnesia restored from backup successfully.~n"),
            start_mnesia();
        {error, Reason} ->
            io:format("Error restoring Mnesia from backup: ~p~n", [Reason])
    end.

stop_application_services() ->
    io:format("Stopping application services.~n"),
    application:stop(mazaryn),
    ok.

start_mnesia() ->
    case mnesia:start() of
        ok ->
            io:format("Mnesia started successfully.~n");
        {error, Reason} ->
            io:format("Error starting Mnesia: ~p~n", [Reason])
    end.

start_application_services() ->
    io:format("Starting application services.~n"),
    application:start(mazaryn),
    ok.
