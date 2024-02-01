-module(post_api).
-author("Zaryn Technologies").
-export([start/0]).

start() ->
    % Start the inets application if it's not already started
    case application:start(inets) of
        ok ->
            ok;
        {error, {already_started, inets}} ->
            ok;
        {error, Reason} ->
            io:format("Failed to start inets: ~p~n", [Reason]),
            halt(1)
    end,

    % Specify the options for the HTTP server
    Options = [
        {port, 50557},
        {server_name, "user_api"},
        {server_root, "../www"},
        {document_root, "../www"},
        {bind_address, {127,0,0,1}},
        {server_modules, [user_api]}
    ],

    % Start the HTTP server
    case inets:start(httpd, Options) of
        {ok, _Pid} ->
            io:format("Server started at http://localhost:50557 ~n");
        {error, already_started} ->
            io:format("Server is already running.~n");
        {error, _Reason} ->
            io:format("Failed to start server: ~p~n", [_Reason])
    end.