-module(user_api).
-author("Zaryn Technologies").
-export([start/0, handler/1]).

start() ->
    % Start the inets application
    application:start(inets),

    % Specify the options for the HTTP server
    Options = [
    {port, 8081},  % Use a different port number
    {server_name, "user_api"},
    {server_root, "./www"},
    {document_root, "./www"}
    ],

    % Start the HTTP server
    case inets:start(httpd, Options) of
        {ok, _Pid} ->
            io:format("Server started at http://localhost:8080~n");
        {error, already_started} ->
            io:format("Server is already running.~n");
        {error, Reason} ->
            io:format("Failed to start server: ~p~n", [Reason])
    end.

handler(_Request) ->
    % Simple request handler
    {ok, [
        {"Content-Type", "text/html"}
    ], "<html><body><h1>Hello, Erlang!</h1></body></html>"}.
