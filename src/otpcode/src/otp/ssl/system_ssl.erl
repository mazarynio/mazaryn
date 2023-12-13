-module(system_ssl).
-author("Zaryn Technologies").
-export([start/0, get_peer_name/1]).

start() ->
    ssl:start().

get_peer_name(SslSocket) ->
    ssl:peername(SslSocket).