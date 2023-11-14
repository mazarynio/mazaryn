-module(kad_utils).
-author("Zaryn Technologies").
-include("kademlia.hrl").
-export([random_id/0, check_size/1, get_udp_port/0, get_socket/0, get_ip_port/0]).

random_id() ->
    Data = key_guardian:gen_address(100),
    ID = crypto:hash(sha, Data),
    ID.

%
check_size(Data) ->
    Size = byte_size(Data) * 8,
    Size.

%
get_udp_port() ->
    {ok, Socket} = gen_udp:open(0, [{reuseaddr, true}, {ip, {127, 0, 0, 1}}]),
    {ok, {IP, Port}} = inet:sockname(Socket),
    gen_udp:close(Socket),
    {IP, Port}.

get_socket() ->
    {ok, Socket} = gen_udp:open(22346),
    Socket.

get_ip_port() ->
    Socket = get_socket(),
    {IP, Port} = inet:peername(Socket),
    get_ip(IP),
    get_port(Port).

get_ip(IP) ->
    IP.
get_port(Port) ->
    Port.





