-module(kademlia).
-author("Zaryn Technologies").
-include("kademlia.hrl").
-export([insert_node/0]).

insert_node() ->
    Fun = fun() ->
        ID = kad_utils:random_id(),
        {IP, Port} = kad_utils:get_udp_port(),
        KadNode = #k_node{
            id = ID,
            ip = IP,
            port = Port},
        mnesia:write(KadNode),
        ID
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.





    
