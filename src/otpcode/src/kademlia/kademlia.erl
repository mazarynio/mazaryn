-module(kademlia).
-author("Zaryn Technologies").
-include("kademlia.hrl").
-export([insert_node/1, get_node/1, list_nodes/0, get_addr/1, remove_node/1]).

insert_node(UserID) ->
    Fun = fun() ->
        ID = kad_utils:random_id(),
        {IP, Port} = kad_utils:get_udp_port(),
        KadNode = #knode{
            id = ID,
            ip = IP,
            port = Port,
            user = UserID},
        mnesia:write(KadNode),
        ID
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

% Get Kademlia Node By NodeID
get_node(ID) ->
    Res = mnesia:transaction(
          fun() ->
              mnesia:match_object(#knode{id = ID, _= '_'})
          end),
    case Res of
        {atomic, []} -> user_not_exist;
        {atomic, [KNode]} -> KNode;
        _ -> error
    end.

% List of All Nodes Participated on the Network
list_nodes() ->
    Fun = fun() ->
            mnesia:all_keys(knode)
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_addr(NodeID) ->
    KNode = get_node(NodeID),
    IP = KNode#knode.ip,
    Port = KNode#knode.port,
    io:fwrite("~p:~p~n", [IP, Port]).

% Remove the Node from Routing Table
remove_node(NodeID) ->
    NodeID.





    
