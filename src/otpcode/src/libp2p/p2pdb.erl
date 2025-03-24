-module(p2pdb).
-author("Zaryn Technologies").
-export([insert/1]).
-include("../records.hrl"). 

insert(NodeID) ->
    case go_libp2p:create_node(NodeID) of
        {ok, _Network} ->
            {ok, Address} = go_libp2p:get_single_address(NodeID),
            {ok, PeerID} = go_libp2p:get_peerid(NodeID),
            Now = calendar:universal_time(),
            Fun = fun() ->
                P2PNode = #p2p_node{
                    node_id = NodeID,
                    address = Address,
                    peer_id = PeerID,
                    date_created = Now 
                },
                mnesia:write(P2PNode),
                binary_to_list(Address)
            end,
            {atomic, Res} = mnesia:transaction(Fun),
            Res;
        {error, _Error} ->
            "fallback_address_" ++ NodeID
    end.