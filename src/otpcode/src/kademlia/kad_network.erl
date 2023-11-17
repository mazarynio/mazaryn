-module(kad_network).
-author("Zaryn Technologies").
-include("kademlia.hrl").
-export([look_up/2, bucket_index/1, put/2, get/1, update_bucket/0, ping_node/0,
 pong_node/0, store/3, find_node/0, find_value/2, notify/0]).


look_up(TargetID, Node) ->
    TargetID, Node.

bucket_index(Distance) ->
    Distance.

put(Key, Value) ->
    Key, Value.

get(Key) ->
    Key.

update_bucket() ->
    'not implemented'.
    
    
% Testing Node Liveness 
ping_node() ->
    'not implemented'.

pong_node() ->
    'not implemented'.

store(NodeID, Key, Value) ->
    NodeID, Key, Value.

find_node() ->
    'not implemeted'.

find_value(NodeID, Key) ->
    NodeID, Key.

% Notify Nodes in the KBucket that a new Node Joined the network
notify() ->
    'not implemented'.