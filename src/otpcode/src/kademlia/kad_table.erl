-module(kad_table).
-author("Zaryn Technologies").
-include("kademlia.hrl").
-export([distance/2, closest_node/1]).

% Calculate the XOR distance between two Kademlia node IDs
distance(ID1, ID2) ->
    List1 = binary_to_list(ID1),
    List2 = binary_to_list(ID2),
    Int1 = list_to_integers(List1),
    Int2 = list_to_integers(List2),
    Result = Int1 bxor Int2,
    Result.

list_to_integers(List) ->
    Result = lists:foldl(fun(Digit, Acc) -> Acc * 10 + Digit end, 0, List),
    Result.

% Find closest Node to TargetID
closest_node(TargetID) ->
    NodeList = kademlia:list_nodes(),
    closest_node(TargetID, NodeList).

closest_node(TargetID, NodeList) ->
    TargetID, NodeList.