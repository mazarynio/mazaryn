-module(knode_utils).
-author("Zaryn Technologies").
-export([mynode/1]).

mynode(Username) ->
    Atom = list_to_atom(Username),
    {ok, Pid} = net_kernel:start([Atom, shortnames]),
    Pid,
    node().