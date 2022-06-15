-module(file_store).
-export([insert_file/1]).
-include("../p2p.hrl").

insert_file(Name) ->
    File = #file{name = Name},
    F = fun() ->
        mnesia:write(File)
    end,
    {atomic, _} = mnesia:transaction(F).

