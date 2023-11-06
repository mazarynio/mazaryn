-module(post_content).
-export([banned_list/0]).

banned_list() ->
    List = [<<"123ghg">>, <<"234tyt">>],
    List.