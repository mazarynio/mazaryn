-module(post_ipfs_utils).
-author("Zaryn Technologies").
-export([get_ipns/1]).
-include("../records.hrl").

get_ipns(PostID) ->
    Fun = fun() ->
        case mnesia:read({post, PostID}) of
            [Post] ->
                Post#post.content; 
            [] ->
                throw(post_not_found)
        end
    end,
    IPFS = case mnesia:transaction(Fun) of
               {atomic, Hash} -> Hash;
               {aborted, {throw, post_not_found}} -> throw(post_not_found);
               {aborted, Reason} -> throw({transaction_failed, Reason})
           end,
    Parent = self(),
    Ref = make_ref(),
    spawn(fun() ->
        Result = case ipfs_client_5:name_publish(IPFS) of
                     {ok, Map} when is_map(Map), is_map_key(name, Map), is_map_key(value, Map) ->
                         {ok, Map};
                     {ok, _Invalid} ->
                         {error, invalid_ipns_result};
                     {error, _Reason} ->
                         {error, {ipns_publish_failed, _Reason}}
                 end,
        Parent ! {Ref, Result}
    end),
    receive
        {Ref, {ok, Map}} ->
            {ok, Map};
        {Ref, {error, _Reason}} ->
            {error, _Reason}
    after 12000 -> 
        {error, ipns_timeout}
    end.