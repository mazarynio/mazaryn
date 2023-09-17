-module(manage_post).
-export([get_post/1]).
-include("../records.hrl").

get_post(ID) ->
  Fun = fun() ->
            mnesia:match_object(#comment{post = ID,
                                         _ = '_'}),
            [Post] = mnesia:read({post, ID}),
            Comments = lists:foldl(fun(Id, Acc) ->
                                       [Comment] = mnesia:read({comment, Id}),
                                       [Comment|Acc]
                                   end,[], Post#post.comments),
            Post#post{comments = Comments}
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.


    
