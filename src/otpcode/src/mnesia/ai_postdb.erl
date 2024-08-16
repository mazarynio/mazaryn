-module(ai_postdb).
-author("Zaryn Technologies").
-include("../records.hrl"). 
-export([insert/1, get_ai_post_by_ai_id/1, get_ai_post_by_post_id/1]).

insert(PostID) ->
    Fun = fun() ->
        ID = key_guardian:gen_address(80),
        AI_Post = #ai_post{
            id = ID,
            post_id = PostID
        },
        mnesia:write(AI_Post),
        ID
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_ai_post_by_ai_id(ID) ->
    Res = mnesia:transaction(
        fun() ->
            mnesia:match_object(#ai_post{id = ID, _= '_'})
        end),
    case Res of
      {atomic, []} -> ai_post_not_exist;
      {atomic, [Post]} -> Post;
      _ -> error
end.

get_ai_post_by_post_id(ID) ->
    Post = postdb:get_post_by_id(ID),
    AI_Post_ID = Post#post.ai_post_id,
    AI_Post = get_ai_post_by_ai_id(AI_Post_ID),
    AI_Post.