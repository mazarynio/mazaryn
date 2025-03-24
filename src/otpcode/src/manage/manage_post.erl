-module(manage_post).
-author("Zaryn Technologies").
-include("../records.hrl").
-include("admins.hrl").
-export([get_post/1, delete_post/2, delete_hashtag/2, delete_post_content/1]).

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

%% delete_post(PostID, AdminUsername)
delete_post(Id, AdminUsername) ->
  case userdb:get_user_id(AdminUsername) of
      {error, _} = Error ->
          Error;
      {ok, _} ->
          case lists:member(AdminUsername, ?ADMIN_USERNAMES) of
              true ->
                  F = fun() ->
                          mnesia:delete({post, Id})
                      end,
                  mnesia:activity(transaction, F);
              false ->
                  {error, not_admin}
          end
  end.
% Delete Hashtag if it's Restricted 
delete_hashtag(PostID, AdminUsername) ->
  case userdb:get_user_id(AdminUsername) of
      {error, _} = Error ->
          Error;
      {ok, _} ->
          case lists:member(AdminUsername, ?ADMIN_USERNAMES) of
              true ->
                  Post = postdb:get_post_by_id(PostID),
                  BannedList = hashtags:banned_list(),
                  Hashtag = Post#post.hashtag,
                  case lists:member(Hashtag, BannedList) of
                      true ->
                          Fun = fun() ->
                                    NewPost = Post#post{hashtag = undefined},
                                    mnesia:write(NewPost),
                                    io:fwrite("~p~n", [NewPost]),
                                    ok
                                end,
                          {atomic, Res} = mnesia:transaction(Fun),
                          Res;
                      false ->
                          {error, not_banned}
                  end;
              false ->
                  {error, not_admin}
          end
  end.
% Delete Post if the Content is Restricted
delete_post_content(PostID) ->
  Post = postdb:get_post_by_id(PostID),
  BannedList = post_content:banned_list(),
  Content = Post#post.content_cid,
  case lists:member(Content, BannedList) of
    true ->
      F = fun() ->
          mnesia:delete({post, PostID})
      end,
      mnesia:activity(transaction, F);
    false ->
      Content
  end.

  